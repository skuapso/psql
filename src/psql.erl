-module(psql).

-behaviour(application).
-behaviour(supervisor).

-export([json_enc/1]).
-export([pre_json/1]).
%% hooks
-export([connection_accepted/4]).
-export([connection_closed/3]).
-export([terminal_uin/3]).
-export([terminal_info/4]).
-export([terminal_raw_data/4]).
-export([terminal_answer/5]).
-export([terminal_packet/4]).
-export([get/5]).
-export([set/5]).
-export([get_opts/1]).

%% API
-export([
  start/0,
  start_link/0,
  execute/4,
  execute/3,
  execute/2
  ]).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("logger/include/log.hrl").
%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
  application:start(?MODULE).

start(_StartType, _StartArgs) ->
  start_link().

stop(_State) ->
  ok.

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

connection_accepted(Pid, Proto, Socket, Timeout) when is_port(Socket) ->
  trace("new connection accepted"),
  {ok, {RemoteIP, RemotePort}} = inet:peername(Socket),
  {ok, {LocalIP, LocalPort}} = inet:sockname(Socket),
  ConnectionId = now2id(),
  [[{open, ConnectionID}]] = execute(10000, function, {connection, open,
                               [ConnectionId, Proto, LocalIP, LocalPort, RemoteIP, RemotePort]},
              Timeout),
  hooks:set(Pid, connection_id, ConnectionID),
  debug("~w connection id is ~w", [Pid, ConnectionID]),
  ok.

connection_closed(Pid, normal, Timeout) ->
  case hooks:get(Pid, connection_id) of
    undefined ->
      alert("connection closed, but no connection ID specified for ~w", [Pid]),
      ok;
    ConnectionId ->
      debug("closing connection ~w", [ConnectionId]),
      case execute(-10, function, {connection, close, [ConnectionId]}, Timeout) of
        [[{close, ConnectionId}]] -> trace("connection closed"), ok;
        [[{close, null}]] -> alert("no connection in DB ~w", [ConnectionId]), ok;
        [] -> alert("no connection in DB ~w", [ConnectionId]), ok
      end
  end;
connection_closed(Pid, {broken, Data}, Timeout) ->
  ConnectionId = hooks:get(Pid, connection_id),
  Id = now2id(),
  execute(-10, function, {data, add_broken, [Id, Data, ConnectionId]}, Timeout),
  connection_closed(Pid, normal, Timeout);
connection_closed(Pid, {incomplete, Data}, Timeout) ->
  connection_closed(Pid, {broken, Data}, Timeout);
connection_closed(Pid, {function_clause, [{_Module, parse, [Data], _FileInfo} | _ ]}, Timeout) ->
  connection_closed(Pid, {broken, Data}, Timeout);
connection_closed(Pid, Reason, Timeout) ->
  warning("closed connection with unknown reason ~w", [Reason]),
  connection_closed(Pid, normal, Timeout).

terminal_uin(_Pid, Terminal, Timeout) ->
  TerminalId = get_terminal_id(Terminal, Timeout),
  ConnectionId = hooks:get(connection_id),
  debug("setting terminal ~w on connection ~w", [TerminalId, ConnectionId]),
  hooks:set(terminal_id, TerminalId),
  execute(10, function, {connection, set_terminal, [ConnectionId, TerminalId]}, Timeout),
  ok.

terminal_info(_Pid, _Terminal, M, _Timeout) when map_size(M) =:= 0 ->
  ok;
terminal_info(_Pid, Terminal, Info, Timeout) ->
  debug("setting terminal ~w info ~w", [Terminal, Info]),
  TerminalId = get_terminal_id(Terminal, Timeout),
  execute(5, function, {terminal, set_info, [TerminalId, json_enc(Info)]}, Timeout),
  ok.

terminal_raw_data(_Pid, _Terminal, RawData, Timeout) ->
  trace("terminal raw data"),
  ConnectionID = hooks:get(connection_id),
  RawID = now2id(),
  case execute(1000, function, {data, add_raw, [RawID, RawData, ConnectionID]}, Timeout) of
    [] ->
      debug("data repeat"),
      hooks:delete(raw_id),
      stop;
    [[{add_raw, null}]] ->
      debug("data repeat"),
      hooks:delete(raw_id),
      stop;
    [[{add_raw, RawID}]] ->
      debug("raw id is ~w", [RawID]),
      hooks:set(raw_id, RawID),
      ok
  end.

terminal_packet(_Pid, _Terminal, #{type := Type,
                                   raw := RawPacket
                                  } = Packet, Timeout) ->
  trace("terminal packet"),
  Eventtime = case maps:get(eventtime, Packet, undefined) of
                undefined -> erlang:universaltime();
                ET -> ET
              end,
  PacketJSON = json_enc(maps:without([type, raw, eventtime], Packet)),
  case hooks:get(raw_id) of
    undefined -> stop;
    RawId ->
      case execute(1000,
                   function,
                   {data, add_packet,
                    [now2id(), RawPacket, RawId, Type, Eventtime, PacketJSON]},
                   Timeout) of
        [] ->
          hooks:delete(packet_id),
          stop;
        [[{add_packet, null}]] ->
          hooks:delete(packet_id),
          stop;
        [[{add_packet, PacketId}]] ->
          hooks:set(packet_id, PacketId),
          ok
      end
  end.

terminal_answer(_Pid, _Terminal, Module, Answer, Timeout) ->
  trace("terminal answer"),
  case hooks:get(raw_id) of
    undefined ->
      stop;
    RawDataID ->
      execute(600, function, {data, set_answer, [RawDataID, Answer, Module]}, Timeout),
      ok
  end.

get(_Pid, terminal, id, Terminal, Timeout) ->
  TID = get_terminal_id(Terminal, Timeout),
  hooks:set(terminal_id, TID),
  {ok, {?MODULE, TID}};
get(_Pid, replica, servers, {_Module, _UIN}, Timeout) ->
  trace("getting servers"),
  TerminalID = hooks:get(terminal_id),
  ConnectionID = hooks:get(connection_id),
  ServersPrepare = execute(100, function,
                           {replica, get_servers, [TerminalID, ConnectionID]},
                           Timeout),
  Servers = lists:map(
      fun(X) ->
          ServerId = proplists:get_value(server_id, X),
          ServerProto = binary_to_atom(proplists:get_value(server_protocol, X), latin1),
          [ServerId, ServerProto]
      end, ServersPrepare),
  debug("servers: ~w", [Servers]),
  {ok, {?MODULE, Servers}};
get(_Pid, replica, server_info, ServerId, Timeout) ->
  trace("getting server info"),
  {ok, {?MODULE, execute(30000, function, {replica, server_info, [ServerId]}, Timeout)}};
get(_Pid, replica, data, {ServerID, Terminal, Points}, Timeout) ->
  TerminalID = get_terminal_id(Terminal, Timeout),
  Reply = lists:map(
      fun(X) ->
          ID = proplists:get_value(id, X),
          Data = proplists:get_value(data, X),
          Proto = binary_to_atom(proplists:get_value(protocol, X), latin1),
          [{id, ID}, {data, Data}, {protocol, Proto}]
      end,
      execute(15000, function,
              {replica, undelivered,
                       [ServerID, TerminalID, Points]},
              Timeout)),
  {ok, {?MODULE, Reply}};
get(_Pid, replica, undelivered, [], Timeout) ->
  Data = execute(25000, function, {replica, undelivered, []}, Timeout),
  {ok, {?MODULE, Data}};
get(_Pid, replica, undelivered, ServerID, Timeout) ->
  Data = execute(20000, function, {replica, undelivered, [ServerID]}, Timeout),
  {ok, {?MODULE, Data}};
get(_Pid, m2m, track_info, {Type, EventTime, Lat, Lon, Used, Speed, Course}, Timeout) ->
  trace("searching for m2m track info"),
  TerminalID = hooks:get(terminal_id),
  Params = [Type, TerminalID, EventTime, Lat, Lon, Used, Speed, Course],
  [Data] = execute(500, function, {replica, m2m, Params}, Timeout),
  Track = proplists:get_value(track, Data, 0),
  Action = proplists:get_value(action, Data, 0),
  Reboot = proplists:get_value(reboot, Data, 0),
  Reply = {Action, Track, Reboot},
  debug("reply is ~w", [Reply]),
  {ok, {?MODULE, Reply}};
get(_Pid, terminal, command, Terminal, Timeout) ->
  TerminalId = get_terminal_id(Terminal, Timeout),
  case execute(10, function,
                      {terminal, command, [TerminalId]},
                      Timeout) of
    [] -> ok;
    [D] ->
      {ok, {?MODULE, {
        proplists:get_value(command_id, D),
        proplists:get_value(command, D),
        binary_to_atom(proplists:get_value(send_type, D), latin1)
      }}}
  end;
get(_Pid, _From, _What, _Filter, _Timeout) ->
  ok.

set(_Pid, replica, data, {_ServerID, _ServerProto, <<>>}, _Timeout) ->
  ok;
set(_Pid, replica, data, {ServerID, ServerProto, Data}, Timeout) ->
  PacketID = hooks:get(packet_id),
  TerminalID = hooks:get(terminal_id),
  execute(700, function,
          {replica, add_data,
           [now2id(), PacketID, ServerID, ServerProto, Data, TerminalID]},
          Timeout),
  ok;
set(_Pid, replica, answer, {DataIDs, Answer}, Timeout) ->
  ConnectionID = hooks:get(connection_id),
  [[{set_answer, AnswerID}]] = execute(1000, function,
                                       {replica, set_answer,
                                        [now2id(), Answer, ConnectionID, DataIDs]},
                                       Timeout),
  hooks:set(answer_id, AnswerID),
  ok;
set(_Pid, replica, add_answer, Answer, Timeout) ->
  AnswerId = hooks:get(answer_id),
  execute(-2, function, {replica, expand_answer, [AnswerId, Answer]}, Timeout),
  ok;
set(_Pid, terminal, command, {Terminal, Command}, Timeout) ->
  execute(50000, function,
          {terminal, add_command, [get_terminal_id(Terminal, Timeout), Command]}, Timeout),
  ok;
set(_Pid, terminal, command_exec, {Terminal, CommandId}, Timeout) ->
  execute(10, function,
          {terminal, command_executed, [get_terminal_id(Terminal, Timeout), CommandId]}, Timeout),
  ok;
set(_Pid, _From, _What, _Data, _Timeout) ->
  ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init(Opts) ->
  MaxConnections = misc:get_env(?MODULE, max_connections, Opts),
  QueueSize = misc:get_env(?MODULE, queue_size, Opts, MaxConnections),
  HooksWeight = misc:get_env(?MODULE, weight, Opts),
  hooks:install(connection_accepted, HooksWeight, {?MODULE, connection_accepted}),
  hooks:install(connection_closed, HooksWeight, {?MODULE, connection_closed}),
  hooks:install(terminal_uin, HooksWeight, {?MODULE, terminal_uin}),
  hooks:install(terminal_info, HooksWeight, {?MODULE, terminal_info}),
  hooks:install(terminal_raw_data, HooksWeight, {?MODULE, terminal_raw_data}),
  hooks:install(terminal_answer, HooksWeight, {?MODULE, terminal_answer}),
  hooks:install(terminal_packet, HooksWeight, {?MODULE, terminal_packet}),
  hooks:install(get, HooksWeight, {?MODULE, get}),
  hooks:install({?MODULE, get}, HooksWeight, {?MODULE, get}),
  hooks:install({?MODULE, set}, HooksWeight, {?MODULE, set}),
  Args = get_opts(Opts),
  debug("connection options: ~w", [Args]),
  {
    ok,
    {
      {one_for_one, 5, 10},
      [
        {
          psql_listener,
          {psql_listener, start_link, [get_opts(Args)]},
          permanent,
          5000,
          worker,
          []
        },
        {
          psql_pool,
          {psql_pool, start_link, [psql_pool, get_opts(Args), MaxConnections, QueueSize]},
          permanent,
          5000,
          supervisor,
          []
        }
      ]
    }
  }.

get_opts(Opts) ->
  Host    = misc:get_env(?MODULE, host, Opts),
  Port    = misc:get_env(?MODULE, port, Opts),
  User    = misc:get_env(?MODULE, user, Opts),
  Passwd  = misc:get_env(?MODULE, password, Opts),
  DB      = misc:get_env(?MODULE, database, Opts),
  SSL     = misc:get_env(?MODULE, ssl, Opts),
  SSLOpts = misc:get_env(?MODULE, ssl_opts, Opts),
  Timeout = misc:get_env(?MODULE, timeout, Opts),
  Commands = misc:get_env(?MODULE, pre_commands, Opts),
  [Host, Port, User, Passwd, DB, SSL, SSLOpts, Timeout, Commands].

execute(Request, Data) ->
  execute(unknown, Request, Data).

execute(Request, Data, Timeout) when is_integer(Timeout); (Timeout =:= infinity) ->
  execute(unknown, Request, Data, Timeout);
execute(Priority, Request, Data) when is_tuple(Data) ->
  execute(Priority, Request, Data, 5000).

execute(Priority, Request, Data, Timeout) ->
  psql_pool:request(psql_pool, Priority, {Request, Data}, Timeout).

get_terminal_id({Module, UIN}, Timeout) ->
  case hooks:get(terminal_id) of
    undefined ->
      [[{get, TID}]] = execute(10, function, {terminal, get, [UIN, Module]}, Timeout),
      TID;
    TID -> TID
  end.

now2id() ->
  Now = {_, _, MicroSec} = erlang:now(),
  {Date, {H, M, S}} = calendar:now_to_universal_time(Now),
  {Date, {H, M, S + MicroSec/1000000}}.

json_enc(L) ->
  L1 = pre_json(L),
  case catch jsxn:encode(L1) of
    {'EXIT', {badarg, _}} -> warning("can't transform to json: ~w", [L1]), <<"{}">>;
    {'EXIT', Reason} -> warning("jsx failed transform ~w: ~w", [L1, Reason]), <<"{}">>;
    E -> E
  end.

pre_json(Map) ->
  maps:map(
    fun(_, V) when is_map(V) -> pre_json(V);
       (LL, {G, M}) when LL =:= latitude; LL =:= longitude ->
        #{d => G, m => M};
       (_, V) -> V
    end,
    Map).
