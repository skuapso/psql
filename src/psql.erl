-module(psql).

-behaviour(application).
-behaviour(supervisor).

%% hooks
-export([connected/5]).
-export([disconnected/3]).
-export([info/5]).
-export([raw_data/5]).
-export([answer/6]).
-export([packet/5]).
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

connected(What, Pid, {Proto, UIN}, Socket, Timeout) when is_port(Socket) ->
  '_trace'("new connection accepted"),
  {ok, {RemoteIP, RemotePort}} = inet:peername(Socket),
  {ok, {LocalIP, LocalPort}} = inet:sockname(Socket),
  ConnectionId = now2id(),
  Prio = case What of
           terminal -> 100;
           replica -> 10000
         end,
  [[{terminal_id, TerminalId},
    {connection_id, ConnectionID}]] = execute(Prio, function, {connection, open,
                            [ConnectionId, Proto, UIN, LocalIP, LocalPort, RemoteIP, RemotePort]},
              Timeout),
  hooks:set(Pid, connection_id, ConnectionID),
  hooks:set(Pid, terminal_id, TerminalId),
  '_debug'("~w connection id is ~w", [Pid, ConnectionID]),
  ok.

disconnected(Pid, normal, Timeout) ->
  case hooks:get(Pid, connection_id) of
    undefined ->
      '_alert'("connection closed, but no connection ID specified for ~w", [Pid]),
      ok;
    ConnectionId ->
      '_debug'("closing connection ~w", [ConnectionId]),
      case execute(-10, function, {connection, close, [ConnectionId]}, Timeout) of
        [[{close, ConnectionId}]] -> '_trace'("connection closed"), ok;
        [[{close, null}]] -> '_alert'("no connection in DB ~w", [ConnectionId]), ok;
        [] -> '_alert'("no connection in DB ~w", [ConnectionId]), ok
      end
  end;
disconnected(Pid, {broken, Data}, Timeout) ->
  ConnectionId = hooks:get(Pid, connection_id),
  Id = now2id(),
  execute(-10, function, {data, add_broken, [Id, Data, ConnectionId]}, Timeout),
  disconnected(Pid, normal, Timeout);
disconnected(Pid, {incomplete, Data}, Timeout) ->
  disconnected(Pid, {broken, Data}, Timeout);
disconnected(Pid, Reason, Timeout) ->
  '_warning'("closed connection with unknown reason ~w", [Reason]),
  disconnected(Pid, normal, Timeout).

info(terminal, _Pid, _Terminal, M, _Timeout) when map_size(M) =:= 0 ->
  ok;
info(terminal, _Pid, Terminal, Info, Timeout) ->
  TerminalId = get_terminal_id(Terminal, Timeout),
  '_debug'("setting terminal ~w '_info' ~w", [{TerminalId, Terminal}, Info]),
  execute(5, function, {terminal, set_info, [TerminalId, misc:to_json(Info)]}, Timeout),
  ok.

raw_data(terminal, _Pid, _Terminal, RawData, Timeout) ->
  '_trace'("terminal raw data"),
  ConnectionID = hooks:get(connection_id),
  RawID = now2id(),
  case execute(1000, function, {data, add_raw, [RawID, RawData, ConnectionID]}, Timeout) of
    [] ->
      '_debug'("data repeat"),
      hooks:delete(raw_id),
      stop;
    [[{add_raw, null}]] ->
      '_debug'("data repeat"),
      hooks:delete(raw_id),
      stop;
    [[{add_raw, RawID}]] ->
      '_debug'("raw id is ~w", [RawID]),
      hooks:set(raw_id, RawID),
      ok
  end.

packet(terminal, _Pid, _Terminal, #{type := Type,
                                   raw := RawPacket
                                  } = Packet, Timeout) ->
  '_trace'("terminal packet"),
  Eventtime = case maps:get(eventtime, Packet, undefined) of
                undefined -> erlang:universaltime();
                ET -> ET
              end,
  PacketJSON = misc:to_json(maps:without([type, raw, eventtime], Packet)),
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

answer(terminal, _Pid, _Terminal, Module, Answer, Timeout) ->
  '_trace'("terminal answer"),
  case hooks:get(raw_id) of
    undefined ->
      stop;
    RawDataID ->
      execute(600, function, {data, set_answer, [RawDataID, Answer, Module]}, Timeout),
      ok
  end.

get(_Pid, terminal, id, Terminal, Timeout) ->
  TID = get_terminal_id(Terminal, Timeout),
  {ok, {?MODULE, TID}};
get(_Pid, replica, servers, Terminal, Timeout) ->
  '_trace'("getting servers"),
  TerminalID = get_terminal_id(Terminal, Timeout),
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
  '_debug'("servers: ~w", [Servers]),
  {ok, {?MODULE, Servers}};
get(_Pid, replica, server_info, ServerId, Timeout) ->
  '_trace'("getting server '_info'"),
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
get(_Pid, m2m, track_info, {Type, EventTime, Coords, Used, Speed, Course}, Timeout) ->
  '_trace'("searching for m2m track '_info'"),
  TerminalID = hooks:get(terminal_id),
  Params = [Type, TerminalID, EventTime, Coords, Used, Speed, Course],
  [Data] = execute(500, function, {replica, m2m, Params}, Timeout),
  Track = proplists:get_value(track, Data, 0),
  Action = proplists:get_value(action, Data, 0),
  Reboot = proplists:get_value(reboot, Data, 0),
  Reply = {Action, Track, Reboot},
  '_debug'("reply is ~w", [Reply]),
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
  hooks:install({terminal, connected}, HooksWeight, {?MODULE, connected}, [terminal]),
  hooks:install({terminal, disconnected}, HooksWeight, {?MODULE, disconnected}),
  hooks:install({terminal, info}, HooksWeight, {?MODULE, info}, [terminal]),
  hooks:install({terminal, raw_data}, HooksWeight, {?MODULE, raw_data}, [terminal]),
  hooks:install({terminal, answer}, HooksWeight, {?MODULE, answer}, [terminal]),
  hooks:install({terminal, packet}, HooksWeight, {?MODULE, packet}, [terminal]),
  hooks:install(get, HooksWeight, {?MODULE, get}),
  hooks:install({?MODULE, get}, HooksWeight, {?MODULE, get}),
  hooks:install({?MODULE, set}, HooksWeight, {?MODULE, set}),
  hooks:install({replica, connected}, HooksWeight, {?MODULE, connected}, [replica]),
  hooks:install({replica, disconnected}, HooksWeight, {?MODULE, disconnected}),
  Args = get_opts(Opts),
  '_debug'("connection options: ~w", [Args]),
  {
    ok,
    {
      {one_for_one, 5, 1},
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
      hooks:set(terminal_id, TID),
      TID;
    TID -> TID
  end.

now2id() ->
  Now = {_, _, MicroSec} = erlang:now(),
  {Date, {H, M, S}} = calendar:now_to_universal_time(Now),
  {Date, {H, M, S + MicroSec/1000000}}.
