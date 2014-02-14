-module(psql).

-behaviour(application).
-behaviour(supervisor).

%% hooks
-export([
  connection_accepted/4,
  connection_closed/3,
  terminal_uin/4,
  terminal_raw_data/5,
  terminal_answer/5,
  terminal_packet/7,
  get/5,
  set/5
  ]).

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
  Query = "
  with data_id as (
    insert into data.connections (id, protocol, type)
      values ($1, $2, 'ip')
      returning id
  ) insert into connections.ip_data
      select D.id, $3, $4, $5, $6
        from data_id D returning id",
  ConnectionId = now2id(),
  S = execute(1000, execute, {Query, [ConnectionId, Proto, LocalIP, LocalPort, RemoteIP, RemotePort]}, Timeout),
  debug ("returned ~w", [S]),
  {ok, ConnectionID} = S,
  hooks:set(Pid, connection_id, ConnectionID),
  debug("~w connection id is ~w", [Pid, ConnectionID]),
  ok.

connection_closed(Pid, {incomplete, Data} ,Timeout) ->
  ConnectionID = hooks:get(Pid, connection_id),
  {ok, _BrokenDataID} = execute(-10, insert, {data, broken, [{connection_id, ConnectionID}, {data, Data}]}, Timeout),
  connection_closed(Pid, normal, Timeout);
connection_closed(Pid, {function_clause, [{_Module, parse, [Data], _FileInfo} | _ ]}, Timeout) ->
  ConnectionID = hooks:get(Pid, connection_id),
  {ok, _BrokenDataID} = execute(-10, insert, {data, broken, [{connection_id, ConnectionID}, {data, Data}]}, Timeout),
  connection_closed(Pid, normal, Timeout);
connection_closed(Pid, _Reason, Timeout) ->
  case hooks:get(Pid, connection_id) of
    undefined ->
      alert("connection closed, but no connection ID specified for ~w", [Pid]),
      ok;
    ConnectionID ->
      debug("closing connection ~w", [ConnectionID]),
      Query = "update data.connections set ended=now() where id=$1 returning id",
      {ok, ConnectionID} = execute(-10, execute, {Query, [ConnectionID]}, Timeout),
      trace("connection closed"),
      ok
  end.

terminal_uin(_Pid, _, undefined, _Timeout) ->
  ok;
terminal_uin(_Pid, Module, UIN, Timeout) ->
  TerminalID = get_terminal_id({Module, UIN}, Timeout),
  ConnectionID = hooks:get(connection_id),
  hooks:set(terminal_id, TerminalID),
  Query = "update data.connections set terminal_id=$1 where id=$2",
  execute(1000, execute, {Query, [TerminalID, ConnectionID]}, Timeout),
  ok.

terminal_raw_data(_Pid, _Module, _UIN, RawData, Timeout) ->
  trace("terminal raw data"),
  ConnectionID = hooks:get(connection_id),
  RawID = now2id(),
  case execute(1000, insert, {data, raws, [{id, RawID}, {connection_id, ConnectionID}, {data,  RawData}]}, Timeout) of
    {ok, 0} ->
      debug("data repeat"),
      hooks:delete(raw_id),
      stop;
    {ok, RawID} ->
      debug("raw id is ~w", [RawID]),
      hooks:set(raw_id, RawID),
      ok
  end.

terminal_packet(_Pid, Module, UIN, Type, RawPacket, Packet, Timeout) ->
  trace("terminal packet"),
  {ok, PacketID} = insert_terminal_raw_packet(hooks:get(raw_id), RawPacket, Type, Timeout),
  {TableName, Data} = case proplists:get_value(navigation, Packet, []) of
    []  -> {active, proplists:get_value(active, Packet, [])};
    D   -> {navigation, D}
  end,
  {ok, DataID} = insert_terminal_packet(PacketID, Module, Type, TableName, Data, Timeout),
  debug("packet id is ~w", [DataID]),
  {ok, DataID} = insert_terminal_packet(DataID, Module, Type,
                                        proplists:delete(TableName, Packet), Timeout),
  if
    PacketID =:= 0 -> info("data repeat for {~w, ~w}", [Module, UIN]), stop;
    true -> hooks:set(packet_id, PacketID), hooks:set(data_id, DataID), ok
  end.

terminal_answer(_Pid, Module, _UIN, Answer, Timeout) ->
  trace("terminal answer"),
  case hooks:get(raw_id) of
    undefined ->
      trace("now raw id"),
      stop;
    RawDataID ->
      {ok, _RawDataID} = execute(1000, insert, {data, raws, [{id, RawDataID}, {answer, Answer}, {answered, Module}]}, Timeout),
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
  ServersPrepare = execute(1000, function, {replica, servers, [TerminalID, ConnectionID]}, Timeout),
  Servers = lists:map(
      fun(X) ->
          ServerId = proplists:get_value(server_id, X),
          ServerProto = binary_to_atom(proplists:get_value(server_protocol, X), latin1),
          [ServerId, ServerProto]
      end, ServersPrepare),
  debug("servers: ~w", [Servers]),
  {ok, {?MODULE, Servers}};
get(_Pid, replica, servers, Filter, Timeout) ->
  trace("getting server info"),
  {ok, {?MODULE, execute(1000, select, {replica, servers, Filter}, Timeout)}};
get(_Pid, replica, data, {ServerID, Terminal, Points}, Timeout) ->
  TerminalID = get_terminal_id(Terminal, Timeout),
  Reply = lists:map(
      fun(X) ->
          ID = proplists:get_value(id, X),
          Data = proplists:get_value(data, X),
          Proto = binary_to_atom(proplists:get_value(protocol, X), latin1),
          [{id, ID}, {data, Data}, {protocol, Proto}]
      end,
      execute(1000, select, {replica, data,
                       [{server_id, ServerID},{terminal_id, TerminalID}, {answer_id, null}],
                       [{order, "id"}, {limit, Points}]},
              Timeout)),
  {ok, {?MODULE, Reply}};
get(_Pid, replica, undelivered, [], Timeout) ->
  Data = execute(1000, function, {replica, data, []}, Timeout),
  {ok, {?MODULE, Data}};
get(_Pid, replica, undelivered, ServerID, Timeout) ->
  Data = execute(1000, function, {replica, data, [ServerID]}, Timeout),
  {ok, {?MODULE, Data}};
get(_Pid, m2m, track_info, {Type, EventTime, Lat, Lon, Used, Speed, Course}, Timeout) ->
  trace("searching for m2m track info"),
  TerminalID = hooks:get(terminal_id),
  Params = [Type, TerminalID, EventTime, Lat, Lon, Used, Speed, Course],
  [Data] = execute(1000, function, {replica, m2m, Params}, Timeout),
  Track = proplists:get_value(track, Data, 0),
  Action = proplists:get_value(action, Data, 0),
  Reboot = proplists:get_value(reboot, Data, 0),
  Reply = {Action, Track, Reboot},
  debug("reply is ~w", [Reply]),
  {ok, {?MODULE, Reply}};
get(_Pid, terminal, command, Terminal, Timeout) ->
  case execute(1000, function,
                      {terminal, command, [get_terminal_id(Terminal, Timeout)]},
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
  execute(1000, insert, {replica, data, [
        {id, now2id()},
        {parent_id, PacketID},
        {server_id, ServerID},
        {protocol, ServerProto},
        {data, Data},
        {terminal_id, TerminalID}
        ]}, Timeout),
  ok;
set(_Pid, replica, answer, {DataIDs, Answer}, Timeout) ->
  ConnectionID = hooks:get(connection_id),
  {ok, AnswerID} = execute(1000, insert, {replica, answers, [
        {id, now2id()},
        {data, Answer},
        {connection_id, ConnectionID}
        ]}, Timeout),
  lists:map(fun(X) ->
        Query = "update replica.data set answer_id=$1 where id=$2 returning id",
        execute(1000, execute, {Query, [AnswerID, X]}, Timeout)
    end, DataIDs),
  hooks:set(answer_id, AnswerID),
  ok;
set(_Pid, replica, add_answer, Answer, Timeout) ->
  AnswerID = hooks:get(answer_id),
  Query = "update replica.answers set data=data || $1 where id=$2",
  execute(1000, execute, {Query, [Answer, AnswerID]}, Timeout),
  ok;
set(_Pid, terminal, command, {Terminal, Command}, Timeout) ->
  execute(1000, insert, {terminals, commands, [
        {terminal_id, get_terminal_id(Terminal, Timeout)},
        {command, Command}]}),
  ok;
set(_Pid, terminal, command_exec, {_Terminal, Command}, Timeout) ->
  Query = "update terminals.commands set executed=now() where id=$1",
  execute(1000, execute, {Query, [Command]}, Timeout),
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
  hooks:install(connection_accepted, HooksWeight, fun ?MODULE:connection_accepted/4),
  hooks:install(connection_closed, HooksWeight, fun ?MODULE:connection_closed/3),
  hooks:install(terminal_uin, HooksWeight, fun ?MODULE:terminal_uin/4),
  hooks:install(terminal_raw_data, HooksWeight, fun ?MODULE:terminal_raw_data/5),
  hooks:install(terminal_answer, HooksWeight, fun ?MODULE:terminal_answer/5),
  hooks:install(terminal_packet, HooksWeight, fun ?MODULE:terminal_packet/7),
  hooks:install(get, HooksWeight, fun ?MODULE:get/5),
  hooks:install({?MODULE, get}, HooksWeight, fun ?MODULE:get/5),
  hooks:install({?MODULE, set}, HooksWeight, fun ?MODULE:set/5),
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
          {psql_pool, start_link, [get_opts(Args), MaxConnections, QueueSize]},
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
  [Host, Port, User, Passwd, DB, SSL, SSLOpts, Timeout].

execute(Request, Data) ->
  execute(unknown, Request, Data).

execute(Request, Data, Timeout) when is_integer(Timeout); (Timeout =:= infinity) ->
  execute(unknown, Request, Data, Timeout);
execute(Priority, Request, Data) when is_tuple(Data) ->
  execute(Priority, Request, Data, 5000).

execute(Priority, Request, Data, Timeout) ->
  psql_pool:request(Priority, {Request, Data}, Timeout).

get_terminal_id({Module, UIN}, Timeout) ->
  case hooks:get(terminal_id) of
    undefined ->
      [[{get, TID}]] = execute(1000, function, {terminal, get, [UIN, Module]}, Timeout),
      TID;
    TID -> TID
  end.

insert_terminal_raw_packet(RawID, _RawPacket, _Type, _Timeout)
    when RawID =:= 0; RawID =:= undefined; RawID =:= null
    ->
  {ok, 0};
insert_terminal_raw_packet(RawID, RawPacket, Type, Timeout) ->
  execute(1000, insert, {data, packets, [{id, now2id()}, {raw_id, RawID}, {data, RawPacket}, {type, Type}]}, Timeout).


insert_terminal_packet(PacketID, _Module, Type, _Table, _Packet, _Timeout)
    when PacketID =:= 0; PacketID =:= undefined; PacketID =:= null;
        Type =:= broken; Type =:= unknown
    ->
  {ok, 0};
insert_terminal_packet(PacketID, Module, _Type, Table, Packet, Timeout) ->
  execute(1000, insert, {Module, Table, [{id, PacketID} | Packet]}, Timeout).

insert_terminal_packet(PacketID, _Module, Type, _Packet, _Timeout)
    when PacketID =:= 0; PacketID =:= undefined; PacketID =:= null;
        Type =:= broken; Type =:= unknown
    ->
  {ok, 0};
insert_terminal_packet(PacketID, _Module, _Type, [], _Timeout) ->
  {ok, PacketID};
insert_terminal_packet(PacketID, Module, Type, [Packet | Else], Timeout) ->
  insert_terminal_packet_data(PacketID, Module, Packet, Timeout),
  insert_terminal_packet(PacketID, Module, Type, Else, Timeout).

insert_terminal_packet_data(PacketID, Module, {PacketType, []}, _Timeout) ->
  warning("blank data for ~w ~w", [Module, PacketType]),
  {ok, PacketID};
insert_terminal_packet_data(PacketID, Module, {set, Data}, Timeout) ->
  insert_terminal_packet_sets_data(PacketID, Module, Data, Timeout);
insert_terminal_packet_data(PacketID, Module, {PacketType, Data}, Timeout) ->
  execute(1000, insert, {Module, PacketType, [{id, PacketID} | Data]}, Timeout).

insert_terminal_packet_sets_data(PacketID, _Module, [], _Timeout) ->
  {ok, PacketID};
insert_terminal_packet_sets_data(PacketID, Module, [{SetName, Data} | Else], Timeout) ->
  insert_terminal_packet_set_data(PacketID, Module, SetName, Data, Timeout),
  insert_terminal_packet_sets_data(PacketID, Module, Else, Timeout).

insert_terminal_packet_set_data(PacketID, _Module, _SetName, [], _Timeout) ->
  {ok, PacketID};
insert_terminal_packet_set_data(PacketID, Module, SetName, [{_, Value} | Else], Timeout)
    when Value =:= null; Value =:= undefined
    ->
  insert_terminal_packet_set_data(PacketID, Module, SetName, Else, Timeout);
insert_terminal_packet_set_data(PacketID, Module, SetName, [{N, Value} | Else], Timeout) ->
  execute(1000, insert, {Module, SetName, [{id, PacketID}, {sensor, N}, {value, Value}]}, Timeout),
  insert_terminal_packet_set_data(PacketID, Module, SetName, Else, Timeout).

now2id() ->
  Now = {_, _, MicroSec} = erlang:now(),
  {Date, {H, M, S}} = calendar:now_to_universal_time(Now),
  {Date, {H, M, S + MicroSec/1000000}}.
