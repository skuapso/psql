-module(psql_listener).

-behaviour(gen_fsm).

%% API
-export([
  start_link/1
  ]).

%% states callbacks
-export([
  disconnected/2,
  ready/2
  ]).

%% gen_fsm callbacks
-export([
  init/1,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4
  ]).

-record(state, {connection}).

-include_lib("logger/include/log.hrl").
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Opts) ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Host, Port, User, Passwd, DB, SSL, SSLOpts, Timeout, Commands]) ->
  trace("init"),
  gen_fsm:send_event(self(), {connect, Host, Port, User, Passwd, DB, SSL, SSLOpts, Timeout, Commands}),
  {ok, disconnected, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
disconnected({connect, Host, Port, User, Passwd, DB, SSL, SSLOpts, undefined, Commands}, S) ->
  disconnected({connect, Host, Port, User, Passwd, DB, SSL, SSLOpts, infinity, Commands}, S);
disconnected({connect, Host, Port, User, Passwd, DB, SSL, SSLOpts, Timeout, _Commands}, S) ->
  trace("connecting"),
  Opts = [{port, Port}, {database, DB},
          {ssl, SSL}, {ssl_opts, SSLOpts},
          {timeout, Timeout}, {async, self()}],
  debug("connecting to ~s:~w, user ~s, opts ~w", [Host, Port, User, Opts]),
  {ok, C} = pgsql:connect(Host, User, Passwd, Opts),
  pgsql:squery(C, "listen system"),
  pgsql:squery(C, "listen ui"),
  trace("connected"),
  {next_state, ready, S#state{connection = C}}.

ready(Msg, S) ->
  warning("ready: unhandled msg ~w", [Msg]),
  {next_state, ready, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(Event, StateName, State) ->
  warning("unhandled all state event ~w when ~s", [Event, StateName]),
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(Event, From, StateName, State) ->
  warning("unhandled all state request ~w when ~s from ~w", [Event, StateName, From]),
  Reply = ok,
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({pgsql, _Pid, {notification, <<"ui">>, _PgPid, Payment}},
            StateName, #state{connection = C} = State) ->
  {Type, Table, Schema, Id} = parse(Payment),
  Query = <<"select row_to_json(O.*) as json from"
              "(select *,$1::varchar as type from ",
                Table/binary, ".", Schema/binary,
              " where id=$2) as O"
          >>,
  [{Data}] = case pgsql:equery(C, Query, [Type, Id]) of
           {ok, _, Row} ->
             debug("ui notification ~w", [Row]),
             Row;
           Else ->
             alert("unknown answer ~w", [Else]),
             []
         end,
  hooks:run(ui, [{Type, Id}, Data]),
  {next_state, StateName, State};
handle_info(stop, _StateName, State) ->
  {stop, normal, State};
handle_info(Info, StateName, State) ->
  warning("unhandled info msg ~w when ~s", [Info, StateName]),
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse(Data) ->
  [Type, Table, Schema | IdBin] = re:split(Data, " "),
  Id = parse_id(IdBin),
  {erlang:binary_to_atom(Type, latin1), Table, Schema, Id}.

parse_id([IdBin]) ->
  binary_to_integer(IdBin).
