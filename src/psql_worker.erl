-module(psql_worker).

-behaviour(gen_fsm).

%% API
-export([
  start_link/1,
  start_link/2,
  start_link/9,
  insert/2,
  select/2,
  update/2,
  function/2,
  execute/2
  ]).

%% gen_fsm callbacks
-export([
  disconnected/2,
  ready/2,
  ready/3
  ]).
-export([
  init/1,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4
  ]).

-define(WAIT_TIMEOUT, 30000).

-record(state, {
    socket,
    timeout
    }).

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
%% @spec start_link() -> {ok, Pid} | ignore | {'_err'or, Error}
%% @end
%%--------------------------------------------------------------------
insert(Pid, {Module, Type, Data}) ->
  insert(Pid, Module, Type, Data).

select(Pid, {Schema, View, Filter}) ->
  select(Pid, {Schema, View, Filter, []});
select(Pid, {Schema, View, Filter, Opts}) ->
  select(Pid, Schema, View, Filter, Opts).

update(Pid, {Schema, Table, {Update, Condition}}) ->
  update(Pid, Schema, Table, Update, Condition).

function(Pid, {Schema, FunName, Params}) ->
  function(Pid, Schema, FunName, Params).

execute(Pid, {Query, Values}) ->
  execute(Pid, Query, Values).

start_link(Pid, Timeout) ->
  gen_fsm:start_link(?MODULE, [Pid, Timeout], [{timeout, Timeout}]).

start_link([Host, Port, User, Passwd, DB, SSL, SSLOpts, Timeout, Commands]) ->
  start_link(Host, Port, User, Passwd, DB, SSL, SSLOpts, Timeout, Commands).

start_link(Host, Port, User, Passwd, DB, SSL, SSLOpts, undefined, Commands) ->
  start_link(Host, Port, User, Passwd, DB, SSL, SSLOpts, ?WAIT_TIMEOUT, Commands);
start_link(Host, Port, User, Passwd, DB, SSL, SSLOpts, Timeout, Commands) ->
  '_trace'("starting"),
  case gen_fsm:start_link(?MODULE,
                          [Host, Port, User, Passwd, DB, SSL, SSLOpts, Timeout, Commands],
                          [{timeout, Timeout}]) of
    {ok, Pid} ->
      gen_fsm:send_event(Pid, {backend, self()}),
      {ok, Pid, backend};
    Else -> Else
  end.

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
  '_trace'("initialization"),
  gen_fsm:send_event(self(),
                     {connect, Host, Port, User, Passwd, DB, SSL, SSLOpts, Timeout, Commands}),
  {ok, disconnected, #state{}, Timeout};
init([Pid, Timeout]) ->
  {ok, ready, #state{socket = Pid, timeout = Timeout}, Timeout}.

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
disconnected({connect, Host, Port, User, Passwd, DB, SSL, SSLOpts, Timeout, Commands}, _S) ->
  '_trace'("connecting"),
  Opts = [{port, Port}, {database, DB}, {ssl, SSL}, {ssl_opts, SSLOpts}, {timeout, infinity}],
  '_debug'("connecting to ~s:~w, user ~s, opts ~w", [Host, Port, User, Opts]),
  {ok, C} = pgsql:connect(Host, User, Passwd, Opts),
  Commands = misc:get_env(psql, pre_commands, []),
  '_debug'("pre commands is ~w", [Commands]),
  lists:map(fun(X) ->
        '_debug'("running command ~s", [X]),
        A = pgsql:squery(C, X),
        '_debug'("answer is ~w", [A])
    end, Commands),
  '_trace'("connected"),
  {next_state, ready, #state{socket=C, timeout=Timeout}, Timeout}.

ready({Pid, Query, Values}, State) ->
  {reply, Reply, NextState, NewState, Timeout} = ready({Query, Values}, {self(), now()}, State),
  Pid ! {?MODULE, self(), Reply},
  {next_state, NextState, NewState, Timeout};
ready({backend, From}, #state{socket = Socket} = State) ->
  From ! {?MODULE, backend, self(), Socket},
  {next_state, ready, State, State#state.timeout};
ready(timeout, S) ->
  {stop, normal, S}.

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
ready({Query, Values}, _From, S) ->
  '_trace'("quering"),
  '_debug'("query ~w", [Query]),
  '_debug'("values ~w", [Values]),
  Reply = normalize(pgsql:equery(S#state.socket, Query, Values)),
  '_trace'("query finished"),
  '_debug'("reply is ~w", [Reply]),
  {reply, Reply, ready, S, S#state.timeout}.

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
handle_event(_Event, StateName, State) ->
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
handle_sync_event(_Event, _From, StateName, State) ->
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
handle_info(_Info, StateName, State) ->
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
terminate(normal, ready, #state{socket = C} = _S) ->
  '_trace'("terminating"),
  close(C),
  ok;
terminate(Reason, StateName, #state{socket = C} = _S) ->
  '_warning'("terminating when ~w, reason ~w", [StateName, Reason]),
  close(C),
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
normalize({ok, 1, _Cols, [{DataId}]}) ->
  {ok, DataId};
normalize({ok, Cols, Rows}) ->
  normalize_rows(Cols, Rows);
normalize(Else) ->
  Else.

normalize_rows(Columns, Rows) ->
  normalize_rows(Columns, Rows, []).
normalize_rows(_Columns, [], Result) ->
  lists:reverse(Result);
normalize_rows(Columns, [Row | T], Result) ->
  normalize_rows(Columns, T, [normalize_row(Columns, tuple_to_list(Row)) | Result]).

normalize_row(Columns, Row) ->
  normalize_row(Columns, Row, []).
normalize_row([], [], Result) ->
  Result;
normalize_row([{column, BinName, _Type, _ByteSize, _, _} | Columns], [Value | Row], Result) ->
  normalize_row(Columns, Row, [{list_to_atom(binary_to_list(BinName)), Value} | Result]).

insert(Pid, Schema, Table, Data) ->
  {ok, Columns, Params, Values} = prepare_insert_data (Data),
  Relation = relation({Schema, Table}),
  Query = iolist_to_binary(["insert into ", Relation, " (", Columns, ") values (", Params, ") returning id"]),
  execute(Pid, Query, Values).

select(Pid, Schema, Table, Filters, Opts) ->
  {ok, Conditions, Values} = prepare_select_data(Filters),
  Options = case proplists:get_value(order, Opts) of
    undefined -> "";
    OrderCol -> " order by " ++ OrderCol
  end ++ case proplists:get_value(limit, Opts) of
    undefined -> "";
    LimitNum -> " limit " ++ integer_to_list(LimitNum)
  end,
  Relation = relation({Schema, Table}),
  MQuery = iolist_to_binary(["select * from ", Relation, Conditions, Options]),
  Query = case proplists:get_value(json, Opts) of
            true -> <<"select row_to_json(S.*) as json from (", MQuery/binary, ") as S">>;
            undefined -> MQuery
          end,
  execute(Pid, Query, Values).

update(Pid, Schema, Table, SetList, ConditionList) ->
  {ok, SetCols, SetVals} = prepare_update_data(SetList),
  {ok, Conditions, CondVals} = prepare_select_data(ConditionList, length(SetVals) + 1),
  Params = SetVals ++ CondVals,
  Relation = relation({Schema, Table}),
  Query = iolist_to_binary(["update ", Relation, " set ", SetCols, Conditions, " returning id"]),
  execute(Pid, Query, Params).

function(Pid, Schema, FunName, Params) ->
  {ok, BoundList} = prepare_function_data(Params),
  Relation = relation({Schema, FunName}),
  Query = iolist_to_binary(["select * from ", Relation, "(", BoundList, ")"]),
  execute(Pid, Query, Params).

execute(Pid, Query, Values) ->
  '_trace'("calling gen fsm to execute query"),
  gen_fsm:send_event(Pid, {self(), Query, Values}).

prepare_update_data(Data) ->
  prepare_update_data(Data, [], [], 1).

prepare_update_data([{Key, Val} | Tail], Columns, Vals, N) ->
  '_debug'("adding ~w", [{Key, Val}]),
  prepare_update_data(
    Tail,
    Columns ++ "," ++ atom_to_list(Key) ++ "=$" ++ integer_to_list(N),
    [Val | Vals],
    N + 1);
prepare_update_data([], [$, | Columns], Vals, _N) ->
  {ok, Columns, lists:reverse(Vals)}.

prepare_insert_data([]) ->
  {ok, [], [], []};
prepare_insert_data(Data) ->
  '_debug'("preparing data ~w", [Data]),
  prepare_insert_data(Data, [], [], [], 1).

prepare_insert_data([], [$, | Columns], [$, | Params], Values, _N) ->
  {ok, Columns, Params, lists:reverse(Values)};
prepare_insert_data([{Key, Value} | T], Col, P, Vals, N) ->
  Columns = Col ++ "," ++ atom_to_list(Key),
  Params = P ++ ",$" ++ integer_to_list(N),
  Values = [Value | Vals],
  prepare_insert_data(T, Columns, Params, Values, N + 1).

prepare_select_data(Data) ->
  prepare_select_data(Data, 1).

prepare_select_data([], _N) ->
  {ok, [], []};
prepare_select_data(Data, N) ->
  prepare_select_data(Data, [], [], N).

prepare_select_data([], [32 | [$a | [$n | [$d | Condition]]]], Values, _N) ->
  {ok, " where" ++ Condition, lists:reverse(Values)};
prepare_select_data([{Key, null} | T], Con, Vals, N) ->
  Condition = Con ++ " and " ++ atom_to_list(Key) ++ " is null",
  prepare_select_data(T, Condition, Vals, N);
prepare_select_data([{Key, Value} | T], Con, Vals, N) ->
  Condition = Con ++ " and " ++ atom_to_list(Key) ++ "=$" ++ integer_to_list(N),
  Values = [Value | Vals],
  prepare_select_data(T, Condition, Values, N + 1).

prepare_function_data([]) ->
  {ok, []};
prepare_function_data(Params) ->
  prepare_function_data(Params, [], 1).

prepare_function_data([], [$, | PList], _N) ->
  {ok, PList};
prepare_function_data([_Param | Params], PList, N) ->
  prepare_function_data(Params, PList ++ ",$" ++ integer_to_list(N), N + 1).

close(undefined) -> ok;
close(Pid) when is_pid(Pid) -> pgsql:close(Pid).

relation(Rel) when is_binary(Rel) -> Rel;
relation({Schema, Name}) when is_binary(Schema), is_binary(Name) ->
  relation(<<Schema/binary, ".", Name/binary>>);
relation({Schema, Name}) when is_atom(Schema) ->
  relation({atom_to_binary(Schema, latin1), Name});
relation({Schema, Name}) when is_atom(Name) ->
  relation({Schema, atom_to_binary(Name, latin1)});
relation({Schema, Name}) when is_list(Schema) ->
  relation({list_to_binary(Schema), Name});
relation({Schema, Name}) when is_list(Name) ->
  relation({Schema, list_to_binary(Name)}).
