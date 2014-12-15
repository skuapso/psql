%%%-------------------------------------------------------------------
%%% @author il
%%% @copyright (C) 2012, il
%%% @doc
%%%
%%% @end
%%% Created : 2012-02-28 17:27:55.374769
%%%-------------------------------------------------------------------
-module(psql_pool).

-behaviour(gen_server).

-compile({no_auto_import, [link/1, unlink/1]}).
%% API
-export([
  start_link/3,
  start_link/4,
  request/2,
  request/3,
  request/4
  ]).

%% gen_event callbacks
-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    terminate/2,
    code_change/3
  ]).

% ets:fun2ms(fun(_) -> true end)
-define(CountSpec, [{'_', [], [true]}]).

-record(state, {options, max_connections, queue_size, queries_ets, workers_ets, backends_ets}).

-include_lib("logger/include/log.hrl").
%%%===================================================================
%%% API
%%%===================================================================
request(Query, Timeout) ->
  request(unknown, Query, Timeout).

request(Pid, Query, Timeout) when is_pid(Pid) ->
  request(Pid, unknown, Query, Timeout);

request(Priority, Query, Timeout) ->
  request(?MODULE, Priority, Query, Timeout).

request(Pid, Priority, Query, Timeout) when Priority > -10000 ->
  gen_server:call(Pid, {request, Priority, Query}, Timeout).

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {'_err'or, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Opts, MaxConnections, QueueSize) ->
  gen_server:start_link(?MODULE, {Opts, MaxConnections, QueueSize}, []).
start_link(Name, Opts, MaxConnections, QueueSize) ->
  gen_server:start_link({local, Name}, ?MODULE, {Opts, MaxConnections, QueueSize}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init({Opts, MaxConnections, QueueSize}) ->
  '_trace'("init"),
  process_flag(trap_exit, true),
  WEts = ets:new(workers, [ordered_set, protected]),
  QEts = ets:new(queries, [ordered_set, protected]),
  BEts = ets:new(backends, [ordered_set, protected]),
  {ok, #state{
      options=Opts,
      max_connections=MaxConnections,
      queue_size=QueueSize,
      queries_ets = QEts,
      workers_ets = WEts,
      backends_ets = BEts}}.

handle_cast(new_request, #state{queries_ets = QEts} = State) ->
  case ets:match(QEts, {{'$1', '$2', '$3'}, '$4'}, 1) of
    {[[Priority, Tag, From, Query]], _} ->
      psql({{Priority, Tag, From}, Query}, State);
    '$end_of_table' ->
      '_trace'("query not found");
    Else ->
      '_emerg'("unknown answer while searching ~w", [Else])
  end,
  {noreply, State};
handle_cast(Msg, State) ->
  '_notice'("unhandled cast ~w when ~w", [Msg, State]),
  {noreply, State}.

handle_call({request, Priority, Query}, From, #state{queries_ets = QEts} = State) ->
  '_trace'("new request ~w with priority ~w from ~w", [Query, Priority, From]),
  link(From),
  ets:insert(QEts, {{Priority, now(), From}, Query}),
  '_debug'("ets is ~w", [ets:match(QEts, '$1')]),
  check_queries(State),
  new_request(),
  {noreply, State};
handle_call(Msg, From, State) ->
  '_notice'("unhanded call ~w from ~w when ~w", [Msg, From, State]),
  {noreply, State}.

handle_info({psql_worker, Pid, Answer}, #state{workers_ets = WEts} = State) ->
  '_debug'("answer ~w from ~w", [Answer, Pid]),
  case ets:match(WEts, {Pid, {'_', '_', '$1'}, '_'}) of
    [[From]] ->
      gen_server:reply(From, Answer),
      unlink(From);
    _ ->
      if
        Answer =:= {error, timeout} -> ok;
        true -> '_emerg'("can't find from for ~w", [Pid])
      end
  end,
  ets:insert(WEts, {Pid, ready}),
  new_request(),
  {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
  '_trace'("~w finished", [Pid]),
  case whois(Pid, State) of
    worker ->
      clean_worker(Pid, Reason, State);
    client ->
      clean_client(Pid, State);
    _ ->
      '_warning'("unknown process died ~w", [Pid])
  end,
  {noreply, State};
handle_info(Info, State) ->
  '_warning'("unhandled '_info' msg ~w", [Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _S) ->
  '_debug'("terminating because ~w", [Reason]),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, S, _E) ->
  {ok, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_queries(#state{queries_ets = QEts} = State) ->
  check_queries(State, ets:select_count(QEts, ?CountSpec)).
check_queries(#state{queue_size = QSize}, CurSize) when QSize >= CurSize -> ok;
check_queries(#state{queries_ets = QEts} = State, _) ->
  {Priority, _Tag, _From} = ets:last(QEts),
  delete_first_query(Priority, State).

delete_first_query(Priority, _State) when Priority < 0 ->
  '_emerg'("queue is full of system messages");
delete_first_query(P, #state{queries_ets = QEts}) ->
  {[[Tag, From, GenTag]], _} = ets:match(QEts, {{P, '$1', {'$2', '$3'}}, '_'}, 1),
  ets:delete(QEts, {P, Tag, {From, GenTag}}),
  unlink(From),
  From ! {'DOWN', GenTag, process, ?MODULE, timeout}.

clean_worker(Pid, normal, #state{workers_ets = WEts, backends_ets = BEts}) ->
  '_trace'("worker ~w terminated", [Pid]),
  case ets:match(WEts, {Pid, '$1'}) of
    [[ready]] -> ok;
    Else -> '_warning'("died while ~w", [Else])
  end,
  ets:delete(WEts, Pid),
  ets:delete(BEts, Pid),
  ok;
clean_worker(Pid, Reason, State) ->
  '_warning'("worker ~w terminated with reason ~w", [Pid, Reason]),
  clean_worker(Pid, normal, State).

clean_client(Pid, #state{queries_ets = QEts} = State) ->
  case ets:match(QEts, {{'$1', '$2', {Pid, '$3'}}, '_'}) of
    [] ->
      cancel_backend(Pid, State);
    L when is_list(L) ->
      lists:map(fun([Priority, Tag, Tag1]) ->
            ets:delete(QEts, {Priority, Tag, {Pid, Tag1}})
        end, L);
    Else ->
      '_emerg'("unknown answer while search not started queries ~w", [Else])
  end.

cancel_backend(Pid, #state{workers_ets = WEts} = State) ->
  case ets:match(WEts, {'$1', {'$2', '_', {Pid, '_'}}, '$3'}) of
    [] ->
      '_warning'("no backends to cancel, pid is ~w", [Pid]),
      '_debug'("workers ets ~w", [ets:match(WEts, '$1')]),
      '_debug'("queries ets ~w", [ets:match(State#state.queries_ets, '$1')]),
      '_debug'("backends ets ~w", [ets:match(State#state.backends_ets, '$1')]);
    L when is_list(L) ->
      lists:map(fun([Wid, Priority, Query]) ->
            '_warning'("canceling worker ~w query is ~w, priority ~w", [Wid, Query, Priority]),
            case ets:match(State#state.backends_ets, {Wid, '$1'}) of
              [[Bid]] ->
                ets:insert(WEts, {Wid, canceling}),
                gen_fsm:send_event(Bid, timeout);
              _ ->
                '_warning'("no backend id for ~w", [Wid])
            end
        end, L);
    Else ->
      '_warning'("unknown answer while search ~w", [Else])
  end.

whois(Pid, State) ->
  case ets:match(State#state.backends_ets, {Pid, '$1'}, 1) of
    {[[_]], _} -> worker;
    '$end_of_table' -> client
  end.

new_request() ->
  gen_server:cast(self(), new_request).

psql(Query, #state{backends_ets = BEts} = State) ->
  case get_ready(State) of
    {ok, Pid} ->
      request_worker(Pid, Query, State);
    {ok, Pid, BackendPid} ->
      ets:insert(BEts, {Pid, BackendPid}),
      request_worker(Pid, Query, State);
    _ ->
      ok
  end.

request_worker(Pid, {Id, {Request, Data}}, #state{queries_ets = QEts, workers_ets = WEts}) ->
  ets:insert(WEts, {Pid, Id, Data}),
  ets:delete(QEts, Id),
  psql_worker:Request(Pid, Data).

get_ready(#state{workers_ets = WEts} = State) ->
  '_trace'("searching for ready"),
  case ets:match(WEts, {'$1', ready}, 1) of
    {[[Pid]], _} ->
      '_trace'("found ~w", [Pid]),
      {ok, Pid};
    '$end_of_table' ->
      '_trace'("not found, starting new"),
      new(ets:select_count(WEts, ?CountSpec), State);
    Else ->
      '_emerg'("unknown answer while searching ~w", [Else]),
      Else
  end.

new(CurNo, #state{max_connections = MaxNo, options = Opts}) when
    (MaxNo =:= infinity) or
    (MaxNo =:= -1) or
    (CurNo < MaxNo) ->
  '_trace'("starting new worker"),
  if (CurNo + 2) =:= MaxNo -> '_warning'("max connections reached"); true -> ok end,
  psql_worker:start_link(Opts);
new(_, _) ->
  {error, max_connection_reached}.

link({Pid, _}) when is_pid(Pid) -> link(Pid);
link(Pid) when is_pid(Pid) -> erlang:link(Pid).

unlink({Pid, _}) when is_pid(Pid) -> unlink(Pid);
unlink(Pid) when is_pid(Pid) -> erlang:unlink(Pid).
