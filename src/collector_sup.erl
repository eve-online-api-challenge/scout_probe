-module(collector_sup).

-compile(export_all).

-behaviour(supervisor).

-define(UPDATE_TIMER, 30000).
-define(WORKERS_AMOUNT, 2).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
%	ets:new(trackers,[named_table,public,set,{write_concurrency,true},{read_concurrency,true}]),
  {ok, { {one_for_one, 5, 10}, [
  {0, {km_collector, master, []}, permanent, brutal_kill, worker, []}
  ]++[{Id, {km_collector, worker, []}, permanent, brutal_kill, worker, []}|| Id <- lists:seq(1,?WORKERS_AMOUNT)]} }.
