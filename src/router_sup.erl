%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Starting all routers and keep dthem clean
%%% @end
%%% Created : 20 Feb 2016 by Redpate
%%% Copyright © 2016 Redpate
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%-------------------------------------------------------------------
-module(router_sup).

-compile(export_all).

-behaviour(supervisor).
-include("config.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {PropList}=pub_crest:req("/constellations/"),
  ets:new(s2r,[named_table,public,set,{write_concurrency,true},{read_concurrency,true}]), % solar system to region
  IdList=[proplists:get_value(<<"id_str">>, Proplist2)||{Proplist2}<-lists:filter(
    fun({_Proplist2})->
      _ID = proplists:get_value(<<"id">>,_Proplist2,0),
      (_ID < 21000001) and (not lists:member(_ID,
        lists:seq(20000213, 20000224)++ % A821-A
        lists:seq(20000047, 20000062)++
        lists:seq(20000237, 20000243)
      ))
    end, proplists:get_value(<<"items">>, PropList, [{[]}]))],
  timer:apply_interval(?CLEANING_TIMER,?MODULE, clear, []),
  timer:apply_after(2,?MODULE, init_map, [IdList]),
  {ok, { {simple_one_for_one, 5, 10}, [{router, {router, start_link, []}, permanent, brutal_kill, worker, []}]} }.

init_map(IdList)->
  lists:foreach(fun(ID)-> supervisor:start_child(router_sup, [ID])  end, IdList).

clear()->
  lists:foreach(fun({_,PID,_,_})->gen_server:cast(PID,clear),  erlang:garbage_collect(PID) end, supervisor:which_children(?MODULE)).
