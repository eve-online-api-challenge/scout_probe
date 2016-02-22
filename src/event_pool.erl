%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Event Pool Olso mamging hash table for trackers
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

-module(event_pool).

-include("config.hrl").
-behaviour(gen_server).

-record(state,{}).
-export([add/1, state/0, get/1]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
 terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================
%%add event to pool
add(#event{}=Event)->
  gen_server:cast(?MODULE, {add,Event}).

%%get event state (ets:info)
state()->
  gen_server:call(?MODULE, state).

%%get event by ID
get(ID)->
  gen_server:call(?MODULE,{get,ID}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%% {ok, State, Timeout} |
%% ignore |
%% {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  ets:new(event,[named_table,public,set, {keypos,2}]),
  ets:new(hash,[named_table,public,set,{read_concurrency,true}]),
  timer:send_interval(?EVENT_TTL*1000, ?MODULE, clear),
  {ok, #state{}}.



handle_call({get,ID}, _From, State) ->
  Event = ets:lookup(event, ID),
  {reply, Event, State};
handle_call(state, _From, State) ->
  {reply, ets:info(event), State}.
handle_cast({add,#event{}=Event}, State) ->
  ets:insert_new(event, Event),
  {noreply, State};
handle_cast(stop, State)->
  {stop, normal, State};

handle_cast(_Msg, State) ->
 {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------


handle_info(clear, State)->
  MS = [{#event{_ = '_',expire = '$2'},
    [{'>',{'-',erlang:monotonic_time(seconds),'$2'},?EVENT_TTL}],
    [true]}], % match for N sec old records
  MS2 = [{{'_','_','$2'},
        [{'>',{'-',erlang:monotonic_time(seconds),'$2'},?HASH_TTL}],
        [true]}],
  ets:select_delete(hash,MS2),
  N = ets:select_delete(event,MS),
  if
		N>0 ->
			%%io:format("~p| Deleted ~p expired events~n", [time(), N]);
      ok;
		true->
			ok
	end,

  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
 ok.

code_change(_OldVsn, State, _Extra) ->
 {ok, State}.
