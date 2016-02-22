%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Router unit. Ussed to rote all messages recived to trackers asignet to it.
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
-module(router).

-include("config.hrl").

-export([start_link/1, apply/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
 terminate/2, code_change/3]).

-behaviour(gen_server).
-record(router_state,{id, connected=[], nodes=[], sov=[], messages=[], last_check=?SEC}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(ID) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ID) ->
  gen_server:start_link(?MODULE, [ID], []).

 %%--------------------------------------------------------------------
 %% @doc
 %% Apply method to router, based on links (by SolarSystemID or ConstellationID)
 %%
 %% @spec apply(call|cast, ID=binary(), Message=temr()) -> Result=list()
 %% @end
 %%--------------------------------------------------------------------
apply(Method,ID,Msg)->
  Res0=ets:lookup(s2r,ID),
  case Res0 of
    []->
      [];
    [{_,PID}]->
      erlang:apply(gen_server, Method, [PID,Msg])
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% routerapply(Method,ID,Msg)-
%% @spec init([ID]) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([ID]) ->
  [_,{_,IDs}]=sqlite3:sql_exec(?DB_NAME, "select toConstellationID from mapConstellationJumps where fromConstellationID=?;",[ID]), % todo: union (req?)
  [_,{_,SolarIDs}]=sqlite3:sql_exec(?DB_NAME, "select solarSystemID from mapSolarSystems where constellationID=?;",[ID]), %% public crest have limit 150 req per second. beter to use static export.
  Pid = self(), % save pid. else call self fun in each foreach iteration.
  ets:insert(s2r,{ID,self()}), % create ConstellationID -> Router link
  lists:foreach(fun({SolarID})-> ets:insert(s2r,{list_to_binary(integer_to_list(SolarID)),Pid}) end, SolarIDs), % create SystemID -> Router link. Thanks CCP for ID range.
  {ok, #router_state{id= ID,connected=[list_to_binary(integer_to_list(_ID))||{_ID}<-IDs]}}.


handle_call(events, _From, State) ->
  {reply, State#router_state.messages, State};
handle_call(sov, _From, State) ->
  {reply, State#router_state.sov, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({msg,EventID,0,_From}, State) -> % there only 2 types of msg. noneed to use record to tuplelist transfom
  Duplicated = proplists:get_value(EventID,State#router_state.messages),
  case Duplicated of
    undefined->
      lists:foreach(fun({User,_})->tracker:send(User,{event,EventID}) end, State#router_state.nodes),
      {noreply, State#router_state{ messages=State#router_state.messages++[{EventID,?SEC}]}};
    _->
      {noreply, State#router_state{ messages=proplists:delete(EventID,State#router_state.messages)++[{EventID,?SEC}]}}
  end;
handle_cast({sov,EventID,0,_From}, State) -> % mostly same as previous function. got Message with TTl = 0, send to your nodes and dont broadcast to other routers
    Duplicated = proplists:get_value(EventID,State#router_state.sov),
    case Duplicated of
      undefined->
        lists:foreach(fun({User,_})->tracker:send(User,{sov,EventID}) end, State#router_state.nodes),
        {noreply, State#router_state{ sov=State#router_state.sov++[{EventID,?SEC}]}};
      _->
        {noreply, State#router_state{ sov=proplists:delete(EventID,State#router_state.sov)++[{EventID,?SEC}]}}
    end;

handle_cast({msg,EventID,TTL,From}, State) -> % handle foor msg with no-zero ttl.
  Duplicated = proplists:get_value(EventID,State#router_state.messages),
  case Duplicated of
    undefined->
      NewTTL= TTL-1,
      NextStep = lists:filter(fun(X)-> X=/=From end, State#router_state.connected), %% dont send msg to router you recived it from
   	  lists:foreach(fun(ID)->
      router:apply(cast,ID, {msg,EventID,NewTTL,State#router_state.id}) end,
        NextStep
      ),
      lists:foreach(fun({User,_})-> tracker:send(User,{event,EventID}) end, State#router_state.nodes),
      {noreply, State#router_state{ messages=State#router_state.messages++[{EventID,?SEC}]}};
    Duplicated->
      {noreply, State#router_state{ messages=proplists:delete(EventID,State#router_state.messages)++[{EventID,?SEC}]}}
  end;
handle_cast({sov,EventID,TTL,From}, State) -> %% save as fun before but for sov
    Duplicated = proplists:get_value(EventID,State#router_state.sov),
    case Duplicated of
      undefined->
        NewTTL= TTL-1,
        NextStep = lists:filter(fun(X)-> X=/=From end, State#router_state.connected),

     	  lists:foreach(fun(ID)->
        router:apply(cast,ID, {sov,EventID,NewTTL,State#router_state.id}) end,
          NextStep
        ),
        {noreply, State#router_state{ sov=State#router_state.sov++[{EventID,?SEC}]}};
      _->
        {noreply, State#router_state{ sov=proplists:delete(EventID,State#router_state.sov)++[{EventID,?SEC}]}}
    end;


handle_cast(stop, State) ->
 {stop, normal, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling clear cast, cleaning old messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(clear, State) ->
  M = ?SEC,
  if
  	abs(M-State#router_state.last_check ) < ?MSG_RECORDS_TTL->
  		{noreply, State};
  	true->
  		{noreply, State#router_state{ sov= lists:filter(fun({_ID,_M})-> M-_M< ?SOV_RECORDS_TTL end,State#router_state.sov), messages= lists:filter(fun({_ID,_M})-> M-_M< ?MSG_RECORDS_TTL end,State#router_state.messages), nodes=lists:filter(fun({_ID,_M})-> M-_M< ?MAX_DIF end,State#router_state.nodes),last_check=M }}
  end;
handle_cast({update,ID},State)->
  %%io:format("~p found in ~p~n",[ID, State#router_state.id]),
  {noreply,  State#router_state{ nodes= lists:keydelete(ID,1,State#router_state.nodes)++[{ID,?SEC}] }};
handle_cast({delete,ID},State)->
  %%io:format("~p moved from ~p~n",[ID, State#router_state.id]),
  {noreply,  State#router_state{ nodes= lists:keydelete(ID,1,State#router_state.nodes)}};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
 {noreply, State}.

terminate(_Reason, _State) ->
 ok.

code_change(_OldVsn, State, _Extra) ->
 {ok, State}.
