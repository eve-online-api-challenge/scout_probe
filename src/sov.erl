%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Process for managing soverenity entities from EVE. Capital, Alliances, Campaigns.
%%% @end
%%% Created : 21 Feb 2016 by Redpate
%%% Copyright © 2016 Redpate
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%-------------------------------------------------------------------
-module(sov).

-behaviour(gen_server).

%% API
-export([start_link/0,get_capital_sytem/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
 terminate/2, code_change/3]).

-record(sov_state,{alliances=[], capitals=[], events=[], update_counter=1}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
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
%% @spec init([]) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  timer:send_interval(60000, self(), update),  % might be worst way of updating. check 1 alliance for minute
  timer:send_interval(30000, self(), update_events),
  Alliances = [{proplists:get_value(<<"id_str">>,Proplist), proplists:get_value(<<"name">>,Proplist)} ||
    {Proplist}<-pub_crest:req("/alliances/", all)],
  {ok,[CapDump]}=file:consult("sov.dump"), %% get initial sov, gues, no easy way to get capitals of 3000 alliances (even with 150 per sec limmit)
  {ok, #sov_state{capitals=CapDump, alliances=Alliances}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_cast(Msg, State) -> {reply, Reply, State}
%% @end
%%--------------------------------------------------------------------


handle_call({get,ID}, _From, State) ->
 _Capital = lists:keyfind(ID,2,State#sov_state.capitals),
 Capital = case _Capital of %% format Capital system info to usable format
   {AllianceID,CapitalSystemID, CapitalSystemName}->{proplists:get_value(AllianceID,State#sov_state.alliances),AllianceID,CapitalSystemID, CapitalSystemName};
   _->_Capital
 end,
 Events = lists:foldr(fun({_,SubEvent},{Upcoming,Active})->
   {ok,[Y,M,D,H,MM,SS],_} = io_lib:fread("~d-~d-~dT~d:~d:~d", binary_to_list(proplists:get_value(<<"startTime">>,SubEvent))),
   Now = calendar:now_to_universal_time(erlang:timestamp()),
   if
     {{Y,M,D},{H,MM,SS}} > Now -> %upcoming event
       {Upcoming++[{proplists:get_value(<<"eventType_str">>,SubEvent),{SubEvent}}],Active};
     true-> %active event
       {Upcoming,Active++[{proplists:get_value(<<"eventType_str">>,SubEvent),{SubEvent}}]}
   end end, {[],[]}, proplists:lookup_all(ID,State#sov_state.events)) ,
 {reply, {Capital,Events}, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(stop, State) ->
 {stop, normal, State};

handle_cast(_Msg, State) ->
 {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling info "update" and "update_events" call.
%%
%% @spec handle_info(Info, State) -> {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_info(update, State) ->
  lists:foreach(fun({_AllianceID,CapitalSystemID, _CapitalSystemName})-> router:apply(cast, CapitalSystemID, {sov,CapitalSystemID,2,0}) end,State#sov_state.capitals),
  Counter = if
    State#sov_state.update_counter >= length(State#sov_state.alliances)->
      %%io:format("~p| All alliances updated~n",[time()]),
      file:write_file("sov.dump",io_lib:format("~p.",[State#sov_state.capitals]),[write]),
      1;
    true->
      State#sov_state.update_counter+1
  end,
  {ID,_Name}=lists:nth(Counter,State#sov_state.alliances),
  case get_capital_sytem(ID) of
    undefined->
      {noreply, State#sov_state{update_counter=Counter, capitals= lists:keydelete(ID,1,State#sov_state.capitals)}};
    {SysID, Name}->
      router:apply(cast,SysID,{sov,SysID,2,0}),  % post ref link to sov in system
      {noreply, State#sov_state{update_counter=Counter, capitals= lists:keydelete(ID,1,State#sov_state.capitals)++[{ID, SysID, Name}]}}
  end;
handle_info(update_events, State) ->
  NewEvents = lists:map(fun({Proplist})->
    {R} = proplists:get_value(<<"sourceSolarsystem">>,Proplist),
    ID=proplists:get_value(<<"id_str">>,R),
    router:apply(cast,ID,{sov,ID,2,0}),
    {ID, Proplist}
  end, pub_crest:req("/sovereignty/campaigns/", all)), %% get all events, parse them and send to routers
  {noreply, State#sov_state{events=NewEvents}};
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get info about alliance capital system
%%
%% @spec get_capital_sytem(AllianceID) -> {SolarSystemID, SolarSystemName}|
%% undefined
%% @end
%%--------------------------------------------------------------------
get_capital_sytem(ID)->
  {Proplist}=pub_crest:req(io_lib:format("/alliances/~s/",[ID])),
  CapitalSystem= element(1,proplists:get_value(<<"capitalSystem">>,Proplist,{[]})),
  IDStr= proplists:get_value(<<"id_str">> ,CapitalSystem),
  case IDStr of
    undefined->
      undefined;
    _->
      {IDStr, proplists:get_value(<<"name">> ,CapitalSystem)}
  end.

terminate(_Reason, _State) ->
 ok.

code_change(_OldVsn, State, _Extra) ->
 {ok, State}.
