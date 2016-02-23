%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Cache server for public crest api
%%% @end
%%% Created : 23 Feb 2016 by Redpate
%%% Copyright © 2016 Redpate
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%-------------------------------------------------------------------

-module(crest_cache).

-compile(export_all).
-include("config.hrl").

-behaviour(gen_server).

-define(ENDPOINT_CACHE_SCHEMA, [
  {<<"constellations">>, 60*60} ,
  {<<"alliances">>, 30*60} ,
  {<<"systems">>, 30*60}
]).
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {P} = pub_crest:req("/"),
  Cache_Schema = lists:map(fun({EndpointName, CacheTime})->
    <<?PUBLIC_CREST_HOST, EndPointPath/binary>>
      =proplists:get_value(<<"href">>,
        element(1,proplists:get_value(EndpointName,P,{[]})),
        <<?PUBLIC_CREST_HOST>>),
      {EndPointPath,CacheTime}
    end, ?ENDPOINT_CACHE_SCHEMA),
  State = lists:zipwith(fun({EndPointPath,CacheTime}, Index)->
    TableName= list_to_atom(atom_to_list(?MODULE)++"_"++integer_to_list(Index)),
    TablePID =  ets:new(TableName, [{read_concurrency, true}, public, {write_concurrency, true}]),
    timer:send_interval(CacheTime*1000, self(), {flush, EndPointPath}),
    {EndPointPath, TablePID, CacheTime}
  end, Cache_Schema, lists:seq(1,length(Cache_Schema))),
  {ok, State}.

handle_call(_, _From, State) ->
  {reply, State, State}.

handle_cast({req,From,Req}, State) ->
  spawn_link(?MODULE, worker, [list_to_binary(Req), From, State]),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({flush, EndPointPath}, State) ->
  {_, TableID, TTL}=lists:keyfind(EndPointPath, 1, State),
  MS = [{{'_','_','$2'},
        [{'>',{'-',erlang:monotonic_time(seconds),'$2'},TTL}],
        [true]}],
  ets:select_delete(TableID,MS),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

req(Req)->
  gen_server:cast(?MODULE, {req,self(),Req}),
  receive
    Reply->
      Reply
  after
    20000 ->
      timeout
  end.

worker(Req, To, [])->
  To ! pub_crest:req(Req),
  exit(normal);
worker(Req, To, [{EndPointPath, TablePID, TTL}|State])->
  Matched = (nomatch=/=binary:match(Req,EndPointPath)),
  if
    Matched ->
      worker_run(Req, To, TablePID, TTL),
      exit(normal);
    true->
      worker(Req, To, State)
  end.


worker_run(Req, To, TablePID, TTL)->
  Now = ?SEC,
  Res = ets:lookup(TablePID, Req),
  case Res of
    []->
      Reply = pub_crest:req(binary_to_list(Req)),
      To ! Reply,
      ets:insert(TablePID, {Req, Reply, Now});
    [{Req, Reply, Time}] when TTL > Now-Time->
      To ! Reply;
    [{Req, _Reply, _Time}]->
      Reply = pub_crest:req(binary_to_list(Req)),
      To ! Reply,
      ets:delete(TablePID,Req),
      ets:insert(TablePID, {Req, Reply, Now})
  end.
