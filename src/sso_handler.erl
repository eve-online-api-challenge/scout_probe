%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Haandling last step of SSO
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
-module(sso_handler).

-compile(export_all).

-include("config.hrl").

init(_,Req, Opts) ->
	init(Req, Opts).
init(Req, Opts) ->
	inets:start(),
	%Action = hd(cowboy_req:path_info(Req)),
	%Method = cowboy_req:method(Req),
	QsVals = cowboy_req:parse_qs(Req),
	{_, Code} = lists:keyfind(<<"code">>, 1, QsVals),
	{_, _State} = lists:keyfind(<<"state">>, 1, QsVals), %% yeah better to check state. TODO 
	{Status, VerifiedRecord} = tracker:start(Code),
	%% hash woud define character id. by character id service would be able to find all it needed.
	case Status of
		new->
		 	ok; %nothing to clean
		duplicated-> % other way is to use old hash. now we are updating it completle. so, other active sessions for this tracker going to crash.
			MS = [{{'_','$1','_'},
		        [{'==',VerifiedRecord#crest.character_id,'$1'}],
		        [true]}], % match for N sec old records
		  ets:select_delete(hash,MS)
	end,
	Hash = hash(),%%  creaning in event pool
	true = ets:insert_new(hash,{Hash,VerifiedRecord#crest.character_id,erlang:monotonic_time(seconds)}), %% one more hash uniqe validation in case of race conditions
	CookieReq = cowboy_req:set_resp_cookie(
			<<"user">>, base64:encode(Hash), [{path, <<"/">>}], Req),
	Reply = cowboy_req:reply(302, [
				{<<"content-type">>, <<"text/html">>},
				{<<"Location">>, <<"/tracker">>}],
			         <<"Redirecting!">> , CookieReq),
	{ok, Reply, Opts}.

hash()-> %% might be usefull to limit generator iterations.
	Hash = crypto:strong_rand_bytes(5),
	case ets:lookup(hash,Hash) of
		[]->
			Hash;
		List when is_list(List)->
			hash();
		_-> %% error with ets. none cares about hash uniqe now.
			Hash
	end.
