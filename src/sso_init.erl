%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Init SSO
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
-module(sso_init).
-behaviour(cowboy_http_handler).

-include("config.hrl").
-compile(export_all).

init(_,Req, Opts) ->
	init(Req, Opts).
init(Req, Opts) ->
    {ok,cowboy_req:reply(302,
        [{<<"Location">>, list_to_binary(
				io_lib:format("~s/oauth/authorize/?response_type=code&redirect_uri=~s&client_id=~s&scope=publicData+characterLocationRead+characterNavigationWrite&state=init",[?CREST_AUTH, ?REDIRECT_URL, ?APPLICATION_ID ]))}],
        <<"Redirecting to EVE Online SSO!">>,
        Req), Opts}.

terminate(_Req, _State) ->
    ok.
