%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Main application module, make all stuff runing.
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

-module(scout_probe_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

-include("config.hrl").
%% ===================================================================
%% Application callbacks
%% ===================================================================
start()->
	start([],[]).

start(_StartType, _StartArgs) ->
	%% new version of cowboy have troubes with easy start
    application:start(ranch),
    ssl:start(),timer:start(),
    application:start(cowlib),
    application:start(cowboy),
    erlydtl:compile("web/main.html", main_template),
		inets:start(),
		crest_cache:start_link(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {file, "web/index.html"}}, % beter to use something like nginx for static data
            {"/js/[...]", cowboy_static, {dir, "web/js"}},
            {"/img/[...]", cowboy_static, {dir, "web/img"}},
            {"/owl-carousel/[...]", cowboy_static, {dir, "web/owl-carousel"}},
            {"/css/[...]", cowboy_static, {dir, "web/css"}},
            {"/sso", sso_init, []}, %% ss redirect
            {"/tracker", tracker_handler, []},
            {"/auth", sso_handler, []} %% post-sso handler
        ]}
    ]),
    cowboy:start_http(probe_http_listener, 100, [{port, 80}],
        [{env, [{dispatch, Dispatch}]}] %% here should be ssl options. if needed
    ),
    scout_probe_sup:start_link().

stop(_State) ->
    ok.
