%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Starts and mnage trackers
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
-module(tracker_sup).

-compile(export_all).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks  supervisor:which_children(tracker_sup)
-export([init/1]).
-include("config.hrl").
%% Helper macro for declaring children of supervisor
-define(CHILD(CREST),{CREST#crest.character_id, {tracker, start_link, [CREST]}, transient, brutal_kill, worker, []}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, { {one_for_one, 5, 10}, []} }.

add(CREST)->
	supervisor:start_child(?MODULE, ?CHILD(CREST)).

del(ID)->
  case get_pid(ID) of
    undefined->
      supervisor:delete_child(?MODULE, ID);
    PID->
      PID ! stop,
      timer:apply_after(1000,supervisor,delete_child, [?MODULE, ID])
  end.

get_pid(Id)->
	Res = lists:keyfind(Id, 1,supervisor:which_children(?MODULE)),
	case Res of
		{Id, Child, _Type, _Modules}->Child;
		{Id, undefined, _Type, _Modules}->supervisor:delete_child(?MODULE, Id),undefined;
		_->undefined
	end.
