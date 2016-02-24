%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Simple non OTP worker
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

-module(km_collector).

-export([collector/0, collector/2]).

-include("config.hrl").

collector()-> % start worker
  LastID = zkillboard:get_last_kill_dirty(52125320),
  error_logger:info_msg("Last killID ~p~n",[LastID]),
  PID = spawn_link(?MODULE,collector, [LastID-1,LastID-1]),
  timer:send_interval(?ZKB_UPDATE_TIMER, PID, update), {ok,PID}.

collector(LastKill,MaxKill)-> %% simple worker
  receive
    update->
      KillID = zkillboard:check_kills(LastKill),
      if
        KillID > MaxKill->
          collector(KillID,KillID);
        true->
          collector(LastKill-1,MaxKill)
      end
  after
    60000 ->
      error_logger:error_msg("Collector died in ages~n")
  end.

%% --------------------------------------
%% old part just for history of wheel.
%% first idea was pase killmails for all groups of ships (about 44 groups). but with few dirty code and learning zkb api this idea was drown in whisky
%% --------------------------------------
%worker()->
%  {ok,spawn_link(?MODULE,worker_loop, [])}.
%worker_loop()->
%    {0,MasterPid,_,_}=lists:keyfind(0,1,supervisor:which_children(collector_sup)),worker_loop(MasterPid).
%worker_loop(MasterPid)->
%  MasterPid ! {get_task, self()},
%  receive
%    empty->
%      receive
%        {ID,LastKill}->
%          NewLastKill = zkillboard:check_kills(ID,LastKill),
%          MasterPid ! {update_task,ID,NewLastKill},
%          worker_loop(MasterPid)
%      after
%        60000 ->
%          ok
%      end;
%    {ID,LastKill}->
%      NewLastKill = zkillboard:check_kills(ID,LastKill),
%      MasterPid ! {update_task,ID,NewLastKill},
%      worker_loop(MasterPid)
%  after
%    2*60000 ->
%      io:format("Pool is dead?~n")
%  end.



%master()->
%  [_,{rows, GroupsList}]=sqlite3:sql_exec(db, "select groupID from invGroups where categoryID=6;"),
%  LastID = zkillboard:get_last_kill_dirty(52094252),
%  io:format("Last killID ~p~n",[LastID]),
%  Pool = [{GroupID, LastID}||{GroupID}<-GroupsList],
%  PID = spawn_link(?MODULE,master_loop, [Pool,length(Pool)+1,1]),
%  timer:send_interval(?ZKB_UPDATE_TIMER, PID, update), {ok,PID}.

%master_loop(Pool,NMax,N)->
%  receive
%    {get_task,PID} ->
%      if
%        NMax==N ->
%          PID ! empty, master_loop(Pool,NMax,N);
%        true->
%          PID ! lists:nth(N,Pool), master_loop(Pool,NMax,N+1)
%      end;
%    {update_task,ID,Key}->
%      master_loop(lists:keyreplace(ID, 1, Pool, {ID,Key}),NMax,N);
%    update->
%      ActiveWorkers=lists:filter(fun({Id,Child,_,_})-> (Id=/=0) and (Child=/=restarting) end,supervisor:which_children(collector_sup)),
%      NewN = lists:foldr(fun({_Id,Child,_,_},Acc)-> Child ! lists:nth(Acc,Pool),Acc+1 end,1,ActiveWorkers),
%      master_loop(Pool,NMax,NewN)
%  after
%    60000 ->
%      io:format("Pool died from ages~n")
%  end.
