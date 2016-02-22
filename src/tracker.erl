%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Worker for tracking uesr current system, send crest reqest, keep crest keys updated.
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
-module(tracker).

-include("config.hrl").

-export([start_link/1, start/1, stop/1]).
-export([loop/1, send/2, get/1, update/1, set_waypoint/2, set_destination/2, send_callback/2]).

start_link(CREST)-> %% start worker
	{ok,spawn_link(?MODULE,loop,[CREST])}.

start(Code)-> % start new tracker check for duplicates, register if none.
		VerifiedRecord = crest:auth(Code),
		PID = tracker_sup:get_pid(VerifiedRecord#crest.character_id),
		case PID of
			undefined->
				tracker_sup:add(VerifiedRecord),{new,VerifiedRecord};
			PID->
				PID ! {set, crest, VerifiedRecord},{duplicated,VerifiedRecord}
		end.
stop(ID)-> % stop tracker by id(character id used)
		tracker_sup:del(ID).

loop(#crest{}=VerifiedRecord)-> %% main loop
	Now = ?SEC,
	if
		(Now - VerifiedRecord#crest.last_update) > ?TRACKER_TTL -> %% tracker was not updted too long, stop it
			router:apply(cast,VerifiedRecord#crest.location_id,{delete, VerifiedRecord#crest.character_id}), %% delete from current router
			send_callback(VerifiedRecord, stop), %% delete from supervisor, close linked ws
			exit(normal);
		(Now - VerifiedRecord#crest.last_call) > ?TRACKER_CREST_DELAY -> % time is up/ lets call CREST
			{NewVerifiedRecord, Res}=crest:req(VerifiedRecord, get,  io_lib:format(?CREST_HOST++"/characters/~p/location/",[VerifiedRecord#crest.character_id]), []),
			case Res of
				{[{<<"solarSystem">>,{PropList}}]}-> %% found location
					SolarSystemName=proplists:get_value(<<"name">>,PropList),
					SolarSystemID=proplists:get_value(<<"id_str">>,PropList),
					_SolarSystemID=proplists:get_value(<<"id">>,PropList),
					IsWH = _SolarSystemID > 31000000,
					if
						IsWH->%went in wh. fly safe xD
							ok;
						IsWH=/=VerifiedRecord#crest.in_wh-> % jumped out wh. or just logged on. throw tons of events in this face!
							lists:foreach(fun({ID,_Time})->
								EventList = event_pool:get(ID),
								case EventList of
									[]->
										ok;
									[Event]->
										send_callback(VerifiedRecord, {text, jiffy:encode({[
											{msg_type, 0}, % event msg
											{id, list_to_binary(integer_to_list(Event#event.id))},
											{text, list_to_binary(Event#event.text)},
											{time, list_to_binary(Event#event.time)},
											{system, pub_crest:req(io_lib:format("/solarsystems/~s/",[Event#event.system]))} %% todo - use sqlite
										]})})
								end	end, router:apply(call,SolarSystemID, events)), %% all cached killmails in your face!
								lists:foreach(fun({ID,_Time})->
									EventList = gen_server:call(sov,{get,ID}),
									case EventList of
										{Capital, {Upcoming,Active}}->
											Cap = case Capital of
												false->
													[];
												{AllianceName,AllianceID,CapitalSystemID, CapitalSystemName}->
													[{capital_alliance, AllianceID},{capital_alliance_name, AllianceName},{capital_id, CapitalSystemID},{capital_name, CapitalSystemName}]
											end,
											send_callback(VerifiedRecord, {text, jiffy:encode({[
													{msg_type, 1} % sov msg
												]++[{upcoming,{Upcoming}}, {active, {Active}}]++Cap
												})});
										_-> ok
									end	end, router:apply(call,SolarSystemID, sov)),  %% all cached soverenity entities in your face!
							router:apply(cast,SolarSystemID,{update, VerifiedRecord#crest.character_id}),
							router:apply(cast,VerifiedRecord#crest.location_id,{delete, VerifiedRecord#crest.character_id});
						true-> %still in known space. just basic tracking
							if
								SolarSystemID == VerifiedRecord#crest.location_id->
									router:apply(cast,SolarSystemID,{update, VerifiedRecord#crest.character_id});
								true->
									router:apply(cast,SolarSystemID,{update, VerifiedRecord#crest.character_id}),
									router:apply(cast,VerifiedRecord#crest.location_id,{delete, VerifiedRecord#crest.character_id})
							end
					end,
					loop(NewVerifiedRecord#crest{in_wh=IsWH, location_id=SolarSystemID, location_name=SolarSystemName, last_call=Now});
				_-> %% unknown location, servers ofline, character logged off
					loop(NewVerifiedRecord#crest{last_call=Now})
			end;
		true-> %% no time for crest, fast, check erlang messages
			receive
				{event,ID}-> %% router send event, parse and send it to ws (callback)
					EventList = event_pool:get(ID),
					case EventList of
						[]-> %% its a joke? mabe cached for a short period of time
							loop(VerifiedRecord);
						[Event]->
							send_callback(VerifiedRecord, {text, jiffy:encode({[
									{msg_type, 0}, % event msg
									{id, list_to_binary(integer_to_list(Event#event.id))},
									{text, list_to_binary(Event#event.text)},
									{time, list_to_binary(Event#event.time)},
									{system, pub_crest:req(io_lib:format("/solarsystems/~s/",[Event#event.system]))} %% todo - use sqlite
								]})}),
							loop(VerifiedRecord)
					end;
				{sov,ID}->
					EventList = gen_server:call(sov,{get,ID}),
					case EventList of
						{Capital, {Upcoming,Active}}->
							Cap = case Capital of
								false->
									[];
								{AllianceName,AllianceID,CapitalSystemID, CapitalSystemName}->
									[{capital_alliance, AllianceID},{capital_alliance_name, AllianceName},{capital_id, CapitalSystemID},{capital_name, CapitalSystemName}]
							end,
							send_callback(VerifiedRecord, {text, jiffy:encode({[
									{msg_type, 1} % sov msg
								]++[{upcoming,{Upcoming}}, {active, {Active}}]++Cap
								})});
						_-> ok
					end,
					loop(VerifiedRecord);
				update->
					loop(VerifiedRecord#crest{last_update=Now});
				{set,waypoint,ID}-> %% ws asket to set waypoint
					crest:set_waypoint(VerifiedRecord,ID,[<<"first">>]),
					loop(VerifiedRecord);
				{set,destination,ID}-> %% ws asket to set destination
					crest:set_waypoint(VerifiedRecord,ID,[<<"clearOtherWaypoints">>]),
					loop(VerifiedRecord);
				{set,crest,Crest}-> %% forced record update
					loop(Crest);
				{set,callback,PID}-> %% link to ws
					io:format("Tracker set callback ~p~n", [PID]),
					loop(VerifiedRecord#crest{callback=PID});
				{get,PID}-> %% get state
					PID ! {tracker,VerifiedRecord},
					loop(VerifiedRecord);
				stop->
					send_callback(VerifiedRecord, stop),
					exit(normal);
				_A->
					loop(VerifiedRecord)
				after (?TRACKER_CREST_DELAY-(Now - VerifiedRecord#crest.last_call))*1000 ->
					loop(VerifiedRecord)
		    end
	end.

send(ID,Msg)->
	PID = tracker_sup:get_pid(ID),
	case PID of
		undefined->
			{error, badarg};
		_->PID ! Msg
	end.

update(ID)->
		send(ID,update).

get(ID)->
	PID = tracker_sup:get_pid(ID),
	case PID of
		undefined->
			{error, badarg};
		_->PID ! {get,self()},
		receive
			A->A
		after 10000->
			timeout
		end
	end.

set_waypoint(ID,System)->
	?MODULE:send(ID,{set,waypoint,System}).
set_destination(ID,System)->
	?MODULE:send(ID,{set,destination,System}).

send_callback(VerifiedRecord, Msg)->
	case VerifiedRecord#crest.callback of
		undefined-> Msg;
		PID->
			PID ! Msg
	end.
