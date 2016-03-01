%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Dashboard handler
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
-module(tracker_handler).

-compile(export_all).

-include("config.hrl").

init(_,Req, Opts) ->
	init(Req, Opts).
init(Req, Opts) ->
  #{user := Hash}
		= cowboy_req:match_cookies([{user, [], <<>>}], Req),
  case Hash of
    <<>>->to_sso(Req,Opts);
    _->
      case ets:lookup(hash,base64:decode(Hash)) of
    		[]->
    			to_sso(Req,Opts);
    		[{_,TrackerID,_}]->
          Upgrade = cowboy_req:header(<<"upgrade">>,Req),
          case Upgrade of
            undefined->
              case tracker:get(TrackerID) of
                {tracker,Crest}->
                  {ok, Answer}=main_template:render([{name,Crest#crest.character_name},
										{location_name, Crest#crest.location_name },
										{jumped_id, Crest#crest.jumped_id},{jumped_name, Crest#crest.jumped_name}]),
            	    {ok,cowboy_req:reply(200, [
            		    {<<"content-type">>, <<"text/html">>}
            	    ], Answer , Req), Opts};
                _->
                  ets:delete(hash,base64:decode(Hash)),
                  tracker_sup:del(TrackerID),
                  to_sso(Req,Opts)
              end;
            <<"websocket">>->
              tracker:send(TrackerID,{set,callback,self()}),
              {cowboy_websocket, Req, [TrackerID]}
          end;
    		_-> %% error with ets
    			to_sso(Req,Opts)
    	end
  end.

to_sso(Req,Opts)->
  Reply = cowboy_req:reply(302, [
      {<<"content-type">>, <<"text/html">>},
      {<<"Location">>, <<"/sso">>}],
    <<"Redirecting to EVE Online SSO!">> , Req),
  {ok, Reply, Opts}.

websocket_handle({text, <<"and">>}, Req, [TrackerID]) ->
  tracker:update(TrackerID),
  {tracker,VerifiedRecord} = tracker:get(TrackerID),
  lists:foreach(fun(ID)->
    EventList = gen_server:call(sov,{get,ID}),
    case EventList of
      {Capital, {Upcoming,Active}}->
        Cap = case Capital of
          []->
            [];
          {AllianceName,AllianceID,CapitalSystemID, CapitalSystemName}->
            [{capital_alliance, AllianceID},{capital_alliance_name, AllianceName},{capital_id, CapitalSystemID},{capital_name, CapitalSystemName}]
        end,
        tracker:send_callback(VerifiedRecord, {text, jiffy:encode({[
            {msg_type, 1} % sov msg
          ]++[{upcoming,{Upcoming}}, {active, {Active}}]++Cap
          })});
      _-> ok
    end	end, router:apply(call,VerifiedRecord#crest.location_id, sov)),
  {ok, Req, [TrackerID]};
websocket_handle({text, <<"D:", Destination/binary>>}, Req, [TrackerID]=State) ->
  tracker:send(TrackerID,{set,destination,list_to_integer(binary_to_list(Destination))}),
  {ok, Req, State};
websocket_handle({text, <<"W:", Destination/binary>>}, Req, [TrackerID]=State) ->
  tracker:send(TrackerID,{set,waypoint,list_to_integer(binary_to_list(Destination))}),
  {ok, Req, State};
websocket_handle({text, <<"S">>}, Req, [TrackerID]=State) ->
  tracker:send(TrackerID,stop),
  tracker_sup:del(TrackerID),
  {stop, Req, State};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({text, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(stop, Req, State) ->
    {stop, Req, State};
websocket_info(_Info, Req, State) ->
  	{ok, Req, State}.
