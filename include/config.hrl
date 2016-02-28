%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Main config
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
%gllobal define
-define(SEC, erlang:system_time(seconds)).
-record(crest,{access_token, token_type, owner_hash, character_name, character_id, expires_on, location_id, location_name, jumped_id, jumped_name,
			expires_in=300, callback, in_wh=true, last_call=0, last_update=?SEC,
			refresh_token}).

-record(event,{id,tags,text,time,system,expire=erlang:monotonic_time(seconds)}).

%% CREST
%%-define(CREST_SERVER,"crest-tq.eveonline.com").
-define(CREST_AUTH, pub_crest:get_variable(<<"authEndpoint">>)).
-define(PUBLIC_CREST_HOST,"https://public-crest.eveonline.com").
-define(PUBLIC_CREST_HOST_BIN,list_to_binary(?PUBLIC_CREST_HOST)).
-define(CREST_HOST,"https://crest-tq.eveonline.com").
-define(REDIRECT_URL,"").
-define(APPLICATION_ID,""). %% client id from https://developers.eveonline.com
-define(AUTH_TOKEN, ""). %% precompiled  base64:encode(ClientID++":"++SecretKey)

%% router chache expire time in seconds
-define(MSG_RECORDS_TTL, 120).
-define(MAX_DIF, 500).
-define(SOV_RECORDS_TTL, 1200).
%% router cleaner call period in seconds
-define(CLEANING_TIMER, 30).

%%tracker expire time in seconds
-define(TRACKER_TTL,150).

-define(TRACKER_CREST_DELAY, 25).

%% Cooke Server expire time in seconds
-define(HASH_TTL, 20*60).

%% zkillboard
-define(ZKB_UPDATE_TIMER, 30000).
-define(ZKB_HOST, "https://zkillboard.com").


%% sqlite static export
-define(DB_NAME, db).

%% router
-define(MSG_START_TTL, 3). % N constelations away

%% event Pool

-define(EVENT_TTL,160). % in seconds
