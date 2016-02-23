%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Pure interface for CREST api
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
-module(crest).

-define(AUTH,"Basic "++?AUTH_TOKEN).

-include("config.hrl").

-export([auth/1, obtain_character_id/1, update_token/1]).

-export([req/4, set_waypoint/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates CREST record from Code returned by EVE SSO
%%
%% @spec auth(list()|binary()) -> #crest{}
%% @end
%%--------------------------------------------------------------------

auth(Code)-> %% used in page generator process. so let it crash!
	{ok,{{_,200,_}, _Headers, Body}} = httpc:request(post,
	    {?CREST_AUTH++"/oauth/token", [{"Authorization" , ?AUTH}],
	    "application/x-www-form-urlencoded",
	    list_to_binary(io_lib:format("grant_type=authorization_code&code=~s",[Code]))
	    }, [], []),
	{PropList} = jiffy:decode(Body),
	AccessToken=proplists:get_value(<<"access_token">>,PropList,<<"">>),
	TokenType=proplists:get_value(<<"token_type">>,PropList,<<"">>),
	RefreshToken=proplists:get_value(<<"refresh_token">>,PropList,null),
	ExpiresIn=proplists:get_value(<<"expires_in">>,PropList,300),
	BaseCrestRecod = #crest{access_token=AccessToken, token_type=TokenType, expires_in=ExpiresIn, refresh_token=RefreshToken},
	obtain_character_id(BaseCrestRecod).

	%%--------------------------------------------------------------------
	%% @doc
	%% Update CREST record with character name & id
	%%
	%% @spec auth(#crest{}) -> #crest{}
	%% @end
	%%--------------------------------------------------------------------
obtain_character_id(#crest{}=Crest)->%% used in page generator process. so let it crash!
	{ok,{{_,200,_}, _Headers, Body}} = httpc:request(get,
		  {?CREST_AUTH++"/oauth/verify",
		    [{"Authorization" ,
		  		io_lib:format("~s ~s", [Crest#crest.token_type, Crest#crest.access_token])
	    	}]
	    }, [], []),
	{PropList} = jiffy:decode(Body),
	CharacterID=proplists:get_value(<<"CharacterID">>,PropList,0),
	CharacterName=proplists:get_value(<<"CharacterName">>,PropList,<<"">>),
	ExpiresOn=proplists:get_value(<<"ExpiresOn">>,PropList,0),
	CharacterOwnerHash=proplists:get_value(<<"CharacterOwnerHash">>,PropList,<<"">>),
	Crest#crest{character_name=CharacterName, character_id=CharacterID, expires_on=ExpiresOn, owner_hash=CharacterOwnerHash}.

	%%--------------------------------------------------------------------
	%% @doc
	%% Send request to CREST api
	%%
	%% @spec req(#crest{}, ReqType, URL, ReqBody)-> {#crest{}, Result}
	%% @end
	%%--------------------------------------------------------------------
req(#crest{}=Crest, get, _URL, _)-> % few error to catch. but if there is no conection etc. - let it crash.
	URL = lists:flatten(_URL),
	Res =httpc:request(get,
	    {URL,
	    	[{"Authorization" ,
	    		io_lib:format("~s ~s", [Crest#crest.token_type, Crest#crest.access_token])
	    	}]}, [], []),
	case Res of
		{ok,{{_,200,_}, _Headers, Body}}->
			{Crest,jiffy:decode(Body)};
		{ok,{{_,401,_}, _Headers, _Body}}->
			req(update_token(Crest),get,URL,[]);
		{ok,{{_,Code,_}, _Headers, _Body}}->
			{Crest,{Code,_Body}}
	end;
req(#crest{}=Crest, Method, _URL, ReqBody)-> % req to private crst. few error to catch. but if there is no conection etc. - let it crash.
	URL = lists:flatten(_URL),
	Res =httpc:request(Method,
	    {URL,
	    	[{"Authorization" ,
	    		io_lib:format("~s ~s", [Crest#crest.token_type, Crest#crest.access_token])
	    	}], "application/json",ReqBody
	    }, [], []),
	case Res of
	{ok,{{_,200,_}, _Headers, Body}}->
		Result = try jiffy:decode(Body) of
			Succses->
				Succses
		catch
			_Error->
				Body
		end,
		{Crest,Result};
	{ok,{{_,401,_}, _Headers, _Body}}->
		req(update_token(Crest),Method,URL,ReqBody);
	{ok,{{_,Code,_}, _Headers, _Body}}->
		{Crest,{Code,_Body}}
	end.

%%--------------------------------------------------------------------
%% @doc
%% Send CREST request to set destination endpoint
%%
%% @spec set_waypoint(#crest{}, Id, Options)-> {#crest{}, Result}
%% @end
%%--------------------------------------------------------------------
set_waypoint(VerifiedRecord,Id,Options)->
	crest:req(VerifiedRecord,post,io_lib:format("https://~s/characters/~p/navigation/waypoints/",[?CREST_SERVER,VerifiedRecord#crest.character_id]),
				jiffy:encode({[{<<"solarSystem">>,
				 {[{<<"href">>,
						list_to_binary(io_lib:format("https://~s/solarsystems/~p/",[?CREST_SERVER, Id]))},
					 {<<"id">>,Id}]}},
				{<<"first">>,lists:member(<<"first">>,Options)},
				{<<"clearOtherWaypoints">>,lists:member(<<"clearOtherWaypoints">>,Options)}]})
			).

%%--------------------------------------------------------------------
%% @doc
%% Update Token for CREST request
%%
%% @spec update_token(#crest{})-> #crest{}
%% @end
%%--------------------------------------------------------------------
update_token(#crest{}=Crest)-> %% used in tracker proc. if it is unable to update token - let it crash.
	{ok,{{_,200,_}, _Headers, Body}} = httpc:request(post,
	    {?CREST_AUTH++"/oauth/token", [{"Authorization" , ?AUTH}],
	    "application/x-www-form-urlencoded",
	    list_to_binary(io_lib:format("grant_type=refresh_token&refresh_token=~s",[Crest#crest.refresh_token]))
	    }, [], []),
	{PropList} = jiffy:decode(Body),
	AccessToken=proplists:get_value(<<"access_token">>,PropList,<<"">>),
	RefreshToken=proplists:get_value(<<"refresh_token">>,PropList,null),
	ExpiresIn=proplists:get_value(<<"expires_in">>,PropList,300),
	Crest#crest{access_token=AccessToken, expires_in=ExpiresIn, refresh_token=RefreshToken}.
