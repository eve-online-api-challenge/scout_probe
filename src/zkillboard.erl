%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Prety stupid interface for zkillboard. I'm sorry for zkb team for this.
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
-module(zkillboard).

-compile(export_all).

-include("killmail.hrl").

-include("config.hrl").


get_json(LastKill)-> % get json with al kills
	RandomTail = "/pastSeconds/"++integer_to_list(random:uniform(700)+700), %% not evven close to random, even not initiated by time or crypto module.
	case httpc:request(get,{?ZKB_HOST++"/api/losses/afterKillID/"++integer_to_list(LastKill)++RandomTail, % sorry for zkillboard guys. but forced cache ignoring headers.
		[{"Accept-Encoding", "gzip"}]},[],[]) of
	  {ok,{{_,200,_}, Headers, Data}} ->
	    JResult = case proplists:get_value("content-encoding", Headers) of
	      "gzip" ->
	        zlib:gunzip(Data);
	       _A ->
	        Data
	   	end,
	   	jiffy:decode(JResult);
	  Rslt ->
	    error_logger:info_msg("Request failed: ~p~n", [Rslt]),
	    []
	  end.

get_last_kill_dirty(Id)->  % find last killID in zkb
  case httpc:request(get,{?ZKB_HOST++"/api/losses/pastSeconds/598/", % 600 is cached sometimes
    [{"Accept-Encoding", "gzip"}]},[],[]) of
  	{ok,{{_,200,_}, Headers, Data}} ->
      JResult = case proplists:get_value("content-encoding", Headers) of
        "gzip" ->
          zlib:gunzip(Data);
         _A ->
          Data
      end,
      [{KillMail}|_]=jiffy:decode(JResult),
      to_integer(proplists:get_value(<<"killID">>,KillMail));
    _ ->
      Id
  end.

check_kills(LastKill)->
	A = get_json(LastKill),
	if
		length(A)>0 ->
			lists:foldr(fun({X},Acc)-> Acc2=?MODULE:parse_kill(X), max(Acc,Acc2) end, LastKill-1, A);
		true->
			LastKill-1
	end.


parse_kill(KillMail)->
  KillID = to_integer(proplists:get_value(<<"killID">>,KillMail)),
  KillTime = binary_to_list(proplists:get_value(<<"killTime">>,KillMail)),
  SystemID = to_integer(proplists:get_value(<<"solarSystemID">>,KillMail)),
  SystemIDStr = list_to_binary(integer_to_list(SystemID)),
  Victim = element(1,proplists:get_value(<<"victim">>,KillMail)),
  %VictimName = proplists:get_value(<<"characterName">>,Victim),
  VictimShip = to_integer(proplists:get_value(<<"shipTypeID">>,Victim)),
  {Position} = proplists:get_value(<<"position">>,KillMail),
  Attackers = [#attacker{ id=proplists:get_value(<<"characterID">>,Attacker,0),
            name=proplists:get_value(<<"characterName">>,Attacker,""),
            ship=proplists:get_value(<<"shipTypeID">>,Attacker,0),
            module=proplists:get_value(<<"weaponTypeID">>,Attacker,0)}
  || {Attacker}<-proplists:get_value(<<"attackers">>,KillMail,[])],
	% tried to understand way of parsing fit. unused part for now
  %% ISK = to_float(proplists:get_value(<<"totalValue">>, element(1,proplists:get_value(<<"zkb">>,KillMail)))),
  %FitList=lists:foldr(fun({X},Acc)->
  %  case to_integer(proplists:get_value(<<"typeID">>,X)) of
  %    0-> Acc;
  %    ItemID->
  %      Dupllicted = lists:member(ItemID,Acc),
  %      if
  %        Dupllicted-> Acc;
  %        (ItemID>10) and (ItemID<35)-> Acc++[ItemID];
  %        (ItemID>91) and (ItemID<100)->Acc++[ItemID];
  %        true-> Acc
  %      end
  %  end
  %end, [], proplists:get_value(<<"items">>,KillMail,[])),
	%% find nearest obj and calculate distance
  Y = proplists:get_value(<<"y">>,Position),
  X = proplists:get_value(<<"x">>,Position),
  Z = proplists:get_value(<<"z">>,Position),
  [_,{rows, [{_ItemID, _Locationname,XX,YY,ZZ}|_]}]=sqlite3:sql_exec(?DB_NAME, "SELECT itemID,itemName,x,y,z FROM mapDenormalize WHERE abs(x - ?) = (SELECT min(abs(x - ?)) FROM mapDenormalize where solarSystemID=?);",[X, X, SystemID]),
  Locationname = if
    (XX bor YY bor ZZ)==0 -> %% Star of system
      [_,{rows, [{__Locationname}]}]=sqlite3:sql_exec(?DB_NAME, "select solarSystemName from mapSolarSystems where solarSystemID=?;",[SystemID]), __Locationname++" - Star";
    _Locationname==null-> %% others
      [_,{rows, [{__Locationname}]}]=sqlite3:sql_exec(?DB_NAME, "select itemName from invNames where itemID = ?;",[_ItemID]), __Locationname;
    true-> %% weird exceptions
      _Locationname
  end,
  KM = #km{id=KillID, solarSystem=SystemIDStr, time=KillTime, attackers=Attackers, shiptype=VictimShip, pos={Locationname,
    round(math:sqrt(math:pow((YY-Y),2)+math:pow((XX-X),2)+math:pow((ZZ-Z),2) )/1000)} % distance in km
		},
  {System,ID,Text,Tags,Time} = generate_msg(KM),
  Event = #event{id=ID,
    tags=Tags,
    text=Text,
    time=Time,
    system=System
    },
	%%io:format("[~p] New Event: ~p~n", [time(),Event]),
  event_pool:add(Event),
 	router:apply(cast,SystemIDStr,{msg,ID,?MSG_START_TTL,0}),
  KillID.

generate_msg(KM)->
	InvolvedShips = [X#attacker.ship ||X<-KM#km.attackers],
	SupersInvolved = lists:foldr(fun(X,Acc)->Acc or lists:member(X,[11567,671,3764,23773,23913,3514,23917,22852,23919]) end, false,InvolvedShips),
	CapitalsInvolved = lists:foldr(fun(X,Acc)->Acc or lists:member(X,[24483, 23915, 23911, 23757, 19724, 34339, 19722, 19726,19720, 34341, 34345, 34343]) end, false,InvolvedShips),
	{Name,L}=KM#km.pos,
	Where = if
		L > 1000 ->
			" near "++Name;
		true->% wrek less then 1k km away from object
			" on "++Name
	end,
	if
		SupersInvolved ->
			{KM#km.solarSystem,KM#km.id,"Supercapital"++Where,[dictors],KM#km.time};
		CapitalsInvolved ->
			{KM#km.solarSystem,KM#km.id,"Capital"++Where,[],KM#km.time};
		length(InvolvedShips) <5 -> % need filter for solo kills?
			{KM#km.solarSystem,KM#km.id,"Few ships"++Where,[few],KM#km.time};
		length(InvolvedShips) <15  ->
			{KM#km.solarSystem,KM#km.id,"Small fleet"++Where,[small],KM#km.time};
		length(InvolvedShips) <31  ->
			{KM#km.solarSystem,KM#km.id,"Medium fleet"++Where,[medium],KM#km.time};
		true ->
			{KM#km.solarSystem,KM#km.id,"Large fleet"++Where,[large],KM#km.time}
	end.

to_integer(Int) when is_integer(Int)->Int;
to_integer(Binary) when is_binary(Binary)->
	list_to_integer(binary_to_list(Binary)).

%to_float(Binary) when is_binary(Binary)->
%	list_to_float(binary_to_list(Binary));
%to_float(F) ->F.
