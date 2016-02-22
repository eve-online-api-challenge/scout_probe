%%%-------------------------------------------------------------------
%%% @author Redpate <redpate@hotmail.com>
%%% @copyright (C) 2016, Redpate
%%% @doc
%%% Pure interface for public CREST
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
-module(pub_crest).

-include("config.hrl").

-export([req/1, req/2]).

req(Endpoint)->
  Res =httpc:request(?PUBLIC_CREST_HOST++Endpoint),
	case Res of
		{ok,{{_,200,_}, _Headers, Body}}->
			jiffy:decode(Body);
		{ok,{{_,_Code,_}, _Headers, _Body}}->
			{[]};
    {error,_}->
      {[]}
	end.

req(Endpoint, all)->
  {Page}=pub_crest:req(Endpoint),
  NextPage = proplists:get_value(<<"next">>, Page, none),
  case NextPage of
    none->
      proplists:get_value(<<"items">>, Page, []);
    {[{<<"href">>, NextPageUrl}]}->
      proplists:get_value(<<"items">>, Page, [])++req_dirty(binary_to_list(NextPageUrl), all)
  end.

req_dirty(Endpoint, all)->
  Res =httpc:request(Endpoint),
	case Res of
		{ok,{{_,200,_}, _Headers, Body}}->
      {Page}=jiffy:decode(Body),
      NextPage = proplists:get_value(<<"next">>, Page, none),
      case NextPage of
        none->
          proplists:get_value(<<"items">>, Page, []);
        {[{<<"href">>, NextPageUrl}]}->
          proplists:get_value(<<"items">>, Page, [])++req_dirty(binary_to_list(NextPageUrl), all)
      end;
		{ok,{{_,_Code,_}, _Headers, _Body}}->
			[]
	end.
