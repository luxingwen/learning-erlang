-module(name_server).

-export([init/0, add/2, find/1, handle/2]).

add(Name, Place) ->
	server:rpc(name_server, {add, Name, Place}).

find(Name) ->
	server:rpc(name_server, {find, Name}).

init() ->
	dict:new().

handle({add, Name, Place}, Dict) ->
	{ok, dict:store(Name, Place, Dict)};

handle({find, Name}, Dict) ->
	{dict:find(Name, Dict), Dict}.