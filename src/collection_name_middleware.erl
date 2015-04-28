-module(collection_name_middleware).
-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    _CollectionName = proplists:get_value(collection_name, cowboy_req:bindings(Req)),
    {ok, Req, Env}.
