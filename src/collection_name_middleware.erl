-module(collection_name_middleware).
-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    case proplists:get_value(collection_name, cowboy_req:bindings(Req)) of
        undefined ->
            {ok, Req, Env};
        CollectionName ->
            check_collection_name(Req, Env, CollectionName)
    end.

check_collection_name(Req, Env, CollectionName) ->
    case schema_service:schema_is_found(CollectionName) of
        true ->
            {ok, Req, Env};

        false ->
            {ok, Resp, _} = responses:not_found(Req, undefined),
            {stop, Resp}

    end.
