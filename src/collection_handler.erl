-module(collection_handler).
-export([init/2]).


init(Req, [MongoConnection]) ->
    Method = cowboy_req:method(Req),
    [{collection_name, CollectionName}] = cowboy_req:bindings(Req),
    handle(Method, MongoConnection, CollectionName, Req).


handle(<<"GET">>, MongoConnection, CollectionName, Req) ->
    case schema_service:schema_is_found(CollectionName) of
        true ->
            Filter = filter_builder:build_from_req(Req),
            Where = filter_builder:where(Filter),
            Result = database:find(MongoConnection, CollectionName, Where),
            JsonBody = resource_object:to_json_list(Result),
            responses:json_success(Req, [MongoConnection], JsonBody);
        false ->
            responses:not_found(Req, [MongoConnection])
    end;

handle(<<"POST">>, MongoConnection, CollectionName, Req) ->
    {ok, Body, _} = cowboy_req:body(Req),

    case resource_object:from_json(Body) of
        {ok, Resource} ->
            ResourceWithPrimaryKey = database:insert(MongoConnection, CollectionName, Resource),
            JsonBody = resource_object:to_json(ResourceWithPrimaryKey),
            responses:json_created(Req, [MongoConnection], JsonBody);

        {invalid_json, _} ->
            responses:invalid_json(Req, [MongoConnection])
    end;


handle(_, MongoConnection, _, Req)->
    responses:method_not_allowed(Req, [MongoConnection]).
