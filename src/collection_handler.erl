-module(collection_handler).

-export([init/2]).

init(Req, [MongoConnection]) ->
    Method = cowboy_req:method(Req),
    [{collection_name, CollectionName}] = cowboy_req:bindings(Req),
    handle(Method, MongoConnection, CollectionName, Req).

handle(<<"GET">>, MongoConnection, _, Req) ->
    responses:not_found(Req, [MongoConnection]);

handle(<<"POST">>, MongoConnection, CollectionName, Req) ->
    {ok, Body, _} = cowboy_req:body(Req),

    case resource_object:from_json(Body) of
        {ok, Resource} ->
            insert_resource_in_collection(MongoConnection, CollectionName, Req, Resource);
        {invalid_json, _} ->
            responses:invalid_json(Req, [MongoConnection])
    end;

handle(_, MongoConnection, _, Req)->
    responses:method_not_allowed(Req, [MongoConnection]).

insert_resource_in_collection(MongoConnection, CollectionName, Req, Resource) ->
    ResourceWithPrimaryKey = database:insert(MongoConnection, CollectionName, Resource),
    JsonBody = jiffy:encode(resource_object:to_json(ResourceWithPrimaryKey)),
    responses:json_created(Req, [MongoConnection], JsonBody).
