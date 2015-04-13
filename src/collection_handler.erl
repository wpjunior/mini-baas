-module(collection_handler).

-export([init/2]).

init(Req, [MongoConnection]) ->
    Method = cowboy_req:method(Req),
    [{collection_name, CollectionName}] = cowboy_req:bindings(Req),
    handle(Method, MongoConnection, CollectionName, Req).

handle(<<"GET">>, MongoConnection, _, Req) ->
    Resp = cowboy_req:reply(404, Req),
    {ok, Resp, [MongoConnection]};

handle(<<"POST">>, MongoConnection, CollectionName, Req) ->
    {ok, Body, _} = cowboy_req:body(Req),

    Resource = resource_object:from_json(Body),

    PrimaryKey = erlang:list_to_binary(uuid:to_string(uuid:uuid4())),
    ResourceWithPrimaryKey = bson:update('_id', PrimaryKey, Resource),

    mongo:insert(MongoConnection, CollectionName, [ResourceWithPrimaryKey]),
    JsonBody = jiffy:encode(resource_object:to_json(ResourceWithPrimaryKey)),
    Resp = cowboy_req:reply(201, [
                                  {<<"content-type">>, <<"application/json">>}
                                 ], JsonBody, Req),
    {ok, Resp, [MongoConnection]};

handle(_, MongoConnection, _, Req)->
    Resp = cowboy_req:reply(405, Req),
    {ok, Resp, [MongoConnection]}.
