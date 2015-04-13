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

    case resource_object:from_json(Body) of
        {ok, Resource} ->
            insert_resource_in_collection(MongoConnection, CollectionName, Req, Resource);
        {invalid_json, _} ->
            invalid_json_response(MongoConnection, Req)
    end;

handle(_, MongoConnection, _, Req)->
    Resp = cowboy_req:reply(405, Req),
    {ok, Resp, [MongoConnection]}.

insert_resource_in_collection(MongoConnection, CollectionName, Req, Resource) ->
    PrimaryKey = erlang:list_to_binary(uuid:to_string(uuid:uuid4())),
    ResourceWithPrimaryKey = bson:update('_id', PrimaryKey, Resource),

    mongo:insert(MongoConnection, CollectionName, [ResourceWithPrimaryKey]),
    JsonBody = jiffy:encode(resource_object:to_json(ResourceWithPrimaryKey)),
    Resp = cowboy_req:reply(201, [
                                  {<<"content-type">>, <<"application/json">>}
                                 ], JsonBody, Req),
    {ok, Resp, [MongoConnection]}.

invalid_json_response(MongoConnection, Req) ->
    Resp = cowboy_req:reply(422, Req),
    {ok, Resp, [MongoConnection]}.
