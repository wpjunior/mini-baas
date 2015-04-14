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
    ResourceWithPrimaryKey = insert_primary_key_if_necessary(Resource),
    mongo:insert(MongoConnection, CollectionName, [ResourceWithPrimaryKey]),
    JsonBody = jiffy:encode(resource_object:to_json(ResourceWithPrimaryKey)),
    Resp = cowboy_req:reply(201, [
                                  {<<"content-type">>, <<"application/json">>}
                                 ], JsonBody, Req),
    {ok, Resp, [MongoConnection]}.

insert_primary_key_if_necessary(Resource) ->
    %% TODO: This function can be improved, the method looks like assign_id method at erlang driver, see more:
    %% https://github.com/comtihon/mongodb-erlang/blob/master/src/api/mongo.erl#L215

    case bson:lookup('_id', Resource, undefined) of
        undefined ->
            PrimaryKey = erlang:list_to_binary(uuid:to_string(uuid:uuid4())),
            bson:update('_id', PrimaryKey, Resource);
        _ ->
            Resource
    end.

invalid_json_response(MongoConnection, Req) ->
    Resp = cowboy_req:reply(422, Req),
    {ok, Resp, [MongoConnection]}.
