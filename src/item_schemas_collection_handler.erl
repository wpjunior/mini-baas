-module(item_schemas_collection_handler).
-export([init/2]).
-define(ITEM_SCHEMA_COLLECTION, <<"item-schemas">>).


init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    handle(Method, Req, Opts).


handle(<<"GET">>, Req, Opts) ->
    [MongoConnection] = Opts,

    Filter = filter_builder:build_from_req(Req),
    Where = filter_builder:where(Filter),

    Result = database:find(MongoConnection, ?ITEM_SCHEMA_COLLECTION, Where),
    JsonBody = schema_object:to_json_list(Result),

    responses:json_success(Req, [MongoConnection], JsonBody);


handle(<<"POST">>, Req, Opts) ->
    {ok, Body, _} = cowboy_req:body(Req),
    [MongoConnection] = Opts,

    case schema_object:from_json(Body) of
        {ok, Schema} ->
            database:insert(MongoConnection, ?ITEM_SCHEMA_COLLECTION, Schema),
            JsonBody = schema_object:to_json(Schema),
            responses:json_created(Req, Opts, JsonBody);

        {invalid_json, _} ->
            responses:invalid_json(Req, Opts)
    end;


handle(_, Req, Opts)->
    responses:method_not_allowed(Req, Opts).
