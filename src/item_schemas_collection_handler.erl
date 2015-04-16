-module(item_schemas_collection_handler).
-export([init/2]).
-define(ITEM_SCHEMA_COLLECTION, <<"item-schemas">>).


init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    handle(Method, Req, Opts).

handle(<<"POST">>, Req, Opts) ->
    {ok, Body, _} = cowboy_req:body(Req),
    [MongoConnection] = Opts,

    case schema_object:from_json(Body) of
        {ok, Schema} ->
            database:insert(MongoConnection, ?ITEM_SCHEMA_COLLECTION, Schema),
            io:format("s ~p\n", [Schema]),
            JsonBody = schema_object:to_json(Schema),
            responses:json_created(Req, Opts, JsonBody);

        {invalid_json, _} ->
            responses:invalid_json(Req, Opts)
    end;

handle(_, Req, Opts)->
    responses:method_not_allowed(Req, Opts).
