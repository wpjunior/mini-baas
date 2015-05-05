-module(item_schemas_collection_handler).
-export([init/2]).
-define(ITEM_SCHEMA_COLLECTION, <<"item-schemas">>).


init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    handle(Method, Req, Opts).


handle(<<"GET">>, Req, Opts) ->
    Filter = filter_builder:build_from_req(Req),
    Where = filter_builder:where(Filter),

    Result = database:find(?ITEM_SCHEMA_COLLECTION, Where),
    JsonBody = schema_object:to_json_list(Result),

    responses:json_success(Req, Opts, JsonBody);


handle(<<"POST">>, Req, Opts) ->
    {ok, Body, _} = cowboy_req:body(Req),

    case resource_object:from_string(Body) of
        {ok, JsonDocument} ->
            Schema = schema_object:from_json(JsonDocument),
            database:insert(?ITEM_SCHEMA_COLLECTION, Schema),
            JsonBody = schema_object:to_json(Schema),
            responses:json_created(Req, Opts, JsonBody);

        invalid_json ->
            responses:invalid_json(Req, Opts)
    end;


handle(_, Req, Opts)->
    responses:method_not_allowed(Req, Opts).
