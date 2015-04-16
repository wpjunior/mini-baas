-module(item_schemas_resource_handler).
-export([init/2]).
-define(ITEM_SCHEMA_COLLECTION, <<"item-schemas">>).


init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    [{collection_name, CollectionName}] = cowboy_req:bindings(Req),

    handle(Method, CollectionName, Req, Opts).


handle(<<"GET">>, CollectionName, Req, Opts)->
    [MongoConnection] = Opts,

    case database:find_by_id(MongoConnection, ?ITEM_SCHEMA_COLLECTION, CollectionName) of
        {ok, Document} ->
            JsonBody = schema_object:to_json(Document),
            responses:json_success(Req, Opts, JsonBody);

        {not_found, _} ->
            responses:not_found(Req, Opts)
    end;


handle(<<"PUT">>, CollectionName, Req, Opts)->
    {ok, Body, _} = cowboy_req:body(Req),
    [MongoConnection] = Opts,

    case schema_object:from_json(Body) of
        {ok, Attributes} ->
            case database:update_attributes(MongoConnection, ?ITEM_SCHEMA_COLLECTION, CollectionName, Attributes) of
                {ok, Document} ->
                    JsonBody = schema_object:to_json(Document),
                    responses:json_success(Req, Opts, JsonBody);

                {not_found, _} ->
                    responses:not_found(Req, Opts)

            end;

        {invalid_json, _} ->
            responses:invalid_json(Req, Opts)
    end;


handle(<<"DELETE">>, CollectionName, Req, Opts) ->
    [MongoConnection] = Opts,
    case database:delete_by_id(MongoConnection, ?ITEM_SCHEMA_COLLECTION, CollectionName) of
        ok ->
            responses:no_content(Req, Opts);
        not_found ->
            responses:not_found(Req, Opts)
    end;


handle(_, _, Req, Opts)->
    responses:method_not_allowed(Req, Opts).