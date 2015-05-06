-module(item_schemas_resource_handler).
-export([init/2]).
-define(ITEM_SCHEMA_COLLECTION, <<"item-schemas">>).


init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    [{collection_name, CollectionName}] = cowboy_req:bindings(Req),

    handle(Method, CollectionName, Req, Opts).


handle(<<"GET">>, CollectionName, Req, Opts)->
    case schemas:find_by_collection_name(CollectionName) of
        {ok, JsonBody} ->
            responses:json_success(Req, Opts, JsonBody);

        not_found ->
            responses:not_found(Req, Opts)
    end;


handle(<<"PUT">>, CollectionName, Req, Opts)->
    {ok, Body, _} = cowboy_req:body(Req),

    case resource_object:from_string(Body) of
        {ok, JsonAttributes} ->
            case schemas:update_attributes(CollectionName, JsonAttributes) of
                {ok, JsonBody} ->
                    responses:json_success(Req, Opts, JsonBody);

                not_found ->
                    responses:not_found(Req, Opts)
            end;

        invalid_json ->
            responses:invalid_json(Req, Opts)
    end;


handle(<<"DELETE">>, CollectionName, Req, Opts) ->
    case schemas:delete_by_collection_name(CollectionName) of
        ok ->
            responses:no_content(Req, Opts);
        not_found ->
            responses:not_found(Req, Opts)
    end;


handle(_, _, Req, Opts)->
    responses:method_not_allowed(Req, Opts).
