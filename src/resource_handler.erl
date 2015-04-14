-module(resource_handler).

-export([init/2]).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    [{id, Id}, {collection_name, CollectionName}] = cowboy_req:bindings(Req),
    [MongoConnection] = Opts,

    handle(Method, MongoConnection, CollectionName, Id, Req, Opts).

handle(<<"GET">>,  MongoConnection, CollectionName, Id, Req, Opts)->
    case database:find_by_id(MongoConnection, CollectionName, Id) of
        {ok, Document} ->
            JsonBody = jiffy:encode(resource_object:to_json(Document)),
            responses:json_success(Req, Opts, JsonBody);

        {not_found, _} ->
            responses:not_found(Req, Opts)
    end;

handle(<<"PUT">>, MongoConnection, CollectionName, Id, Req, Opts)->
    {ok, Body, _} = cowboy_req:body(Req),

    case resource_object:from_json(Body) of
        {ok, Attributes} ->
            case database:update_attributes(MongoConnection, CollectionName, Id, Attributes) of
                {ok, Document} ->
                    JsonBody = jiffy:encode(resource_object:to_json(Document)),
                    responses:json_success(Req, Opts, JsonBody);

                {not_found, _} ->
                    responses:not_found(Req, Opts)

            end;

        {invalid_json, _} ->
            responses:invalid_json(Req, Opts)
    end;

handle(<<"DELETE">>, MongoConnection, CollectionName, Id, Req, Opts) ->
    case database:delete_by_id(MongoConnection, CollectionName, Id) of
        ok ->
            responses:no_content(Req, Opts);
        not_found ->
            responses:not_found(Req, Opts)
    end;

handle(_, _, _, _, Req, Opts)->
    responses:method_not_allowed(Req, Opts).
