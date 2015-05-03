-module(collection_handler).
-export([init/2]).


init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    [{collection_name, CollectionName}] = cowboy_req:bindings(Req),
    handle(Method, CollectionName, Req, Opts).

handle(<<"GET">>, CollectionName, Req, Opts) ->
    Filter = filter_builder:build_from_req(Req),
    Where = filter_builder:where(Filter),
    Result = database_service:find(CollectionName, Where),
    JsonBody = resource_object:to_json_list(Result),
    responses:json_success(Req, Opts, JsonBody);

handle(<<"POST">>, CollectionName, Req, Opts) ->
    {ok, Body, _} = cowboy_req:body(Req),

    case resource_object:from_string(Body) of
        {ok, JsonDocument} ->
            Resource = resource_object:from_json(JsonDocument),
            ResourceWithPrimaryKey = database_service:insert(CollectionName, Resource),
            JsonBody = resource_object:to_json(ResourceWithPrimaryKey),
            responses:json_created(Req, Opts, JsonBody);

        invalid_json ->
            responses:invalid_json(Req, Opts)
    end;

handle(_, _, Req, Opts)->
    responses:method_not_allowed(Req, Opts).
