-module(collection_handler).
-export([init/2]).


init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    [{collection_name, CollectionName}] = cowboy_req:bindings(Req),
    handle(Method, CollectionName, Req, Opts).

handle(<<"GET">>, CollectionName, Req, Opts) ->
    Filter = filter_builder:build_from_req(Req),
    JsonBody = resources:find(CollectionName, Filter),
    responses:json_success(Req, Opts, JsonBody);

handle(<<"POST">>, CollectionName, Req, Opts) ->
    {ok, Body, _} = cowboy_req:body(Req),

    case resource_object:from_string(Body) of
        {ok, JsonDocument} ->
            case resources:create(CollectionName, JsonDocument) of
                {ok, JsonBody} ->
                    responses:json_created(Req, Opts, JsonBody);

                {invalid, Errors} ->
                    responses:invalid_json(Req, Opts, Errors)
            end;

        invalid_json ->
            responses:invalid_json(Req, Opts)
    end;

handle(_, _, Req, Opts)->
    responses:method_not_allowed(Req, Opts).
