-module(item_schemas_collection_handler).
-export([init/2]).


init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    handle(Method, Req, Opts).


handle(<<"GET">>, Req, Opts) ->
    Filter = filter_builder:build_from_req(Req),
    JsonBody = schemas:find(Filter),
    responses:json_success(Req, Opts, JsonBody);


handle(<<"POST">>, Req, Opts) ->
    {ok, Body, _} = cowboy_req:body(Req),

    case resource_object:from_string(Body) of
        {ok, JsonDocument} ->
            JsonBody = schemas:create(JsonDocument),
            responses:json_created(Req, Opts, JsonBody);

        invalid_json ->
            responses:invalid_json(Req, Opts)
    end;


handle(_, Req, Opts)->
    responses:method_not_allowed(Req, Opts).
