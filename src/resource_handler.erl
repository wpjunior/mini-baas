-module(resource_handler).

-export([init/2]).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    [{id, Id}, {collection_name, CollectionName}] = cowboy_req:bindings(Req),

    handle(Method, CollectionName, Id, Req, Opts).

handle(<<"GET">>, CollectionName, Id, Req, Opts)->
    case resources:find_by_id(CollectionName, Id) of
        {ok, JsonBody} ->
            responses:json_success(Req, Opts, JsonBody);

        not_found ->
            responses:not_found(Req, Opts)
    end;

handle(<<"PUT">>, CollectionName, Id, Req, Opts)->
    {ok, Body, _} = cowboy_req:body(Req),

    case resource_object:from_string(Body) of
        {ok, JsonAttributes} ->
            case resources:update_attributes(CollectionName, Id, JsonAttributes) of
                {ok, JsonBody} ->
                    responses:json_success(Req, Opts, JsonBody);

                {invalid, _Errors} ->
                    responses:invalid_json(Req, Opts);

                not_found ->
                    responses:not_found(Req, Opts)
            end;

        invalid_json ->
            responses:invalid_json(Req, Opts)
    end;

handle(<<"DELETE">>, CollectionName, Id, Req, Opts) ->
    case database:delete_by_id(CollectionName, Id) of
        ok ->
            responses:no_content(Req, Opts);
        not_found ->
            responses:not_found(Req, Opts)
    end;

handle(_, _, _, Req, Opts)->
    responses:method_not_allowed(Req, Opts).
