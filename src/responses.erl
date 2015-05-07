-module(responses).
-export([not_found/2, invalid_json/2, invalid_json/3, method_not_allowed/2,
         no_content/2, json_success/3, json_created/3]).

not_found(Req, Opts) ->
    Resp = cowboy_req:reply(404, Req),
    {ok, Resp, Opts}.

invalid_json(Req, Opts) ->
    Resp = cowboy_req:reply(422, Req),
    {ok, Resp, Opts}.

invalid_json(Req, Opts, Errors) ->
    JsonBody = {[{<<"errors">>, {Errors}}]},
    Resp = cowboy_req:reply(422, [
        {<<"content-type">>, <<"application/json">>}
    ], jiffy:encode(JsonBody), Req),

    {ok, Resp, Opts}.

method_not_allowed(Req, Opts) ->
    Resp = cowboy_req:reply(405, Req),
    {ok, Resp, Opts}.

no_content(Req, Opts) ->
    Resp = cowboy_req:reply(204, Req),
    {ok, Resp, Opts}.

json_success(Req, Opts, JsonBody) ->
    Body = jiffy:encode(JsonBody),
    Resp = cowboy_req:reply(200, [
        {<<"content-type">>, <<"application/json">>}
    ], Body, Req),

    {ok, Resp, Opts}.

json_created(Req, Opts, JsonBody) ->
    Body = jiffy:encode(JsonBody),
    Resp = cowboy_req:reply(201, [
        {<<"content-type">>, <<"application/json">>}
    ], Body, Req),
    {ok, Resp, Opts}.
