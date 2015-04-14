-module(responses).
-export([not_found/2, invalid_json/2, method_not_allowed/2]).

not_found(Req, Opts) ->
    Resp = cowboy_req:reply(404, Req),
    {ok, Resp, Opts}.

invalid_json(Req, Opts) ->
    Resp = cowboy_req:reply(422, Req),
    {ok, Resp, Opts}.

method_not_allowed(Req, Opts) ->
    Resp = cowboy_req:reply(405, Req),
    {ok, Resp, Opts}.
