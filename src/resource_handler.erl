%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(resource_handler).

-export([init/2]).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    [{pk, Pk}, {collection_name, CollectionName}] = cowboy_req:bindings(Req),
    [MongoConnection] = Opts,

    handle(Method, MongoConnection, CollectionName, Pk, Req, Opts).

handle(<<"GET">>,  MongoConnection, CollectionName, Pk, Req, Opts)->
    MongoResp = mongo:find_one(MongoConnection, CollectionName, {'_id', Pk}),
    get_response_from_mongo(MongoResp, Req, Opts);

handle(<<"DELETE">>, RedisConnection, _, _, Req, Opts)->
    {ok, RedisResp} = eredis:q(RedisConnection, ["GET", "DeleteResource"]),
    Resp = cowboy_req:reply(200, [
                                  {<<"content-type">>, <<"application/json">>}
                                 ], RedisResp, Req),
    {ok, Resp, Opts};

handle(_, _, _, _, Req, Opts)->
    Resp = cowboy_req:reply(405, Req),
    {ok, Resp, Opts}.

get_response_from_mongo({}, Req, Opts) ->
    Resp = cowboy_req:reply(404, Req),
    {ok, Resp, Opts};

get_response_from_mongo({Document}, Req, Opts) ->
    JsonResp = jiffy:encode({to_proplist(Document)}),
    Resp = cowboy_req:reply(200, [
                                  {<<"content-type">>, <<"application/json">>}
                                 ], JsonResp, Req),
    {ok, Resp, Opts}.

to_proplist([]) ->
	[];
to_proplist({}) ->
	[];
to_proplist({Tuple}) ->
	to_proplist(Tuple);
to_proplist(Tuple) ->
	Size = tuple_size(Tuple),
	[{element(X, Tuple), element(X + 1, Tuple)} || X <- lists:seq(1, Size, 2)].
