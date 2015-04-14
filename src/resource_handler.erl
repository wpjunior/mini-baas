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

handle(<<"PUT">>,  MongoConnection, CollectionName, Pk, Req, Opts)->
    {ok, Body, _} = cowboy_req:body(Req),

    case resource_object:from_json(Body) of
        {ok, UpdateAttributes} ->
            put_response_from_mongo(MongoConnection, CollectionName, Pk, Req, UpdateAttributes, Opts);
        {invalid_json, _} ->
            build_422_response(Req, Opts)
    end;

handle(<<"DELETE">>, MongoConnection, CollectionName, Pk, Req, Opts)->
    Count = mongo:count(MongoConnection, CollectionName, {'_id', Pk}, 1),
    if
        Count == 1 ->
            ok = mongo:delete_one(MongoConnection, CollectionName, {'_id', Pk}),
            Resp = cowboy_req:reply(204, Req),
            {ok, Resp, Opts};

        true ->
            build_404_response(Req, Opts)

    end;

handle(_, _, _, _, Req, Opts)->
    Resp = cowboy_req:reply(405, Req),
    {ok, Resp, Opts}.

get_response_from_mongo({}, Req, Opts) ->
    build_404_response(Req, Opts);

get_response_from_mongo({Document}, Req, Opts) ->
    JsonResp = jiffy:encode(resource_object:to_json(Document)),
    Resp = cowboy_req:reply(200, [
                                  {<<"content-type">>, <<"application/json">>}
                                 ], JsonResp, Req),
    {ok, Resp, Opts}.

put_response_from_mongo(MongoConnection, CollectionName, Pk, Req, UpdateAttributes, Opts) ->
    FindOneResp = mongo:find_one(MongoConnection, CollectionName, {'_id', Pk}),
    if
        erlang:tuple_size(FindOneResp) == 1 ->
            {CurrentDocument} = FindOneResp,
            UpdateCommand = {'$set', UpdateAttributes},
            ok = mongo:update(MongoConnection, CollectionName, {'_id', Pk}, UpdateCommand),

            UpdatedDocument = bson:merge(UpdateAttributes, CurrentDocument),
            io:format("UpdatedDocument: ~p\n", [UpdatedDocument]),
            JsonResp = jiffy:encode(resource_object:to_json(UpdatedDocument)),
            Resp = cowboy_req:reply(200, [
                                          {<<"content-type">>, <<"application/json">>}
                                         ], JsonResp, Req),
            {ok, Resp, Opts};

        true ->
            build_404_response(Req, Opts)

    end.

build_404_response(Req, Opts) ->
    Resp = cowboy_req:reply(404, Req),
    {ok, Resp, Opts}.

build_422_response(Req, Opts) ->
    Resp = cowboy_req:reply(422, Req),
    {ok, Resp, Opts}.
