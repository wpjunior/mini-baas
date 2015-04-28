-module(mini_baas_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
    application:start (bson),
    application:start (mongodb),

    {ok, MongoConnection} = mongo:connect (<<"test">>),
    {ok, _} = schema_service:start_link(MongoConnection),

    Dispatch = cowboy_router:compile([
        {'_', [
               {"/api/item-schemas", item_schemas_collection_handler, [MongoConnection]},
               {"/api/item-schemas/:collection_name", item_schemas_resource_handler, [MongoConnection]},
               {"/api/:collection_name", collection_handler, [MongoConnection]},
               {"/api/:collection_name/:id", resource_handler, [MongoConnection]}
              ]}
       ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
        {env, [{dispatch, Dispatch}]},

        {middlewares, [
                       cowboy_router,
                       collection_name_middleware,
                       cowboy_handler
                      ]}
    ]),
    mini_baas_sup:start_link().

stop(_State) ->
    ok.
