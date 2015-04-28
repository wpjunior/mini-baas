-module(schema_service).
-behaviour(gen_server).

-export([start_link/1, stop/0, terminate/2]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([schema_is_found/1]).

-define(ITEM_SCHEMA_COLLECTION, <<"item-schemas">>).


-record(service_state, {
    mongo_connection,
    schemas
}).

start_link(MongoConnection) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, MongoConnection, []).

init(MongoConnection) ->
    Schemas = gb_trees:empty(),
    State = #service_state{mongo_connection=MongoConnection, schemas=Schemas},
    {ok, State}.

handle_call({schema_is_found, CollectionName}, _From, State) ->
    case database_service:find_by_id(?ITEM_SCHEMA_COLLECTION, CollectionName) of
        {ok, _} ->
            {reply, true, State};
        not_found ->
            {reply, false, State}
    end.

schema_is_found(CollectionName) ->
    gen_server:call(?MODULE, {schema_is_found, CollectionName}).

stop() ->
    gen_server:cast(?MODULE, stop).

handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(normal, _) ->
    ok.
