-module(schema_service).
-behaviour(gen_server).

-export([start_link/0, stop/0, terminate/2]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([schema_is_found/1]).

-define(ITEM_SCHEMA_COLLECTION, <<"item-schemas">>).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    Schemas = gb_trees:empty(),
    {ok, Schemas}.

handle_call({schema_is_found, CollectionName}, _From, Schemas) ->
    {Found, NewSchemas} = handle_schema_is_found(memory, CollectionName, Schemas),
    {reply, Found, NewSchemas}.

schema_is_found(CollectionName) ->
    gen_server:call(?MODULE, {schema_is_found, CollectionName}).

stop() ->
    gen_server:cast(?MODULE, stop).

handle_cast(stop, Schemas) ->
    {stop, normal, Schemas}.

terminate(normal, _) ->
    ok.

handle_schema_is_found(memory, CollectionName, Schemas) ->
    case gb_trees:is_defined(CollectionName, Schemas) of
        true ->
            {true, Schemas};
        false ->
            handle_schema_is_found(database, CollectionName, Schemas)
    end;

handle_schema_is_found(database, CollectionName, Schemas) ->
    case database_service:find_by_id(?ITEM_SCHEMA_COLLECTION, CollectionName) of
        {ok, Document} ->
            NewSchemas = gb_trees:insert(CollectionName, Document, Schemas),
            {true, NewSchemas};

        not_found ->
            {false, Schemas}
    end.
