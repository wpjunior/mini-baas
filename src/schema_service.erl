-module(schema_service).
-behaviour(gen_server).

-export([start_link/0, stop/0, terminate/2]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([schema_is_found/1, validate/2, reset_cache/0]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    Schemas = gb_trees:empty(),
    {ok, Schemas}.

handle_call({schema_is_found, CollectionName}, _From, Schemas) ->
    {Found, NewSchemas} = handle_schema_is_found(memory, CollectionName, Schemas),
    {reply, Found, NewSchemas};

handle_call({validate, CollectionName, Resource}, _From, Schemas) ->
    {Valid, NewSchemas} = handle_validate(CollectionName, Resource, Schemas),
    {reply, Valid, NewSchemas}.

schema_is_found(CollectionName) ->
    gen_server:call(?MODULE, {schema_is_found, CollectionName}).

validate(CollectionName, Resource) ->
    gen_server:call(?MODULE, {validate, CollectionName, Resource}).

stop() ->
    gen_server:cast(?MODULE, stop).

reset_cache() ->
    gen_server:cast(?MODULE, reset_cache).

handle_cast(stop, Schemas) ->
    {stop, normal, Schemas};

handle_cast(reset_cache, _Schemas) ->
    {noreply, gb_trees:empty()}.

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
    case schemas:find_by_collection_name(CollectionName) of
        {ok, Schema} ->
            NewSchemas = gb_trees:enter(CollectionName, Schema, Schemas),
            {true, NewSchemas};

        not_found ->
            {false, Schemas}
    end.

handle_get_schema(memory, CollectionName, Schemas) ->
    case gb_trees:lookup(CollectionName, Schemas) of
        {value, Schema} ->
            {Schema, Schemas};

        none ->
            handle_get_schema(database, CollectionName, Schemas)
    end;

handle_get_schema(database, CollectionName, Schemas) ->
    case schemas:find_by_collection_name(CollectionName) of
        {ok, Schema} ->
            NewSchemas = gb_trees:enter(CollectionName, Schema, Schemas),
            {Schema, NewSchemas};

        not_found ->
            not_found
    end.

handle_validate(CollectionName, Resource, Schemas) ->
    case handle_get_schema(memory, CollectionName, Schemas) of
        not_found ->
            {false, Schemas};

        {Schema, NewSchemas} ->
            Valid = handle_validate_schema(Schema, Resource),
            {Valid, NewSchemas}
    end.

handle_validate_schema(Schema, Resource) ->
    try jesse_schema_validator:validate(Schema, Resource, []) of
         {ok, _} ->
            valid
    catch
        N ->
            {invalid, N}
    end.
