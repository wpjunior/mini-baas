-module(schema_service).
-behaviour(gen_server).

-export([start_link/0, stop/0, terminate/2]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([schema_is_found/1, validate/2, reset_cache/0, humanize_validation_errors/1]).

-define(DEFAULT_STORAGE, database).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    Schemas = gb_trees:empty(),
    {ok, Schemas}.

handle_call({schema_is_found, CollectionName}, _From, Schemas) ->
    {Found, NewSchemas} = handle_schema_is_found(?DEFAULT_STORAGE, CollectionName, Schemas),
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
    case handle_get_schema(?DEFAULT_STORAGE, CollectionName, Schemas) of
        not_found ->
            {false, Schemas};

        {Schema, NewSchemas} ->
            Valid = handle_validate_schema(Schema, Resource),
            {Valid, NewSchemas}
    end.

handle_validate_schema(Schema, Resource) ->
    try jesse_schema_validator:validate(Schema, Resource, [{allowed_errors, infinity}]) of
         {ok, _} ->
            valid
    catch
        Errors ->
            {invalid, humanize_validation_errors(Errors)}
    end.


humanize_validation_errors(ValidationErrors) ->
    lists:map(fun humanize_error/1, ValidationErrors).

humanize_error({data_invalid, _Schema, Type, _Value, Path}) ->
    {humanize_path(Path), validation_message(Type)}.

validation_message(missing_id_field) ->            <<"Missing id field">>;
validation_message(missing_required_property) ->   <<"Missing required property">>;
validation_message(missing_dependency) ->          <<"Missing dependency">>;
validation_message(no_match) ->                    <<"No match">>;
validation_message(no_extra_properties_allowed) -> <<"No extra properties allowed">>;
validation_message(no_extra_items_allowed) ->      <<"No extra items allowed">>;
validation_message(not_enought_items) ->           <<"Not enought items">>;
validation_message(not_allowed) ->                 <<"Not allowed">>;
validation_message(not_unique) ->                  <<"Not unique">>;
validation_message(not_in_range) ->                <<"Not in range">>;
validation_message(not_divisible) ->               <<"Not divisible">>;
validation_message(wrong_type) ->                  <<"Wrong type">>;
validation_message(wrong_type_items) ->            <<"Wrong type items">>;
validation_message(wrong_type_dependency) ->       <<"Wrong type dependency">>;
validation_message(wrong_size) ->                  <<"Wrong size">>;
validation_message(wrong_length) ->                <<"Wrong length">>;
validation_message(wrong_format) ->                <<"Wrong format">>;
validation_message(schema_unsupported) ->          <<"Schema unsupported">>;
validation_message(_) ->                           <<"Unhandled error">>.

-define(HUMAN_PATH_SEPARATOR, <<"/">>).

humanize_path([]) ->
    <<"/">>;

humanize_path([Path]) ->
    <<?HUMAN_PATH_SEPARATOR/binary, Path/binary>>;

humanize_path(Paths) ->
    Tail = lists:foldr(fun join_paths/2, <<>>, Paths),
    <<?HUMAN_PATH_SEPARATOR/binary, Tail/binary>>.

join_paths(Path, Acc) when is_number(Path) ->
    join_paths(integer_to_binary(Path), Acc);

join_paths(Path, Acc) ->
    if
        bit_size(Acc) > 0 -> <<Path/binary, ?HUMAN_PATH_SEPARATOR/binary, Acc/binary>>;
        true -> Path
    end.
