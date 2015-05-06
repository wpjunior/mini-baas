-module(resources).
-export([create/2, update_attributes/3, find_by_id/2, delete_by_id/2, find/2]).

create(CollectionName, JsonDocument) ->
    JsonDocument2 = before_create(JsonDocument),

    case schema_service:validate(CollectionName, JsonDocument2) of
        valid ->
            BsonResource = resource_object:from_json(JsonDocument2),
            database:insert(CollectionName, BsonResource),
            {ok, JsonDocument2};

        {invalid, _Errors}->
            {invalid, []}
    end.

update_attributes(CollectionName, Id, Attributes) ->
    case find_by_id(CollectionName, Id) of
        {ok, Resource} ->
            Attributes2 = before_update(Attributes),
            ResourceUpdated = overwrite_properties(Attributes2, Resource),

            case schema_service:validate(CollectionName, ResourceUpdated) of
                valid ->
                    BsonAttributes = resource_object:from_json(Attributes2),
                    ok = database:atomic_update(CollectionName, Id, BsonAttributes),
                    {ok, ResourceUpdated};

                {invalid, _Errors}->
                    {invalid, []}
            end;

        not_found ->
            not_found
    end.

find_by_id(CollectionName, Id) ->
    case database:find_by_id(CollectionName, Id) of
        {ok, BsonDocument} ->
            JsonBody = resource_object:to_json(BsonDocument),
            {ok, JsonBody};

        not_found ->
            not_found
    end.

delete_by_id(CollectionName, Id) ->
    database:delete_by_id(CollectionName, Id).

find(CollectionName, Filter) ->
    Where = filter_builder:where(Filter),
    Result = database:find(CollectionName, Where),
    resource_object:to_json_list(Result).

before_create(Resource) ->
    Now = iso8601:format(now()),
    VersionId = new_identifier(),
    MetaData = {[{<<"created">>, Now}, {<<"modified">>, Now}, {<<"versionId">>, VersionId}]},
    ResourceWithMetaData = overwrite_properties(MetaData, Resource),
    insert_primary_key_if_necessary(ResourceWithMetaData).

before_update(Attributes) ->
    Now = iso8601:format(now()),
    VersionId = new_identifier(),
    MetaData = {[{<<"modified">>, Now}, {<<"versionId">>, VersionId}]},
    overwrite_properties(MetaData, Attributes).

overwrite_properties({Properties}, {AttrList}) ->
    {lists:foldl(fun overwrite_property/2, AttrList, Properties)}.

overwrite_property({Key, Value}, AttrList) ->
    lists:keystore(Key, 1, AttrList, {Key, Value}).

new_identifier() ->
    list_to_binary(uuid:to_string(uuid:uuid4())).

insert_primary_key_if_necessary({AttrList}) ->
    case lists:keysearch(<<"id">>, 1, AttrList) of
        {value, _Tuple} ->
            {AttrList}; %% No changes
        false ->
            {[{<<"id">>, new_identifier()}] ++ AttrList}
    end.
