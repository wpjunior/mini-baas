-module(resources).
-export([create/2, update_attributes/3, find_by_id/2, delete_by_id/2, find/2]).

create(CollectionName, JsonDocument) ->
    JsonDocument2 = before_create(JsonDocument),
    BsonResource = resource_object:from_json(JsonDocument2),
    database:insert(CollectionName, BsonResource),
    JsonDocument2.

update_attributes(CollectionName, Id, Attributes) ->
    Attributes2 = before_update(Attributes),
    BsonAttributes = resource_object:from_json(Attributes2),
    case database:update_attributes(CollectionName, Id, BsonAttributes) of
        {ok, BsonDocument} ->
            JsonBody = resource_object:to_json(BsonDocument),
            {ok, JsonBody};

        not_found ->
            not_found
    end.

find_by_id(CollectionName, Id) ->
    database:find_by_id(CollectionName, Id).

delete_by_id(CollectionName, Id) ->
    database:delete_by_id(CollectionName, Id).

find(CollectionName, Filter) ->
    Where = filter_builder:where(Filter),
    Result = database:find(CollectionName, Where),
    resource_object:to_json_list(Result).

before_create(Resource) ->
    Now = iso8601:format(now()),
    VersionId = new_identifier(),
    MetaData = [{<<"created">>, Now}, {<<"modified">>, Now}, {<<"versionId">>, VersionId}],
    ResourceWithMetaData = overwrite_properties(MetaData, Resource),
    insert_primary_key_if_necessary(ResourceWithMetaData).

before_update(Attributes) ->
    Now = iso8601:format(now()),
    VersionId = new_identifier(),
    MetaData = [{<<"modified">>, Now}, {<<"versionId">>, VersionId}],
    overwrite_properties(MetaData, Attributes).

overwrite_properties(Properties, {AttrList}) ->
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
