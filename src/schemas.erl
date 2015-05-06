-module(schemas).
-export([create/1, update_attributes/2, find_by_collection_name/1, delete_by_collection_name/1, find/1]).
-define(ITEM_SCHEMA_COLLECTION, <<"item-schemas">>).

create(JsonDocument) ->
    BsonResource = schema_object:from_json(JsonDocument),
    database:insert(?ITEM_SCHEMA_COLLECTION, BsonResource),
    JsonDocument.



find_by_collection_name(CollectionName) ->
    case database:find_by_id(?ITEM_SCHEMA_COLLECTION, CollectionName) of
        {ok, BsonDocument} ->
            JsonBody = schema_object:to_json(BsonDocument),
            {ok, JsonBody};

        not_found ->
            not_found
    end.

update_attributes(CollectionName, Attributes) ->
    BsonAttributes = resource_object:from_json(Attributes),
    case database:update_attributes(?ITEM_SCHEMA_COLLECTION, CollectionName, BsonAttributes) of
        {ok, BsonDocument} ->
            JsonBody = schema_object:to_json(BsonDocument),
            {ok, JsonBody};

        not_found ->
            not_found
    end.

find(Filter) ->
    Where = filter_builder:where(Filter),

    Result = database:find(?ITEM_SCHEMA_COLLECTION, Where),
    schema_object:to_json_list(Result).

delete_by_collection_name(CollectionName) ->
    database:delete_by_id(?ITEM_SCHEMA_COLLECTION, CollectionName).
