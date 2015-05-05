-module(resources).
-export([create/2, update_attributes/3, find_by_id/2, delete_by_id/2, find/2]).

create(CollectionName, JsonDocument) ->
    BsonResource = resource_object:from_json(JsonDocument),
    BsonResourceWithPrimaryKey = database:insert(CollectionName, BsonResource),
    resource_object:to_json(BsonResourceWithPrimaryKey).

update_attributes(CollectionName, Id, Attributes) ->
    BsonAttributes = resource_object:from_json(Attributes),
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

%% before_create(Resource) ->
%%     % create primary key if necessary
%%     % create version, modified, created
%%     ok.

%% before_update(Attributes) ->
%%     % update version, modified
%%     ok. % NewAttributes
