-module(database).

-export([find_by_id/3, exists/3, delete_by_id/3, update_attributes/4, insert/3,
         find/3]).

find_by_id(MongoConnection, CollectionName, Id) ->
    case mongo:find_one(MongoConnection, CollectionName, {'_id', Id}) of
        {Document} ->
            {ok, Document};
        {} ->
            {not_found, undefined}
    end.

exists(MongoConnection, CollectionName, Id) ->
    mongo:count(MongoConnection, CollectionName, {'_id', Id}, 1) == 1.

delete_by_id(MongoConnection, CollectionName, Id) ->
    case exists(MongoConnection, CollectionName, Id) of
        true ->
            mongo:delete_one(MongoConnection, CollectionName, {'_id', Id});

        false ->
            not_found
    end.

update_attributes(MongoConnection, CollectionName, Id, Attributes) ->
    case find_by_id(MongoConnection, CollectionName, Id) of
        {ok, Document} ->
            UpdateCommand = {'$set', Attributes},
            ok = mongo:update(MongoConnection, CollectionName, {'_id', Id}, UpdateCommand),

            UpdatedDocument = bson:merge(Attributes, Document),
            {ok, UpdatedDocument};

        {not_found, undefined} ->
            {not_found, undefined}
    end.

insert(MongoConnection, CollectionName, Resource) ->
    ResourceWithPrimaryKey = insert_primary_key_if_necessary(Resource),
    mongo:insert(MongoConnection, CollectionName, [ResourceWithPrimaryKey]),
    ResourceWithPrimaryKey.

insert_primary_key_if_necessary(Resource) ->
    %% TODO: This function can be improved, the method looks like assign_id method at erlang driver, see more:
    %% https://github.com/comtihon/mongodb-erlang/blob/master/src/api/mongo.erl#L215

    case bson:lookup('_id', Resource, undefined) of
        undefined ->
            PrimaryKey = erlang:list_to_binary(uuid:to_string(uuid:uuid4())),
            bson:update('_id', PrimaryKey, Resource);
        _ ->
            Resource
    end.

find(MongoConnection, CollectionName, Where) ->
    Cursor = mongo:find(MongoConnection, CollectionName, Where),
    Result = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Result.
