-module(database_service).

-export([init/0, stop/0, exists/2, find_by_id/2, delete_by_id/2,
         update_attributes/3, insert/2, find/2]).

-define(PID_ALIAS, database_connection).

init() ->
    {ok, MongoConnection} = mongo:connect (<<"test">>),
    register(?PID_ALIAS, MongoConnection),
    {ok, MongoConnection}.

stop() ->
    Pid = whereis(?PID_ALIAS),
    mongo:disconnect(Pid).

exists(CollectionName, Id) ->
    mongo:count(?PID_ALIAS, CollectionName, {'_id', Id}, 1) == 1.

find_by_id(CollectionName, Id) ->
    case mongo:find_one(?PID_ALIAS, CollectionName, {'_id', Id}) of
        {Document} ->
            {ok, Document};
        {} ->
            not_found
    end.

delete_by_id(CollectionName, Id) ->
    case exists(CollectionName, Id) of
        true ->
            mongo:delete_one(?PID_ALIAS, CollectionName, {'_id', Id});

        false ->
            not_found
    end.

update_attributes(CollectionName, Id, Attributes) ->
    case find_by_id(CollectionName, Id) of
        {ok, Document} ->
            UpdateCommand = {'$set', Attributes},
            ok = mongo:update(?PID_ALIAS, CollectionName, {'_id', Id}, UpdateCommand),
            UpdatedDocument = bson:merge(Attributes, Document),
            {ok, UpdatedDocument};

        not_found ->
            not_found
    end.

insert(CollectionName, Resource) ->
    ResourceWithPrimaryKey = insert_primary_key_if_necessary(Resource),
    mongo:insert(?PID_ALIAS, CollectionName, [ResourceWithPrimaryKey]),
    ResourceWithPrimaryKey.

insert_primary_key_if_necessary(Resource) ->
    %% TODO: This function can be improved, the method looks like assign_id method at erlang driver, see more:
    %% https://github.com/comtihon/mongodb-erlang/blob/master/src/api/mongo.erl#L215

    case bson:lookup('_id', Resource, undefined) of
        undefined ->
            PrimaryKey = list_to_binary(uuid:to_string(uuid:uuid4())),
            bson:update('_id', PrimaryKey, Resource);
        _ ->
            Resource
    end.

find(CollectionName, Where) ->
    Cursor = mongo:find(?PID_ALIAS, CollectionName, Where),
    Result = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Result.
