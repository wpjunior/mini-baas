-module(database).

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
    mongo:insert(?PID_ALIAS, CollectionName, [Resource]),
    Resource.

find(CollectionName, Where) ->
    Cursor = mongo:find(?PID_ALIAS, CollectionName, Where),
    Result = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Result.
