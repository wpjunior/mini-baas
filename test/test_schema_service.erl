-module(test_schema_service).
-include_lib("eunit/include/eunit.hrl").

init_connection() ->
    application:start(bson),
    application:start(mongodb),
    {ok, MongoConnection} = mongo:connect (<<"mini-bass-test">>),
    {ok, SchemaServicePid} = schema_service:start_link(MongoConnection),
    {MongoConnection, SchemaServicePid}.

end_connection() ->
  application:stop(mongodb),
  application:stop(bson),
  schema_service:stop(),
  ok.

no_exists_test() ->
    {_, SchemaServicePid} = init_connection(),
    Exists = schema_service:schema_is_found(<<"test">>),
    ?assertEqual(Exists, false),
    end_connection().

exists_test() ->
    {MongoConnection, SchemaServicePid} = init_connection(),
    database:insert(MongoConnection, <<"item-schemas">>, {'_id', <<"test-exist">>, 'properties', {}}),
    Exists = schema_service:schema_is_found(<<"test-exist">>),
    ?assertEqual(Exists, true),
    end_connection().
