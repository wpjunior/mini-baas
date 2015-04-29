-module(test_schema_service).
-include_lib("eunit/include/eunit.hrl").

init_connection() ->
    application:start(bson),
    application:start(mongodb),
    database_service:init(),
    schema_service:start_link(),
    ok.

end_connection() ->
    application:stop(mongodb),
    application:stop(bson),
    database_service:stop(),
    schema_service:stop(),
    ok.

no_exists_test() ->
    init_connection(),
    Exists = schema_service:schema_is_found(<<"test">>),
    ?assertEqual(Exists, false),
    end_connection().

exists_test() ->
    init_connection(),
    database_service:insert(<<"item-schemas">>, {'_id', <<"test-exist">>, 'properties', {}}),
    Exists = schema_service:schema_is_found(<<"test-exist">>),
    ?assertEqual(Exists, true),
    ok = database_service:delete_by_id(<<"item-schemas">>, <<"test-exist">>),
    end_connection().
