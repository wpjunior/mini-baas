-module(test_schema_service).
-include_lib("eunit/include/eunit.hrl").

init_connection() ->
    application:start(bson),
    application:start(mongodb),
    database:init(),
    schema_service:start_link(),
    ok.

end_connection() ->
    application:stop(mongodb),
    application:stop(bson),
    database:stop(),
    schema_service:stop(),
    ok.

no_exists_test() ->
    init_connection(),
    Exists = schema_service:schema_is_found(<<"test">>),
    ?assertEqual(Exists, false),
    end_connection().

exists_test() ->
    init_connection(),
    database:insert(<<"item-schemas">>, {'_id', <<"test-exist">>, 'properties', {}}),
    schema_service:reset_cache(),
    Exists = schema_service:schema_is_found(<<"test-exist">>),
    ?assertEqual(Exists, true),
    ok = database:delete_by_id(<<"item-schemas">>, <<"test-exist">>),
    end_connection().

validation_ok_test() ->
    init_connection(),
    Props = {'name', {'type', <<"string">>}},
    database:insert(<<"item-schemas">>, {'_id', <<"test-exist">>, 'properties', Props}),
    schema_service:reset_cache(),
    Valid = schema_service:validate(<<"test-exist">>, {[{<<"name">>, <<"my name">>}]}),
    ?assertEqual(Valid, valid),
    ok = database:delete_by_id(<<"item-schemas">>, <<"test-exist">>),
    end_connection().

validation_error_test() ->
    init_connection(),
    Props = {'name', {'type', <<"string">>}},
    database:insert(<<"item-schemas">>, {'_id', <<"test-exist">>, 'properties', Props, 'type', <<"object">>}),
    schema_service:reset_cache(),
    {invalid, Errors} = schema_service:validate(<<"test-exist">>, {[{<<"name">>, 10}]}),
    ok = database:delete_by_id(<<"item-schemas">>, <<"test-exist">>),
    end_connection().

humanize_validation_errors_test() ->
    Schema = undefined,
    Value = undefined,

    ErrorPath1 = [<<"foo">>, <<"subfoo">>],
    Errors1 = [{data_invalid, Schema, wrong_type, Value, ErrorPath1}],

    ?assertEqual([{<<"/foo/subfoo">>, <<"Wrong type">>}],
                 schema_service:humanize_validation_errors(Errors1)),

    ErrorPath2 = [<<"foo">>, 0],
    Errors2 = [{data_invalid, Schema, wrong_type, Value, ErrorPath2}],

    ?assertEqual([{<<"/foo/0">>, <<"Wrong type">>}],
                 schema_service:humanize_validation_errors(Errors2)),

    ErrorPath3 = [<<"foo">>, 0, <<"bar">>],
    Errors3 = [{data_invalid, Schema, wrong_type, Value, ErrorPath3}],

    ?assertEqual([{<<"/foo/0/bar">>, <<"Wrong type">>}],
                 schema_service:humanize_validation_errors(Errors3)),

    ErrorPath4 = [<<"foo">>, 0, <<"bar">>],
    ErrorPath5 = [<<"foo">>, <<"miga">>],

    Errors4 = [{data_invalid, Schema, wrong_type, Value, ErrorPath4},
               {data_invalid, Schema, wrong_type, Value, ErrorPath5}],

    ?assertEqual([{<<"/foo/0/bar">>, <<"Wrong type">>}, {<<"/foo/miga">>, <<"Wrong type">>}],
                 schema_service:humanize_validation_errors(Errors4)),

    ?assertEqual([], schema_service:humanize_validation_errors([])).
