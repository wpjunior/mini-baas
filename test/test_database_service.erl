-module(test_database_service).
-include_lib("eunit/include/eunit.hrl").

init_connection() ->
    application:start(bson),
    application:start(mongodb),
    database_service:init(),
    ok.

end_connection() ->
    application:stop(mongodb),
    application:stop(bson),
    database_service:stop(),
    ok.

exists_test() ->
    init_connection(),
    Exists = database_service:exists(<<"teste">>, <<"undefined">>),
    ?assertEqual(Exists, false),
    end_connection().

find_by_id_test() ->
    init_connection(),
    Response = database_service:find_by_id(<<"teste">>, <<"undefined">>),
    ?assertEqual(Response, not_found),
    end_connection().
