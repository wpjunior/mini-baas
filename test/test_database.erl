-module(test_database).
-include_lib("eunit/include/eunit.hrl").

init_connection() ->
    application:start(bson),
    application:start(mongodb),
    database:init(),
    ok.

end_connection() ->
    application:stop(mongodb),
    application:stop(bson),
    database:stop(),
    ok.

exists_test() ->
    init_connection(),
    Exists = database:exists(<<"teste">>, <<"undefined">>),
    ?assertEqual(Exists, false),
    end_connection().

find_by_id_test() ->
    init_connection(),
    Response = database:find_by_id(<<"teste">>, <<"undefined">>),
    ?assertEqual(Response, not_found),
    end_connection().
