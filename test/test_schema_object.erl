-module(test_schema_object).
-include_lib("eunit/include/eunit.hrl").


to_json_test() ->
    Json = {[{<<"collectionName">>, <<"123">>}, {<<"$schema">>, <<"ref">>}]},
    ?assertEqual(schema_object:to_json({'_id', <<"123">>, '%24schema', <<"ref">>}), Json).


to_json_without_schema_test() ->
    Json = {[{<<"collectionName">>, <<"123">>}]},
    ?assertEqual(schema_object:to_json({'_id', <<"123">>}), Json).


from_json_test() ->
    Json = <<"{\"collectionName\": \"123\", \"$schema\": \"ref\"}">>,
    {ReturnAtom, BsonDocument} = schema_object:from_json(Json),
    ?assertEqual(ok, ReturnAtom),
    ?assertEqual({'_id', <<"123">>, '%24schema', <<"ref">>}, BsonDocument).


to_json_list_test() ->
    Input = [{'_id', <<"123">>, '%24schema', <<"ref">>},
             {'_id', <<"321">>, '%24schema', <<"ref">>}],

    Json = {[
             {<<"items">>, [
                            {[{<<"collectionName">>, <<"123">>}, {<<"$schema">>, <<"ref">>}]},
                            {[{<<"collectionName">>, <<"321">>}, {<<"$schema">>, <<"ref">>}]}
                           ]}
            ]},
    Output = schema_object:to_json_list(Input),
    ?assertEqual(Output, Json).
