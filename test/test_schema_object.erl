-module(test_schema_object).
-include_lib("eunit/include/eunit.hrl").

to_json_sanitize_test() ->
    Json = <<"{\"collectionName\":\"123\",\"$schema\":\"ref\"}">>,
    ?assertEqual(schema_object:to_json({'_id', <<"123">>, '%24schema', <<"ref">>}), Json).

from_json_one_depth_test() ->
    Json = <<"{\"collectionName\": \"123\", \"$schema\": \"ref\"}">>,
    {ReturnAtom, BsonDocument} = schema_object:from_json(Json),
    ?assertEqual(ok, ReturnAtom),
    ?assertEqual({'_id', <<"123">>, '%24schema', <<"ref">>}, BsonDocument).
