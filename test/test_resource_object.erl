-module(test_resource_object).
-include_lib("eunit/include/eunit.hrl").

to_json_one_depth_test() ->
    Json = {[{<<"id">>, <<"123">>}, {<<"name">>, <<"wilson">>}]},
    ?assertEqual(resource_object:to_json({'_id', <<"123">>, name, <<"wilson">>}), Json).

to_json_two_depth_test() ->
    Input = {'_id', <<"123">>, name, <<"wilson">>, second, {field1, <<"321">>, field2, 3}},
    Json = {[
             {<<"id">>, <<"123">>},
             {<<"name">>, <<"wilson">>},
             {<<"second">>, {[
                              {<<"field1">>, <<"321">>},
                              {<<"field2">>, 3}
                             ]
                 }
             }
            ]},
    ?assertEqual(resource_object:to_json(Input), Json).

to_json_one_depth_with_custom_primary_key_test() ->
    Json = {[{<<"slug">>, <<"123">>}, {<<"name">>, <<"wilson">>}]},
    ?assertEqual(resource_object:to_json({'_id', <<"123">>, name, <<"wilson">>}, slug), Json).

to_json_two_depth_with_custom_primary_key_test() ->
    Input = {'_id', <<"123">>, second, {field1, <<"321">>}},
    Json = {[
             {<<"customId">>, <<"123">>},
             {<<"second">>, {[
                              {<<"field1">>, <<"321">>}
                             ]}}
            ]},
    ?assertEqual(resource_object:to_json(Input, customId), Json).

to_json_list_test() ->
    Input = [{'_id', <<"123">>, name, <<"wilson">>},
             {'_id', <<"321">>, name, <<"tayza">>}],

    Json = {[
             {<<"items">>, [
                            {[{<<"id">>, <<"123">>}, {<<"name">>, <<"wilson">>}]},
                            {[{<<"id">>, <<"321">>}, {<<"name">>, <<"tayza">>}]}
                           ]}
            ]},
    Output = resource_object:to_json_list(Input),

    ?assertEqual(Output, Json).

from_json_one_depth_test() ->
    Text = <<"{\"id\": \"123\", \"name\": \"wilson\"}">>,
    {ok, Json} = resource_object:from_string(Text),
    BsonDocument = resource_object:from_json(Json),
    ?assertEqual({'_id', <<"123">>, name, <<"wilson">>}, BsonDocument).

from_json_primary_key_missing_test() ->
    Text = <<"{\"name\": \"wilson\"}">>,
    {ok, Json} = resource_object:from_string(Text),
    BsonDocument = resource_object:from_json(Json),
    ?assertEqual({name, <<"wilson">>}, BsonDocument).

from_json_two_depth_test() ->
    Text = <<"{\"id\": \"123\", \"name\": \"wilson\", \"second\": {\"field1\": \"321\", \"field2\": 3}}">>,
    {ok, Json} = resource_object:from_string(Text),
    ExpectedEmbeddedField = {field1, <<"321">>, field2, 3},

    BsonDocument = resource_object:from_json(Json),
    ?assertEqual({'_id', <<"123">>, name, <<"wilson">>, second, ExpectedEmbeddedField},
                 BsonDocument).

from_json_one_depth_with_custom_primary_key_test() ->
    Text = <<"{\"slug\": \"123\", \"name\": \"wilson\"}">>,
    {ok, Json} = resource_object:from_string(Text),
    BsonDocument = resource_object:from_json(Json, slug),
    ?assertEqual({'_id', <<"123">>, name, <<"wilson">>}, BsonDocument).

from_json_two_depth_with_custom_primary_key_test() ->
    Text = <<"{\"customId\": \"123\", \"name\": \"wilson\", \"second\": {\"field1\": \"321\", \"field2\": 3}}">>,
    {ok, Json} = resource_object:from_string(Text),
    ExpectedEmbeddedField = {field1, <<"321">>, field2, 3},

    BsonDocument = resource_object:from_json(Json, customId),
    ?assertEqual({'_id', <<"123">>, name, <<"wilson">>, second, ExpectedEmbeddedField},
                 BsonDocument).

from_string_invalid_json_test() ->
    Json = <<"{}..A..B..">>,
    ReturnAtom = resource_object:from_string(Json),
    ?assertEqual(ReturnAtom, invalid_json).
