-module(test_resource_object).
-include_lib("eunit/include/eunit.hrl").

to_json_one_depth_test() ->
    ?assertEqual({[{id, "123"}, {name, "wilson"}]},
                 resource_object:to_json({'_id', "123", name, "wilson"})).

to_json_two_depth_test() ->
    InputEmbeddedField = {field1, "321", field2, 3},
    ExpectedEmbeddedField = {[{field1, "321"}, {field2, 3}]},
    ?assertEqual({[{id, "123"}, {name, "wilson"}, {second, ExpectedEmbeddedField}]},
                 resource_object:to_json({'_id', "123", name, "wilson",
                                            second, InputEmbeddedField})).

to_json_one_depth_with_custom_primary_key_test() ->
    ?assertEqual({[{slug, "123"}, {name, "wilson"}]},
                 resource_object:to_json({'_id', "123", name, "wilson"}, slug)).

to_json_two_depth_with_custom_primary_key_test() ->
    InputEmbeddedField = {field1, "321", field2, 3},
    ExpectedEmbeddedField = {[{field1, "321"}, {field2, 3}]},
    ?assertEqual({[{customId, "123"}, {name, "wilson"}, {second, ExpectedEmbeddedField}]},
                 resource_object:to_json({'_id', "123", name, "wilson",
                                            second, InputEmbeddedField}, customId)).

to_bson_one_depth_test() ->
    ?assertEqual({'_id', "123", name, "wilson"},
                 resource_object:to_bson({[{id, "123"}, {name, "wilson"}]})).

to_bson_two_depth_test() ->
    InputEmbeddedField = {[{field1, "321"}, {field2, 3}]},
    ExpectedEmbeddedField = {field1, "321", field2, 3},

    ?assertEqual({'_id', "123", name, "wilson", second, ExpectedEmbeddedField},
                 resource_object:to_bson({[{id, "123"}, {name, "wilson"}, {second, InputEmbeddedField}]})).

to_bson_one_depth_with_custom_primary_key_test() ->
    ?assertEqual({'_id', "123", name, "wilson"},
                 resource_object:to_bson({[{slug, "123"}, {name, "wilson"}]}, slug)).

to_bson_two_depth_with_custom_primary_key_test() ->
    InputEmbeddedField = {[{field1, "321"}, {field2, 3}]},
    ExpectedEmbeddedField = {field1, "321", field2, 3},

    ?assertEqual({'_id', "123", name, "wilson", second, ExpectedEmbeddedField},
                 resource_object:to_bson({[{customId, "123"}, {name, "wilson"}, {second, InputEmbeddedField}]}, customId)).
