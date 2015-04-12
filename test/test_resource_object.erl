-module(test_resource_object).
-include_lib("eunit/include/eunit.hrl").

one_depth_test() ->
    ?assertEqual({[{id, "123"}, {name, "wilson"}]},
                 resource_object:from_bson({'_id', "123", name, "wilson"})).

two_depth_test() ->
    InputEmbeddedField = {field1, "321", field2, 3},
    ExpectedEmbeddedField = {[{field1, "321"}, {field2, 3}]},
    ?assertEqual({[{id, "123"}, {name, "wilson"}, {second, ExpectedEmbeddedField}]},
                 resource_object:from_bson({'_id', "123", name, "wilson",
                                            second, InputEmbeddedField})).

one_depth_with_custom_primary_key_test() ->
    ?assertEqual({[{slug, "123"}, {name, "wilson"}]},
                 resource_object:from_bson({'_id', "123", name, "wilson"}, slug)).

two_depth_with_custom_primary_key_test() ->
    InputEmbeddedField = {field1, "321", field2, 3},
    ExpectedEmbeddedField = {[{field1, "321"}, {field2, 3}]},
    ?assertEqual({[{customId, "123"}, {name, "wilson"}, {second, ExpectedEmbeddedField}]},
                 resource_object:from_bson({'_id', "123", name, "wilson",
                                            second, InputEmbeddedField}, customId)).
