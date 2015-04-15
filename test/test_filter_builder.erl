-module(test_filter_builder).
-include_lib("eunit/include/eunit.hrl").

simple_where_test() ->
    Filter = filter_builder:build_from_querystring(<<"filter[where][name]=10">>),
    ?assertEqual(filter_builder:where(Filter), {name, <<"10">>}).

two_condition_where_test() ->
    Filter = filter_builder:build_from_querystring(<<"filter[where][name]=10&filter[where][age]=18">>),
    ?assertEqual(filter_builder:where(Filter), {age, <<"18">>, name, <<"10">>}).

default_per_page_test() ->
    Filter = filter_builder:build_from_querystring(<<"">>),
    ?assertEqual(filter_builder:per_page(Filter), 10).

per_page_test() ->
    Filter = filter_builder:build_from_querystring(<<"filter[perPage]=5">>),
    ?assertEqual(filter_builder:per_page(Filter), 5).

per_page_invalid_test() ->
    Filter = filter_builder:build_from_querystring(<<"filter[perPage]=100a">>),
    ?assertEqual(filter_builder:per_page(Filter), 10).

per_page_overflow_test() ->
    Filter = filter_builder:build_from_querystring(<<"filter[perPage]=1000000">>),
    ?assertEqual(filter_builder:per_page(Filter), 1000).

default_page_test() ->
    Filter = filter_builder:build_from_querystring(<<"">>),
    ?assertEqual(filter_builder:page(Filter), 1).

page_test() ->
    Filter = filter_builder:build_from_querystring(<<"filter[page]=5">>),
    ?assertEqual(filter_builder:page(Filter), 5).

page_invalid_test() ->
    Filter = filter_builder:build_from_querystring(<<"filter[page]=a1">>),
    ?assertEqual(filter_builder:page(Filter), 1).
