-module(filter_builder).
-export([build_from_querystring/1, where/1, per_page/1, page/1]).

-define(PER_PAGE_DEFAULT, 10).
-define(PER_PAGE_MAX, 1000).

-record(filter, {
    where, per_page, page
}).

build_from_querystring(QueryString) ->
    ParsedQueryString = qsp:decode(QueryString),
    FilterParam = maps:get(<<"filter">>, ParsedQueryString, #{}),

    WhereMap = maps:get(<<"where">>, FilterParam, #{}),
    Where = map_to_bson(WhereMap),

    PerPage = extract_per_page(FilterParam, ?PER_PAGE_DEFAULT),
    Page = extract_page(FilterParam),

    #filter{where=Where, per_page=PerPage, page=Page}.

where(Filter) ->
    Filter#filter.where.

per_page(Filter) ->
    Filter#filter.per_page.

page(Filter) ->
    Filter#filter.page.

map_to_bson(Map) ->
    bson:document([{binary_to_atom(Key, unicode), Value} || {Key, Value} <- maps:to_list(Map)]).

extract_per_page(FilterParam, Default)->
    try binary_to_integer(maps:get(<<"perPage">>, FilterParam)) of
        PerPage ->
            min(PerPage, ?PER_PAGE_MAX)
    catch
        error:_ ->
            Default
    end.

extract_page(FilterParam)->
    try binary_to_integer(maps:get(<<"page">>, FilterParam)) of
        Page ->
            Page
    catch
        error:_ ->
            1
    end.
