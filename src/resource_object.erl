-module(resource_object).
-export([from_bson/1, from_bson/2,
         to_bson/1, to_bson/2]).

from_bson(BsonDocument) ->
    from_bson(BsonDocument, id).

from_bson(BsonDocument, PrimaryKeyName) ->
    {[{'_id', PrimaryKeyValue} | RemaingValues]} = to_proplist(BsonDocument),
    {[{PrimaryKeyName, PrimaryKeyValue} | RemaingValues]}.

to_bson(Document) ->
    to_bson(Document, id).

to_bson(Document, PrimaryKeyName) ->
    {AttrList} = Document,
    PrimaryKeyValue = proplists:get_value(PrimaryKeyName, AttrList),

    recursive_convert_to_bson_document({[{'_id', PrimaryKeyValue} | proplists:delete(PrimaryKeyName, AttrList)]}).

to_proplist([]) ->
	[];

to_proplist({}) ->
	[];

to_proplist({Tuple}) ->
	to_proplist(Tuple);

to_proplist(Tuple) ->
	Size = tuple_size(Tuple),
	{[{element(X, Tuple), recursive_convert(element(X + 1, Tuple))} || X <- lists:seq(1, Size, 2)]}.

recursive_convert(Value) when erlang:is_tuple(Value) ->
    to_proplist(Value);

recursive_convert(Value) ->
    Value.

recursive_convert_to_bson_document(Value = {PropList}) when erlang:is_list(PropList)->
    bson:document([{Key, recursive_convert_to_bson_document(Value)} || {Key, Value} <- PropList]);

recursive_convert_to_bson_document(Value) ->
    Value.
