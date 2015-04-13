-module(resource_object).
-export([to_json/1, to_json/2,
         to_bson/1, to_bson/2]).

to_json(ResourceDocument) ->
    to_json(ResourceDocument, id).

to_json(ResourceDocument, PrimaryKeyName) ->
    {PrimaryKeyValue} = bson:lookup('_id', ResourceDocument),
    TailResourceDocument = bson:exclude(['_id'], ResourceDocument),

    {TailConvertedDocument} = recursive_convert_to_json(TailResourceDocument),
    {[{PrimaryKeyName, PrimaryKeyValue} | TailConvertedDocument]}.

to_bson(Document) ->
    to_bson(Document, id).

to_bson(Document, PrimaryKeyName) ->
    {AttrList} = Document,
    PrimaryKeyValue = proplists:get_value(PrimaryKeyName, AttrList),

    recursive_convert_to_bson({[{'_id', PrimaryKeyValue} | proplists:delete(PrimaryKeyName, AttrList)]}).

recursive_convert_to_json(BsonDocument) when erlang:is_tuple(BsonDocument) ->
    Size = tuple_size(BsonDocument),
    {[{element(X, BsonDocument), recursive_convert_to_json(element(X + 1, BsonDocument))} || X <- lists:seq(1, Size, 2)]};

recursive_convert_to_json(BsonDocument) ->
    BsonDocument.

recursive_convert_to_bson({PropList}) when erlang:is_list(PropList)->
    bson:document([{K, recursive_convert_to_bson(V)} || {K, V} <- PropList]);

recursive_convert_to_bson(Value) ->
    Value.
