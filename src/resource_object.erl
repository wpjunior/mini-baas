-module(resource_object).
-export([to_json/1, to_json/2,
         from_json/1, from_json/2]).

to_json(ResourceDocument) ->
    to_json(ResourceDocument, id).

to_json(ResourceDocument, PrimaryKeyName) ->
    {PrimaryKeyValue} = bson:lookup('_id', ResourceDocument),
    TailResourceDocument = bson:exclude(['_id'], ResourceDocument),

    {TailConvertedDocument} = recursive_convert_to_json(TailResourceDocument),
    {[{PrimaryKeyName, PrimaryKeyValue} | TailConvertedDocument]}.

from_json(Document) ->
    from_json(Document, id).

from_json(Document, PrimaryKeyName) when is_binary(Document)->
    PrimaryKeyNameBinary = erlang:atom_to_binary(PrimaryKeyName, unicode),
    DecodeDocument = jiffy:decode(Document),
    {AttrList} = DecodeDocument,
    PrimaryKeyFound = proplists:is_defined(PrimaryKeyNameBinary, AttrList),

    if
        PrimaryKeyFound ->
            PrimaryKeyValue = proplists:get_value(PrimaryKeyNameBinary, AttrList),
            recursive_convert_from_json({[{<<"_id">>, PrimaryKeyValue} | proplists:delete(PrimaryKeyNameBinary, AttrList)]});

        true ->
            recursive_convert_from_json(DecodeDocument)
    end.


recursive_convert_to_json(BsonDocument) when erlang:is_tuple(BsonDocument) ->
    Size = tuple_size(BsonDocument),
    {[{element(X, BsonDocument), recursive_convert_to_json(element(X + 1, BsonDocument))} || X <- lists:seq(1, Size, 2)]};

recursive_convert_to_json(BsonDocument) ->
    BsonDocument.

recursive_convert_from_json({PropList}) when erlang:is_list(PropList)->
    bson:document([{erlang:binary_to_atom(K, unicode), recursive_convert_from_json(V)} || {K, V} <- PropList]);

recursive_convert_from_json(Value) ->
    Value.
