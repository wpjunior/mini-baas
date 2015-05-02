-module(resource_object).
-export([to_json/1, to_json/2,
         to_json_list/1, to_json_list/2,
         from_json/1, from_json/2]).

to_json(BsonDocument) ->
    to_json(BsonDocument, id).

to_json(BsonDocument, PrimaryKeyName) ->
    {PrimaryKeyValue} = bson:lookup('_id', BsonDocument),
    BsonDocumentWithOutPrimaryKey = bson:exclude(['_id'], BsonDocument),

    {TailConvertedDocument} = recursive_convert_to_json(BsonDocumentWithOutPrimaryKey),
    {[{atom_to_binary(PrimaryKeyName, utf8), PrimaryKeyValue} | TailConvertedDocument]}.

to_json_list(BsonDocuments) ->
    to_json_list(BsonDocuments, id).

to_json_list(BsonDocuments, PrimaryKeyName) ->
    Items = [to_json(Doc, PrimaryKeyName) || Doc <- BsonDocuments],
    {[{<<"items">>, Items}]}.

from_json(JsonDocument) ->
    from_json(JsonDocument, id).

from_json(JsonDocument, PrimaryKeyName) when is_binary(JsonDocument)->
    try jiffy:decode(JsonDocument) of
         JsonDocumentDecoded ->
            {ok, json_decoded_to_bson(JsonDocumentDecoded, PrimaryKeyName)}
    catch
        _ ->
            {invalid_json, undefined}
    end.

json_decoded_to_bson(JsonDocumentDecoded, PrimaryKeyName) ->
    PrimaryKeyNameBinary = atom_to_binary(PrimaryKeyName, unicode),
    {AttrList} = JsonDocumentDecoded,
    PrimaryKeyFound = proplists:is_defined(PrimaryKeyNameBinary, AttrList),

    if
        PrimaryKeyFound ->
            PrimaryKeyValue = proplists:get_value(PrimaryKeyNameBinary, AttrList),
            recursive_convert_from_json({[{<<"_id">>, PrimaryKeyValue} | proplists:delete(PrimaryKeyNameBinary, AttrList)]});

        true ->
            recursive_convert_from_json(JsonDocumentDecoded)
    end.


recursive_convert_to_json(BsonDocument) when is_tuple(BsonDocument) ->
    Size = tuple_size(BsonDocument),
    {[{atom_to_binary(element(X, BsonDocument), unicode), recursive_convert_to_json(element(X + 1, BsonDocument))} || X <- lists:seq(1, Size, 2)]};

recursive_convert_to_json(Value) ->
    Value.

recursive_convert_from_json({PropList}) when is_list(PropList)->
    bson:document([{binary_to_atom(K, unicode), recursive_convert_from_json(V)} || {K, V} <- PropList]);

recursive_convert_from_json(Value) ->
    Value.
