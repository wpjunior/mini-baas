-module(resource_object).
-export([structure_to_string/1,
         to_json/1, to_json/2, to_json/3,
         to_json_list/1, to_json_list/2,
         from_json/1, from_json/2]).

structure_to_string(JsonStructure) ->
     jiffy:encode(JsonStructure).

to_json(BsonDocument) ->
    to_json(BsonDocument, id).

to_json(BsonDocument, PrimaryKeyName) ->
    to_json(BsonDocument, PrimaryKeyName, string).

to_json(BsonDocument, PrimaryKeyName, string) ->
    structure_to_string(to_json(BsonDocument, PrimaryKeyName, structure));

to_json(BsonDocument, PrimaryKeyName, structure) ->
    bson_to_json_structure(BsonDocument, PrimaryKeyName).

to_json_list(BsonDocuments) ->
    to_json_list(BsonDocuments, id).

to_json_list(BsonDocuments, PrimaryKeyName) ->
    Items = [bson_to_json_structure(Doc, PrimaryKeyName) || Doc <- BsonDocuments],
    structure_to_string({[{items, Items}]}).

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

bson_to_json_structure(BsonDocument, PrimaryKeyName) ->
    {PrimaryKeyValue} = bson:lookup('_id', BsonDocument),
    BsonDocumentWithOutPrimaryKey = bson:exclude(['_id'], BsonDocument),

    {TailConvertedDocument} = recursive_convert_to_json(BsonDocumentWithOutPrimaryKey),
    {[{atom_to_binary(PrimaryKeyName, utf8), PrimaryKeyValue} | TailConvertedDocument]}.


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
