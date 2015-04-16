-module(schema_object).
-export([from_json/1, to_json/1, to_json_list/1]).


to_json(BsonDocument) ->
    BsonDocumentUpdated = restore_schema_link(BsonDocument),
    resource_object:to_json(BsonDocumentUpdated, collectionName).


to_json_list(BsonDocuments) ->
    BsonDocumentsUpdated = [restore_schema_link(Doc) || Doc <- BsonDocuments],
    resource_object:to_json_list(BsonDocumentsUpdated, collectionName).


from_json(JsonDocument) ->
    case resource_object:from_json(JsonDocument, collectionName) of
        {ok, BsonDocument} ->
            {ok, convert_schema_link(BsonDocument)};
        Other ->
            Other
    end.


restore_schema_link(BsonDocument) ->
    bson_update_label('%24schema', '$schema', BsonDocument).


convert_schema_link(BsonDocument) ->
    bson_update_label('$schema', '%24schema', BsonDocument).


bson_update_label(OldLabel, NewLabel, BsonDocument) ->
    case bson:lookup(OldLabel, BsonDocument, undefined) of
        undefined ->
            BsonDocument;

        Value ->
            BsonDocumentWithLabel = bson:exclude([OldLabel], BsonDocument),
            bson:update(NewLabel, Value, BsonDocumentWithLabel)

    end.
