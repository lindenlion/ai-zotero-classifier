module ZoteroApi exposing
    ( ArticleData
    , ZoteroCollection
    , ZoteroItem
    , ZoteroNote
    , ZoteroTag
    , articleDataFromItem
    , buildNoteHtml
    , collectionDecoder
    , collectionListDecoder
    , encodeBatchCreateNotes
    , encodeBatchItemPatch
    , encodeCreateCollection
    , encodeCreateNote
    , encodeCreateSubCollection
    , encodeItemPatch
    , encodeNotePatch
    , isReasoningNote
    , itemDecoder
    , itemListDecoder
    , noteListDecoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias ZoteroTag =
    { tag : String
    }


type alias ZoteroItem =
    { key : String
    , version : Int
    , data : ZoteroItemData
    }


type alias ZoteroItemData =
    { title : String
    , abstractNote : String
    , tags : List ZoteroTag
    , collections : List String
    , itemType : String
    , callNumber : String
    }


type alias ZoteroCollection =
    { key : String
    , name : String
    , parentCollection : String
    , deleted : Bool
    }


type alias ZoteroNote =
    { key : String
    , version : Int
    , note : String
    }


type alias ArticleData =
    { title : String
    , abstract : String
    , keywords : String
    , itemKey : String
    , itemVersion : Int
    }



-- Decoders


tagDecoder : Decoder ZoteroTag
tagDecoder =
    Decode.map ZoteroTag
        (Decode.field "tag" Decode.string)


itemDataDecoder : Decoder ZoteroItemData
itemDataDecoder =
    Decode.map5
        (\title abstract tags collections itemType ->
            \callNum ->
                { title = title
                , abstractNote = abstract
                , tags = tags
                , collections = collections
                , itemType = itemType
                , callNumber = callNum
                }
        )
        (Decode.field "title" Decode.string
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault "No title")
        )
        (Decode.field "abstractNote" Decode.string
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault "No abstract available")
        )
        (Decode.field "tags" (Decode.list tagDecoder)
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault [])
        )
        (Decode.field "collections" (Decode.list Decode.string)
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault [])
        )
        (Decode.field "itemType" Decode.string
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault "")
        )
        |> Decode.andThen
            (\partialFn ->
                Decode.field "callNumber" Decode.string
                    |> Decode.maybe
                    |> Decode.map (Maybe.withDefault "")
                    |> Decode.map partialFn
            )


itemDecoder : Decoder ZoteroItem
itemDecoder =
    Decode.map3 ZoteroItem
        (Decode.field "key" Decode.string)
        (Decode.field "version" Decode.int)
        (Decode.field "data" itemDataDecoder)


itemListDecoder : Decoder (List ZoteroItem)
itemListDecoder =
    Decode.list itemDecoder


noteDecoder : Decoder ZoteroNote
noteDecoder =
    Decode.map3 ZoteroNote
        (Decode.field "key" Decode.string)
        (Decode.field "version" Decode.int)
        (Decode.at [ "data", "note" ] Decode.string)


noteListDecoder : Decoder (List ZoteroNote)
noteListDecoder =
    Decode.list noteDecoder


collectionDecoder : Decoder ZoteroCollection
collectionDecoder =
    Decode.map4 ZoteroCollection
        (Decode.field "key" Decode.string)
        (Decode.at [ "data", "name" ] Decode.string)
        (Decode.at [ "data", "parentCollection" ]
            (Decode.oneOf
                [ Decode.string
                , Decode.succeed ""
                ]
            )
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault "")
        )
        (Decode.at [ "data", "deleted" ]
            (Decode.oneOf
                [ Decode.bool
                , Decode.int |> Decode.map (\n -> n /= 0)
                ]
            )
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault False)
        )


collectionListDecoder : Decoder (List ZoteroCollection)
collectionListDecoder =
    Decode.list collectionDecoder



-- Encoders


encodeCreateCollection : String -> Encode.Value
encodeCreateCollection name =
    Encode.list identity
        [ Encode.object
            [ ( "name", Encode.string name )
            ]
        ]


encodeCreateSubCollection : String -> String -> Encode.Value
encodeCreateSubCollection name parentKey =
    Encode.list identity
        [ Encode.object
            [ ( "name", Encode.string name )
            , ( "parentCollection", Encode.string parentKey )
            ]
        ]


{-| Encode a PATCH body that updates tags, collections, and callNumber in one request.
-}
encodeItemPatch : { tags : List ZoteroTag, collections : List String, callNumber : String } -> Encode.Value
encodeItemPatch patch =
    Encode.object
        [ ( "tags"
          , Encode.list
                (\t -> Encode.object [ ( "tag", Encode.string t.tag ) ])
                patch.tags
          )
        , ( "collections"
          , Encode.list Encode.string patch.collections
          )
        , ( "callNumber"
          , Encode.string patch.callNumber
          )
        ]


encodeCreateNote : String -> String -> Encode.Value
encodeCreateNote parentItemKey noteHtml =
    Encode.list identity
        [ Encode.object
            [ ( "itemType", Encode.string "note" )
            , ( "note", Encode.string noteHtml )
            , ( "parentItem", Encode.string parentItemKey )
            , ( "tags", Encode.list identity [] )
            , ( "collections", Encode.list identity [] )
            , ( "relations", Encode.object [] )
            ]
        ]


{-| Encode a PATCH body to update a note's content.
-}
encodeNotePatch : String -> Encode.Value
encodeNotePatch noteHtml =
    Encode.object
        [ ( "note", Encode.string noteHtml )
        ]


{-| Encode a batch of item updates for POST /items (multi-object write).
Each item must include key and version for the server to apply the patch.
-}
encodeBatchItemPatch :
    List
        { key : String
        , version : Int
        , tags : List ZoteroTag
        , collections : List String
        , callNumber : String
        }
    -> Encode.Value
encodeBatchItemPatch items =
    Encode.list
        (\item ->
            Encode.object
                [ ( "key", Encode.string item.key )
                , ( "version", Encode.int item.version )
                , ( "tags"
                  , Encode.list
                        (\t -> Encode.object [ ( "tag", Encode.string t.tag ) ])
                        item.tags
                  )
                , ( "collections", Encode.list Encode.string item.collections )
                , ( "callNumber", Encode.string item.callNumber )
                ]
        )
        items


{-| Encode a batch of new notes for POST /items (multi-object create).
-}
encodeBatchCreateNotes : List { parentItemKey : String, noteHtml : String } -> Encode.Value
encodeBatchCreateNotes notes =
    Encode.list
        (\n ->
            Encode.object
                [ ( "itemType", Encode.string "note" )
                , ( "note", Encode.string n.noteHtml )
                , ( "parentItem", Encode.string n.parentItemKey )
                , ( "tags", Encode.list identity [] )
                , ( "collections", Encode.list identity [] )
                , ( "relations", Encode.object [] )
                ]
        )
        notes



-- Helpers


isReasoningNote : ZoteroNote -> Bool
isReasoningNote note =
    String.contains "Inclusion reasoning" note.note
        || String.contains "Exclusion reasoning" note.note
        || String.contains "auto-generated from structured data" note.note


{-| Build the HTML content for a reasoning note.
Produces HTML that `isReasoningNote` will recognize.
-}
buildNoteHtml : { isInclude : Bool, reasoning : String, note : String } -> String
buildNoteHtml { isInclude, reasoning, note } =
    if isInclude then
        "<p><strong>Todo:</strong></p><p>"
            ++ note
            ++ "</p><p><em>Inclusion reasoning: "
            ++ reasoning
            ++ "</em></p>"

    else
        "<p><em>Exclusion reasoning: "
            ++ reasoning
            ++ "</em></p>"



-- Extraction


articleDataFromItem : ZoteroItem -> ArticleData
articleDataFromItem item =
    let
        keywords =
            item.data.tags
                |> List.map .tag
                |> String.join ", "
    in
    { title = item.data.title
    , abstract = item.data.abstractNote
    , keywords = keywords
    , itemKey = item.key
    , itemVersion = item.version
    }
