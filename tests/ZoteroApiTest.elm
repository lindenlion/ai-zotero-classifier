module ZoteroApiTest exposing (..)

import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)
import ZoteroApi


suite : Test
suite =
    describe "ZoteroApi"
        [ describe "itemDecoder"
            [ test "decodes a complete item" <|
                \_ ->
                    let
                        json =
                            """
                            {
                                "key": "ABC123",
                                "version": 42,
                                "data": {
                                    "title": "Fatal Infections in IEI",
                                    "abstractNote": "We report a case...",
                                    "tags": [{"tag": "immunology"}, {"tag": "case report"}],
                                    "collections": ["COL1"],
                                    "itemType": "journalArticle"
                                }
                            }
                            """
                    in
                    Decode.decodeString ZoteroApi.itemDecoder json
                        |> Result.map .key
                        |> Expect.equal (Ok "ABC123")
            , test "decodes item with missing optional fields" <|
                \_ ->
                    let
                        json =
                            """
                            {
                                "key": "XYZ789",
                                "version": 1,
                                "data": {}
                            }
                            """
                    in
                    Decode.decodeString ZoteroApi.itemDecoder json
                        |> Result.map (\item -> ( item.data.title, item.data.abstractNote ))
                        |> Expect.equal (Ok ( "No title", "No abstract available" ))
            , test "decodes callNumber when present" <|
                \_ ->
                    let
                        json =
                            """
                            {
                                "key": "CN1",
                                "version": 1,
                                "data": {
                                    "callNumber": "{\\"v\\":1}"
                                }
                            }
                            """
                    in
                    Decode.decodeString ZoteroApi.itemDecoder json
                        |> Result.map (.data >> .callNumber)
                        |> Expect.equal (Ok "{\"v\":1}")
            , test "defaults callNumber to empty string when absent" <|
                \_ ->
                    let
                        json =
                            """{"key": "CN2", "version": 1, "data": {}}"""
                    in
                    Decode.decodeString ZoteroApi.itemDecoder json
                        |> Result.map (.data >> .callNumber)
                        |> Expect.equal (Ok "")
            , test "decodes item version as int" <|
                \_ ->
                    let
                        json =
                            """{"key": "A", "version": 99, "data": {}}"""
                    in
                    Decode.decodeString ZoteroApi.itemDecoder json
                        |> Result.map .version
                        |> Expect.equal (Ok 99)
            ]
        , describe "itemListDecoder"
            [ test "decodes empty list" <|
                \_ ->
                    Decode.decodeString ZoteroApi.itemListDecoder "[]"
                        |> Result.map List.length
                        |> Expect.equal (Ok 0)
            , test "decodes list of items" <|
                \_ ->
                    let
                        json =
                            """[{"key": "A", "version": 1, "data": {}}, {"key": "B", "version": 2, "data": {}}]"""
                    in
                    Decode.decodeString ZoteroApi.itemListDecoder json
                        |> Result.map List.length
                        |> Expect.equal (Ok 2)
            ]
        , describe "collectionDecoder"
            [ test "decodes a collection without parentCollection" <|
                \_ ->
                    let
                        json =
                            """{"key": "COL1", "data": {"name": "Claude included"}}"""
                    in
                    Decode.decodeString ZoteroApi.collectionDecoder json
                        |> Expect.equal (Ok { key = "COL1", name = "Claude included", parentCollection = "", deleted = False })
            , test "decodes a collection with parentCollection string" <|
                \_ ->
                    let
                        json =
                            """{"key": "SUB1", "data": {"name": "version_1", "parentCollection": "PARENT_KEY"}}"""
                    in
                    Decode.decodeString ZoteroApi.collectionDecoder json
                        |> Expect.equal (Ok { key = "SUB1", name = "version_1", parentCollection = "PARENT_KEY", deleted = False })
            , test "decodes a collection with parentCollection false (top-level)" <|
                \_ ->
                    let
                        json =
                            """{"key": "TOP1", "data": {"name": "Top level", "parentCollection": false}}"""
                    in
                    Decode.decodeString ZoteroApi.collectionDecoder json
                        |> Expect.equal (Ok { key = "TOP1", name = "Top level", parentCollection = "", deleted = False })
            , test "decodes a deleted collection" <|
                \_ ->
                    let
                        json =
                            """{"key": "DEL1", "data": {"name": "Trashed", "deleted": 1}}"""
                    in
                    Decode.decodeString ZoteroApi.collectionDecoder json
                        |> Expect.equal (Ok { key = "DEL1", name = "Trashed", parentCollection = "", deleted = True })
            ]
        , describe "collectionListDecoder"
            [ test "decodes list of collections" <|
                \_ ->
                    let
                        json =
                            """[{"key": "C1", "data": {"name": "A"}}, {"key": "C2", "data": {"name": "B"}}]"""
                    in
                    Decode.decodeString ZoteroApi.collectionListDecoder json
                        |> Result.map (List.map .name)
                        |> Expect.equal (Ok [ "A", "B" ])
            ]
        , describe "articleDataFromItem"
            [ test "extracts article data correctly" <|
                \_ ->
                    let
                        item =
                            { key = "KEY1"
                            , version = 5
                            , data =
                                { title = "My Article"
                                , abstractNote = "Abstract text"
                                , tags = [ { tag = "kw1" }, { tag = "kw2" } ]
                                , collections = []
                                , itemType = "journalArticle"
                                , callNumber = ""
                                }
                            }

                        article =
                            ZoteroApi.articleDataFromItem item
                    in
                    Expect.all
                        [ \a -> Expect.equal "My Article" a.title
                        , \a -> Expect.equal "Abstract text" a.abstract
                        , \a -> Expect.equal "kw1, kw2" a.keywords
                        , \a -> Expect.equal "KEY1" a.itemKey
                        , \a -> Expect.equal 5 a.itemVersion
                        ]
                        article
            , test "handles empty tags as empty keywords" <|
                \_ ->
                    let
                        item =
                            { key = "K"
                            , version = 1
                            , data =
                                { title = "T"
                                , abstractNote = "A"
                                , tags = []
                                , collections = []
                                , itemType = "journalArticle"
                                , callNumber = ""
                                }
                            }
                    in
                    ZoteroApi.articleDataFromItem item
                        |> .keywords
                        |> Expect.equal ""
            ]
        , describe "encodeCreateCollection"
            [ test "encodes collection creation payload" <|
                \_ ->
                    let
                        encoded =
                            ZoteroApi.encodeCreateCollection "My Collection"
                                |> Encode.encode 0
                    in
                    Decode.decodeString
                        (Decode.index 0 (Decode.field "name" Decode.string))
                        encoded
                        |> Expect.equal (Ok "My Collection")
            ]
        , describe "encodeCreateSubCollection"
            [ test "encodes sub-collection with parent key" <|
                \_ ->
                    let
                        encoded =
                            ZoteroApi.encodeCreateSubCollection "version_1" "PARENT_KEY"
                                |> Encode.encode 0
                    in
                    Decode.decodeString
                        (Decode.index 0
                            (Decode.map2 Tuple.pair
                                (Decode.field "name" Decode.string)
                                (Decode.field "parentCollection" Decode.string)
                            )
                        )
                        encoded
                        |> Expect.equal (Ok ( "version_1", "PARENT_KEY" ))
            ]
        , describe "encodeCreateNote"
            [ test "encodes note with parent item" <|
                \_ ->
                    let
                        encoded =
                            ZoteroApi.encodeCreateNote "PARENT1" "<p>Note content</p>"
                                |> Encode.encode 0
                    in
                    Decode.decodeString
                        (Decode.index 0
                            (Decode.map2 Tuple.pair
                                (Decode.field "parentItem" Decode.string)
                                (Decode.field "itemType" Decode.string)
                            )
                        )
                        encoded
                        |> Expect.equal (Ok ( "PARENT1", "note" ))
            , test "note contains the HTML content" <|
                \_ ->
                    let
                        encoded =
                            ZoteroApi.encodeCreateNote "P" "<p>Hello</p>"
                                |> Encode.encode 0
                    in
                    Decode.decodeString
                        (Decode.index 0 (Decode.field "note" Decode.string))
                        encoded
                        |> Expect.equal (Ok "<p>Hello</p>")
            ]
        , describe "encodeItemPatch"
            [ test "encodes tags, collections, and callNumber together" <|
                \_ ->
                    let
                        encoded =
                            ZoteroApi.encodeItemPatch
                                { tags = [ { tag = "CLAUDE" }, { tag = "⭐⭐⭐" } ]
                                , collections = [ "COL1", "COL2" ]
                                , callNumber = "{\"v\":1}"
                                }
                                |> Encode.encode 0
                    in
                    Decode.decodeString
                        (Decode.map3 (\t c cn -> ( t, c, cn ))
                            (Decode.field "tags" (Decode.list (Decode.field "tag" Decode.string)))
                            (Decode.field "collections" (Decode.list Decode.string))
                            (Decode.field "callNumber" Decode.string)
                        )
                        encoded
                        |> Expect.equal (Ok ( [ "CLAUDE", "⭐⭐⭐" ], [ "COL1", "COL2" ], "{\"v\":1}" ))
            ]
        , describe "encodeNotePatch"
            [ test "encodes note content" <|
                \_ ->
                    let
                        encoded =
                            ZoteroApi.encodeNotePatch "<p>Updated</p>"
                                |> Encode.encode 0
                    in
                    Decode.decodeString
                        (Decode.field "note" Decode.string)
                        encoded
                        |> Expect.equal (Ok "<p>Updated</p>")
            ]
        , describe "encodeBatchItemPatch"
            [ test "encodes multiple items with key and version" <|
                \_ ->
                    let
                        encoded =
                            ZoteroApi.encodeBatchItemPatch
                                [ { key = "A1", version = 10, tags = [ { tag = "CLAUDE" } ], collections = [ "COL1" ], callNumber = "{\"v\":1}" }
                                , { key = "B2", version = 20, tags = [], collections = [], callNumber = "{\"v\":1}" }
                                ]
                                |> Encode.encode 0
                    in
                    Decode.decodeString
                        (Decode.list
                            (Decode.map2 Tuple.pair
                                (Decode.field "key" Decode.string)
                                (Decode.field "version" Decode.int)
                            )
                        )
                        encoded
                        |> Expect.equal (Ok [ ( "A1", 10 ), ( "B2", 20 ) ])
            , test "includes callNumber in each item" <|
                \_ ->
                    let
                        encoded =
                            ZoteroApi.encodeBatchItemPatch
                                [ { key = "X", version = 1, tags = [], collections = [], callNumber = "{\"v\":1}" } ]
                                |> Encode.encode 0
                    in
                    Decode.decodeString
                        (Decode.index 0 (Decode.field "callNumber" Decode.string))
                        encoded
                        |> Expect.equal (Ok "{\"v\":1}")
            ]
        , describe "encodeBatchCreateNotes"
            [ test "encodes multiple notes with parentItem" <|
                \_ ->
                    let
                        encoded =
                            ZoteroApi.encodeBatchCreateNotes
                                [ { parentItemKey = "P1", noteHtml = "<p>Note 1</p>" }
                                , { parentItemKey = "P2", noteHtml = "<p>Note 2</p>" }
                                ]
                                |> Encode.encode 0
                    in
                    Decode.decodeString
                        (Decode.list
                            (Decode.map2 Tuple.pair
                                (Decode.field "parentItem" Decode.string)
                                (Decode.field "note" Decode.string)
                            )
                        )
                        encoded
                        |> Expect.equal (Ok [ ( "P1", "<p>Note 1</p>" ), ( "P2", "<p>Note 2</p>" ) ])
            , test "each note has itemType note" <|
                \_ ->
                    let
                        encoded =
                            ZoteroApi.encodeBatchCreateNotes
                                [ { parentItemKey = "P1", noteHtml = "<p>Hi</p>" } ]
                                |> Encode.encode 0
                    in
                    Decode.decodeString
                        (Decode.index 0 (Decode.field "itemType" Decode.string))
                        encoded
                        |> Expect.equal (Ok "note")
            ]
        , describe "noteListDecoder"
            [ test "decodes child notes" <|
                \_ ->
                    let
                        json =
                            """[{"key": "N1", "version": 3, "data": {"note": "<p>Some note</p>", "itemType": "note"}}]"""
                    in
                    Decode.decodeString ZoteroApi.noteListDecoder json
                        |> Result.map (List.map .note)
                        |> Expect.equal (Ok [ "<p>Some note</p>" ])
            , test "decodes empty note list" <|
                \_ ->
                    Decode.decodeString ZoteroApi.noteListDecoder "[]"
                        |> Result.map List.length
                        |> Expect.equal (Ok 0)
            ]
        , describe "isReasoningNote"
            [ test "identifies inclusion reasoning note" <|
                \_ ->
                    ZoteroApi.isReasoningNote
                        { key = "N1", version = 1, note = "<p><em>Inclusion reasoning: relevant</em></p>", tags = [] }
                        |> Expect.equal True
            , test "identifies exclusion reasoning note" <|
                \_ ->
                    ZoteroApi.isReasoningNote
                        { key = "N2", version = 1, note = "<p><em>Exclusion reasoning: not relevant</em></p>", tags = [] }
                        |> Expect.equal True
            , test "rejects unrelated note" <|
                \_ ->
                    ZoteroApi.isReasoningNote
                        { key = "N3", version = 1, note = "<p>Just a random note</p>", tags = [] }
                        |> Expect.equal False
            , test "rejects empty note" <|
                \_ ->
                    ZoteroApi.isReasoningNote
                        { key = "N4", version = 1, note = "", tags = [] }
                        |> Expect.equal False
            , test "identifies auto-generated note" <|
                \_ ->
                    ZoteroApi.isReasoningNote
                        { key = "N5", version = 1, note = "<p><em>This note is auto-generated from structured data</em></p>", tags = [] }
                        |> Expect.equal True
            ]
        , describe "buildNoteHtml"
            [ test "include note contains Todo and Inclusion reasoning" <|
                \_ ->
                    let
                        html =
                            ZoteroApi.buildNoteHtml
                                { isInclude = True
                                , reasoning = "Patient with IEI died from infection."
                                , note = "Check full text for details."
                                }
                    in
                    Expect.all
                        [ \h -> String.contains "Todo:" h |> Expect.equal True
                        , \h -> String.contains "Inclusion reasoning:" h |> Expect.equal True
                        , \h -> String.contains "Check full text for details." h |> Expect.equal True
                        , \h -> String.contains "Patient with IEI died from infection." h |> Expect.equal True
                        ]
                        html
            , test "exclude note contains Exclusion reasoning without Todo" <|
                \_ ->
                    let
                        html =
                            ZoteroApi.buildNoteHtml
                                { isInclude = False
                                , reasoning = "Not about IEI."
                                , note = ""
                                }
                    in
                    Expect.all
                        [ \h -> String.contains "Exclusion reasoning:" h |> Expect.equal True
                        , \h -> String.contains "Not about IEI." h |> Expect.equal True
                        , \h -> String.contains "Todo:" h |> Expect.equal False
                        ]
                        html
            , test "include note is recognized by isReasoningNote" <|
                \_ ->
                    let
                        html =
                            ZoteroApi.buildNoteHtml { isInclude = True, reasoning = "r", note = "n" }
                    in
                    ZoteroApi.isReasoningNote { key = "K", version = 1, note = html, tags = [] }
                        |> Expect.equal True
            , test "exclude note is recognized by isReasoningNote" <|
                \_ ->
                    let
                        html =
                            ZoteroApi.buildNoteHtml { isInclude = False, reasoning = "r", note = "" }
                    in
                    ZoteroApi.isReasoningNote { key = "K", version = 1, note = html, tags = [] }
                        |> Expect.equal True
            ]
        ]
