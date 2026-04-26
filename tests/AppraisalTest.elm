module AppraisalTest exposing (..)

import Appraisal
import Classification exposing (Relevance(..))
import Dict
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)


sampleAppraisal : Appraisal.ProviderAppraisal
sampleAppraisal =
    { relevance = 4
    , decision = True
    , reasoning = "DOCK8 patient died from fungal sepsis."
    , note = "Verify infection type in full text."
    , deathAfterTherapy = False
    , isRefusal = False
    , model = "claude-opus-4-6"
    , timestamp = "2026-04-13T10:30:00.000Z"
    , renamedFrom = Nothing
    }


sampleData : Appraisal.AppraisalData
sampleData =
    { version = 3
    , appraisals = Dict.singleton "claude" sampleAppraisal
    , asreview = Nothing
    , analysis = Nothing
    }


suite : Test
suite =
    describe "Appraisal"
        [ describe "encode/decode roundtrip"
            [ test "single provider roundtrip" <|
                \_ ->
                    sampleData
                        |> Appraisal.encode
                        |> Encode.encode 0
                        |> Decode.decodeString Appraisal.decode
                        |> Expect.equal (Ok sampleData)
            , test "multi-provider roundtrip" <|
                \_ ->
                    let
                        deepseek =
                            { sampleAppraisal | model = "deepseek-v3", relevance = 3 }

                        multiData =
                            sampleData
                                |> Appraisal.setAppraisal "deepseek" deepseek
                    in
                    multiData
                        |> Appraisal.encode
                        |> Encode.encode 0
                        |> Decode.decodeString Appraisal.decode
                        |> Expect.equal (Ok multiData)
            , test "roundtrip with asreview data" <|
                \_ ->
                    let
                        withAsreview =
                            { sampleData
                                | asreview =
                                    Just
                                        { decision = True
                                        , user = 3
                                        , timestamp = "2026-04-13T10:30:00.000Z"
                                        , tags = Dict.singleton "priority" True
                                        , ranking = "high"
                                        , note = "Confirmed relevant"
                                        }
                            }
                    in
                    withAsreview
                        |> Appraisal.encode
                        |> Encode.encode 0
                        |> Decode.decodeString Appraisal.decode
                        |> Expect.equal (Ok withAsreview)
            ]
        , describe "fromClassificationResult"
            [ test "maps include result correctly" <|
                \_ ->
                    let
                        result =
                            { relevance = FourStars
                            , reasoning = "IEI patient died"
                            , note = "Check full text"
                            , deathAfterTherapy = False
                            }

                        appraisal =
                            Appraisal.fromClassificationResult
                                { model = "claude-opus-4-6", timestamp = "2026-04-13T00:00:00.000Z" }
                                result
                    in
                    Expect.all
                        [ \a -> Expect.equal 4 a.relevance
                        , \a -> Expect.equal True a.decision
                        , \a -> Expect.equal "IEI patient died" a.reasoning
                        , \a -> Expect.equal "Check full text" a.note
                        , \a -> Expect.equal False a.deathAfterTherapy
                        , \a -> Expect.equal False a.isRefusal
                        , \a -> Expect.equal "claude-opus-4-6" a.model
                        ]
                        appraisal
            , test "maps exclude result correctly" <|
                \_ ->
                    let
                        result =
                            { relevance = OneStar
                            , reasoning = "No death reported"
                            , note = ""
                            , deathAfterTherapy = False
                            }

                        appraisal =
                            Appraisal.fromClassificationResult
                                { model = "claude-sonnet", timestamp = "" }
                                result
                    in
                    Expect.all
                        [ \a -> Expect.equal 1 a.relevance
                        , \a -> Expect.equal False a.decision
                        ]
                        appraisal
            , test "maps refusal result" <|
                \_ ->
                    let
                        appraisal =
                            Appraisal.fromClassificationResult
                                { model = "claude-opus-4-6", timestamp = "" }
                                Classification.refusalResult
                    in
                    Expect.all
                        [ \a -> Expect.equal True a.isRefusal
                        , \a -> Expect.equal 3 a.relevance
                        , \a -> Expect.equal True a.decision
                        ]
                        appraisal
            ]
        , describe "setAppraisal"
            [ test "adds new provider" <|
                \_ ->
                    let
                        deepseek =
                            { sampleAppraisal | model = "deepseek-v3" }

                        updated =
                            Appraisal.setAppraisal "deepseek" deepseek sampleData
                    in
                    Dict.size updated.appraisals
                        |> Expect.equal 2
            , test "replaces existing provider" <|
                \_ ->
                    let
                        updated =
                            { sampleAppraisal | relevance = 5 }

                        result =
                            Appraisal.setAppraisal "claude" updated sampleData
                    in
                    Dict.get "claude" result.appraisals
                        |> Maybe.map .relevance
                        |> Expect.equal (Just 5)
            , test "preserves other providers" <|
                \_ ->
                    let
                        withTwo =
                            sampleData
                                |> Appraisal.setAppraisal "deepseek" { sampleAppraisal | model = "deepseek" }

                        updated =
                            Appraisal.setAppraisal "claude" { sampleAppraisal | relevance = 5 } withTwo
                    in
                    Dict.get "deepseek" updated.appraisals
                        |> Maybe.map .model
                        |> Expect.equal (Just "deepseek")
            ]
        , describe "generateNoteHtml"
            [ test "includes auto-generated disclaimer" <|
                \_ ->
                    Appraisal.generateNoteHtml sampleData
                        |> String.contains "auto-generated from structured data"
                        |> Expect.equal True
            , test "includes provider name and model" <|
                \_ ->
                    let
                        html =
                            Appraisal.generateNoteHtml sampleData
                    in
                    Expect.all
                        [ \h -> String.contains "claude" h |> Expect.equal True
                        , \h -> String.contains "claude-opus-4-6" h |> Expect.equal True
                        ]
                        html
            , test "shows Todo for include decisions" <|
                \_ ->
                    Appraisal.generateNoteHtml sampleData
                        |> String.contains "Todo:"
                        |> Expect.equal True
            , test "omits Todo for exclude decisions" <|
                \_ ->
                    let
                        excludeData =
                            { sampleData
                                | appraisals =
                                    Dict.singleton "claude"
                                        { sampleAppraisal | decision = False, relevance = 1, note = "" }
                            }
                    in
                    Appraisal.generateNoteHtml excludeData
                        |> String.contains "Todo:"
                        |> Expect.equal False
            , test "shows all providers" <|
                \_ ->
                    let
                        multiData =
                            sampleData
                                |> Appraisal.setAppraisal "deepseek"
                                    { sampleAppraisal | model = "deepseek-v3" }

                        html =
                            Appraisal.generateNoteHtml multiData
                    in
                    Expect.all
                        [ \h -> String.contains "claude" h |> Expect.equal True
                        , \h -> String.contains "deepseek" h |> Expect.equal True
                        ]
                        html
            ]
        , describe "migrateFromLegacy"
            [ test "migrates from CLAUDE tag + star tag" <|
                \_ ->
                    let
                        result =
                            Appraisal.migrateFromLegacy
                                { tags = [ { tag = "CLAUDE" }, { tag = "⭐⭐⭐⭐" } ]
                                , reasoningNoteHtml = Nothing
                                }
                    in
                    result
                        |> Maybe.andThen (\d -> Dict.get "claude" d.appraisals)
                        |> Maybe.map .relevance
                        |> Expect.equal (Just 4)
            , test "migrates deathAfterTherapy tag" <|
                \_ ->
                    let
                        result =
                            Appraisal.migrateFromLegacy
                                { tags = [ { tag = "CLAUDE" }, { tag = "⭐⭐⭐" }, { tag = "death_after_therapy" } ]
                                , reasoningNoteHtml = Nothing
                                }
                    in
                    result
                        |> Maybe.andThen (\d -> Dict.get "claude" d.appraisals)
                        |> Maybe.map .deathAfterTherapy
                        |> Expect.equal (Just True)
            , test "extracts reasoning from inclusion note HTML" <|
                \_ ->
                    let
                        noteHtml =
                            "<p><strong>Todo:</strong></p><p>Check full text</p><p><em>Inclusion reasoning: IEI patient died from infection</em></p>"

                        result =
                            Appraisal.migrateFromLegacy
                                { tags = [ { tag = "CLAUDE" }, { tag = "⭐⭐⭐⭐⭐" } ]
                                , reasoningNoteHtml = Just noteHtml
                                }
                    in
                    result
                        |> Maybe.andThen (\d -> Dict.get "claude" d.appraisals)
                        |> Maybe.map .reasoning
                        |> Expect.equal (Just "IEI patient died from infection")
            , test "extracts reasoning from exclusion note HTML" <|
                \_ ->
                    let
                        noteHtml =
                            "<p><em>Exclusion reasoning: Not about IEI</em></p>"

                        result =
                            Appraisal.migrateFromLegacy
                                { tags = [ { tag = "CLAUDE" }, { tag = "⭐" } ]
                                , reasoningNoteHtml = Just noteHtml
                                }
                    in
                    result
                        |> Maybe.andThen (\d -> Dict.get "claude" d.appraisals)
                        |> Maybe.map .reasoning
                        |> Expect.equal (Just "Not about IEI")
            , test "extracts Todo note from standard format" <|
                \_ ->
                    let
                        noteHtml =
                            "<p><strong>Todo:</strong></p><p>Check full text for details</p><p><em>Inclusion reasoning: IEI patient died</em></p>"

                        result =
                            Appraisal.migrateFromLegacy
                                { tags = [ { tag = "CLAUDE" }, { tag = "⭐⭐⭐⭐" } ]
                                , reasoningNoteHtml = Just noteHtml
                                }
                    in
                    result
                        |> Maybe.andThen (\d -> Dict.get "claude" d.appraisals)
                        |> Maybe.map .note
                        |> Expect.equal (Just "Check full text for details")
            , test "extracts Todo note with newlines (Zotero br tags)" <|
                \_ ->
                    let
                        noteHtml =
                            "<p><strong>Todo:</strong></p><p>Check foo<br/>and bar in fulltext</p><p><em>Inclusion reasoning: relevant</em></p>"

                        result =
                            Appraisal.migrateFromLegacy
                                { tags = [ { tag = "CLAUDE" }, { tag = "⭐⭐⭐⭐" } ]
                                , reasoningNoteHtml = Just noteHtml
                                }
                    in
                    result
                        |> Maybe.andThen (\d -> Dict.get "claude" d.appraisals)
                        |> Maybe.map .note
                        |> Expect.equal (Just "Check foo<br/>and bar in fulltext")
            , test "extracts Todo note split across multiple p tags" <|
                \_ ->
                    let
                        noteHtml =
                            "<p><strong>Todo:</strong></p><p>Check foo</p><p>and bar in fulltext</p><p><em>Inclusion reasoning: relevant</em></p>"

                        result =
                            Appraisal.migrateFromLegacy
                                { tags = [ { tag = "CLAUDE" }, { tag = "⭐⭐⭐⭐" } ]
                                , reasoningNoteHtml = Just noteHtml
                                }
                    in
                    result
                        |> Maybe.andThen (\d -> Dict.get "claude" d.appraisals)
                        |> Maybe.map .note
                        |> Expect.equal (Just "Check foo</p><p>and bar in fulltext")
            , test "extracts Todo note with Zotero div wrapper" <|
                \_ ->
                    let
                        noteHtml =
                            "<div><p><strong>Todo:</strong></p><p>Verify infection type</p><p><em>Inclusion reasoning: relevant</em></p></div>"

                        result =
                            Appraisal.migrateFromLegacy
                                { tags = [ { tag = "CLAUDE" }, { tag = "⭐⭐⭐⭐" } ]
                                , reasoningNoteHtml = Just noteHtml
                                }
                    in
                    result
                        |> Maybe.andThen (\d -> Dict.get "claude" d.appraisals)
                        |> Maybe.map .note
                        |> Expect.equal (Just "Verify infection type")
            , test "returns empty note for exclusion (no Todo)" <|
                \_ ->
                    let
                        noteHtml =
                            "<p><em>Exclusion reasoning: Not about IEI</em></p>"

                        result =
                            Appraisal.migrateFromLegacy
                                { tags = [ { tag = "CLAUDE" }, { tag = "⭐" } ]
                                , reasoningNoteHtml = Just noteHtml
                                }
                    in
                    result
                        |> Maybe.andThen (\d -> Dict.get "claude" d.appraisals)
                        |> Maybe.map .note
                        |> Expect.equal (Just "")
            , test "returns empty note when no reasoning note exists" <|
                \_ ->
                    let
                        result =
                            Appraisal.migrateFromLegacy
                                { tags = [ { tag = "CLAUDE" }, { tag = "⭐⭐⭐⭐" } ]
                                , reasoningNoteHtml = Nothing
                                }
                    in
                    result
                        |> Maybe.andThen (\d -> Dict.get "claude" d.appraisals)
                        |> Maybe.map .note
                        |> Expect.equal (Just "")
            , test "extracts Todo when Todo text is on same line as strong tag" <|
                \_ ->
                    let
                        -- Some Zotero versions may render inline
                        noteHtml =
                            "<p><strong>Todo:</strong> Check the dosage</p><p><em>Inclusion reasoning: relevant</em></p>"

                        result =
                            Appraisal.migrateFromLegacy
                                { tags = [ { tag = "CLAUDE" }, { tag = "⭐⭐⭐⭐" } ]
                                , reasoningNoteHtml = Just noteHtml
                                }
                    in
                    result
                        |> Maybe.andThen (\d -> Dict.get "claude" d.appraisals)
                        |> Maybe.map .note
                        |> Expect.equal (Just "Check the dosage")
            , test "returns Nothing without CLAUDE tag" <|
                \_ ->
                    Appraisal.migrateFromLegacy
                        { tags = [ { tag = "immunology" } ]
                        , reasoningNoteHtml = Nothing
                        }
                        |> Expect.equal Nothing
            , test "sets model to opus-4-6 for legacy data" <|
                \_ ->
                    Appraisal.migrateFromLegacy
                        { tags = [ { tag = "CLAUDE" }, { tag = "⭐⭐⭐" } ]
                        , reasoningNoteHtml = Nothing
                        }
                        |> Maybe.andThen (\d -> Dict.get "claude" d.appraisals)
                        |> Maybe.map .model
                        |> Expect.equal (Just "opus-4-6")
            ]
        , describe "renameKeys"
            [ test "renames an existing key when timestamp is before cutoff" <|
                \_ ->
                    let
                        data =
                            sampleData
                                |> Appraisal.setAppraisal "deepseek" { sampleAppraisal | model = "deepseek-reasoner", timestamp = "2026-04-01T00:00:00.000Z" }

                        renamed =
                            Appraisal.renameKeys [ { oldKey = "deepseek", newKey = "deepseek-legacy", before = "2026-04-24T00:00:00.000Z" } ] data
                    in
                    Expect.all
                        [ \d -> Dict.member "deepseek-legacy" d.appraisals |> Expect.equal True
                        , \d -> Dict.member "deepseek" d.appraisals |> Expect.equal False
                        , \d -> Dict.get "deepseek-legacy" d.appraisals |> Maybe.map .model |> Expect.equal (Just "deepseek-reasoner")
                        , \d -> Dict.get "deepseek-legacy" d.appraisals |> Maybe.andThen .renamedFrom |> Expect.equal (Just "deepseek")
                        , \d -> Dict.member "claude" d.appraisals |> Expect.equal True
                        ]
                        renamed
            , test "does NOT rename when timestamp is after cutoff" <|
                \_ ->
                    let
                        data =
                            sampleData
                                |> Appraisal.setAppraisal "deepseek" { sampleAppraisal | model = "deepseek-v4-pro", timestamp = "2026-05-01T00:00:00.000Z" }

                        renamed =
                            Appraisal.renameKeys [ { oldKey = "deepseek", newKey = "deepseek-legacy", before = "2026-04-24T00:00:00.000Z" } ] data
                    in
                    Expect.all
                        [ \d -> Dict.member "deepseek" d.appraisals |> Expect.equal True
                        , \d -> Dict.member "deepseek-legacy" d.appraisals |> Expect.equal False
                        ]
                        renamed
            , test "does nothing when old key doesn't exist" <|
                \_ ->
                    let
                        renamed =
                            Appraisal.renameKeys [ { oldKey = "nonexistent", newKey = "new-key", before = "2099-01-01T00:00:00.000Z" } ] sampleData
                    in
                    Expect.equal sampleData.appraisals renamed.appraisals
            , test "does not overwrite if new key already exists" <|
                \_ ->
                    let
                        data =
                            sampleData
                                |> Appraisal.setAppraisal "deepseek" { sampleAppraisal | model = "deepseek-reasoner", timestamp = "2026-04-01T00:00:00.000Z" }

                        renamed =
                            Appraisal.renameKeys [ { oldKey = "deepseek", newKey = "claude", before = "2099-01-01T00:00:00.000Z" } ] data
                    in
                    Expect.all
                        [ \d -> Dict.get "claude" d.appraisals |> Maybe.map .model |> Expect.equal (Just "claude-opus-4-6")
                        , \d -> Dict.member "deepseek" d.appraisals |> Expect.equal True
                        ]
                        renamed
            , test "applies multiple renames" <|
                \_ ->
                    let
                        data =
                            sampleData
                                |> Appraisal.setAppraisal "deepseek" { sampleAppraisal | model = "deepseek-v3", timestamp = "2026-04-01T00:00:00.000Z" }
                                |> Appraisal.setAppraisal "gemini" { sampleAppraisal | model = "gemini-pro", timestamp = "2026-04-01T00:00:00.000Z" }

                        renamed =
                            Appraisal.renameKeys
                                [ { oldKey = "deepseek", newKey = "deepseek-legacy", before = "2026-04-24T00:00:00.000Z" }
                                , { oldKey = "gemini", newKey = "gemini-legacy", before = "2026-04-24T00:00:00.000Z" }
                                ]
                                data
                    in
                    Expect.all
                        [ \d -> Dict.member "deepseek-legacy" d.appraisals |> Expect.equal True
                        , \d -> Dict.member "gemini-legacy" d.appraisals |> Expect.equal True
                        , \d -> Dict.member "deepseek" d.appraisals |> Expect.equal False
                        , \d -> Dict.member "gemini" d.appraisals |> Expect.equal False
                        , \d -> Dict.size d.appraisals |> Expect.equal 3
                        ]
                        renamed
            , test "multiple renames on same key must be applied oldest-first" <|
                \_ ->
                    -- deepseek was used with model A (early), then model B (later)
                    -- Two renames target "deepseek" with different cutoffs
                    -- Only works correctly if applied oldest-first
                    let
                        data =
                            sampleData
                                |> Appraisal.setAppraisal "deepseek"
                                    { sampleAppraisal | model = "deepseek-reasoner", timestamp = "2026-03-15T00:00:00.000Z" }

                        -- Apply oldest cutoff first, then newest — simulates sorted order
                        renamed =
                            Appraisal.renameKeys
                                [ { oldKey = "deepseek", newKey = "deepseek-legacy1", before = "2026-04-01T00:00:00.000Z" }
                                , { oldKey = "deepseek", newKey = "deepseek-legacy2", before = "2026-04-24T00:00:00.000Z" }
                                ]
                                data
                    in
                    Expect.all
                        [ -- The appraisal (timestamp 03-15) is before 04-01, so it matches the first rename
                          \d -> Dict.member "deepseek-legacy1" d.appraisals |> Expect.equal True
                        , \d -> Dict.get "deepseek-legacy1" d.appraisals |> Maybe.map .model |> Expect.equal (Just "deepseek-reasoner")

                        -- deepseek-legacy2 should NOT exist (nothing left to rename)
                        , \d -> Dict.member "deepseek-legacy2" d.appraisals |> Expect.equal False

                        -- Original key should be gone
                        , \d -> Dict.member "deepseek" d.appraisals |> Expect.equal False
                        ]
                        renamed
            , test "wrong order would misassign the rename" <|
                \_ ->
                    -- Same data but renames applied newest-first (wrong order)
                    let
                        data =
                            sampleData
                                |> Appraisal.setAppraisal "deepseek"
                                    { sampleAppraisal | model = "deepseek-reasoner", timestamp = "2026-03-15T00:00:00.000Z" }

                        renamed =
                            Appraisal.renameKeys
                                [ { oldKey = "deepseek", newKey = "deepseek-legacy2", before = "2026-04-24T00:00:00.000Z" }
                                , { oldKey = "deepseek", newKey = "deepseek-legacy1", before = "2026-04-01T00:00:00.000Z" }
                                ]
                                data
                    in
                    -- With wrong order, the newer cutoff grabs it first
                    Expect.all
                        [ \d -> Dict.member "deepseek-legacy2" d.appraisals |> Expect.equal True
                        , \d -> Dict.member "deepseek-legacy1" d.appraisals |> Expect.equal False
                        ]
                        renamed
            ]
        ]
