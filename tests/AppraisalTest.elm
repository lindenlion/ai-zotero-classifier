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
    }


sampleData : Appraisal.AppraisalData
sampleData =
    { version = 1
    , appraisals = Dict.singleton "claude" sampleAppraisal
    , asreview = Nothing
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
        ]
