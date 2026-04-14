module ClassificationTest exposing (..)

import Classification
    exposing
        ( Decision(..)
        , Relevance(..)
        )
import Expect
import Json.Decode as Decode
import Test exposing (..)


suite : Test
suite =
    describe "Classification"
        [ describe "emojiToRelevance"
            [ test "one star" <|
                \_ ->
                    Classification.emojiToRelevance "⭐"
                        |> Expect.equal (Just OneStar)
            , test "two stars" <|
                \_ ->
                    Classification.emojiToRelevance "⭐⭐"
                        |> Expect.equal (Just TwoStars)
            , test "three stars" <|
                \_ ->
                    Classification.emojiToRelevance "⭐⭐⭐"
                        |> Expect.equal (Just ThreeStars)
            , test "four stars" <|
                \_ ->
                    Classification.emojiToRelevance "⭐⭐⭐⭐"
                        |> Expect.equal (Just FourStars)
            , test "five stars" <|
                \_ ->
                    Classification.emojiToRelevance "⭐⭐⭐⭐⭐"
                        |> Expect.equal (Just FiveStars)
            , test "invalid returns Nothing" <|
                \_ ->
                    Classification.emojiToRelevance "invalid"
                        |> Expect.equal Nothing
            , test "empty string returns Nothing" <|
                \_ ->
                    Classification.emojiToRelevance ""
                        |> Expect.equal Nothing
            ]
        , describe "relevanceToEmoji roundtrip"
            [ test "OneStar roundtrips" <|
                \_ ->
                    Classification.relevanceToEmoji OneStar
                        |> Classification.emojiToRelevance
                        |> Expect.equal (Just OneStar)
            , test "FiveStars roundtrips" <|
                \_ ->
                    Classification.relevanceToEmoji FiveStars
                        |> Classification.emojiToRelevance
                        |> Expect.equal (Just FiveStars)
            ]
        , describe "relevanceToDecision"
            [ test "1 star is Exclude" <|
                \_ ->
                    Classification.relevanceToDecision OneStar
                        |> Expect.equal Exclude
            , test "2 stars is Exclude" <|
                \_ ->
                    Classification.relevanceToDecision TwoStars
                        |> Expect.equal Exclude
            , test "3 stars is Include" <|
                \_ ->
                    Classification.relevanceToDecision ThreeStars
                        |> Expect.equal Include
            , test "4 stars is Include" <|
                \_ ->
                    Classification.relevanceToDecision FourStars
                        |> Expect.equal Include
            , test "5 stars is Include" <|
                \_ ->
                    Classification.relevanceToDecision FiveStars
                        |> Expect.equal Include
            ]
        , describe "decisionToString"
            [ test "Include" <|
                \_ ->
                    Classification.decisionToString Include
                        |> Expect.equal "INCLUDE"
            , test "Exclude" <|
                \_ ->
                    Classification.decisionToString Exclude
                        |> Expect.equal "EXCLUDE"
            ]
        , describe "classificationResultDecoder"
            [ test "decodes valid 5-star response" <|
                \_ ->
                    let
                        json =
                            """{"relevance": "⭐⭐⭐⭐⭐", "reasoning": "Patient with DOCK8 deficiency died from fungal infection.", "note": "Check full text for details on infection type.", "death_after_therapy": false}"""
                    in
                    Decode.decodeString Classification.classificationResultDecoder json
                        |> Expect.equal
                            (Ok
                                { relevance = FiveStars
                                , reasoning = "Patient with DOCK8 deficiency died from fungal infection."
                                , note = "Check full text for details on infection type."
                                , deathAfterTherapy = False
                                }
                            )
            , test "decodes 1-star exclusion" <|
                \_ ->
                    let
                        json =
                            """{"relevance": "⭐", "reasoning": "No death reported.", "note": "", "death_after_therapy": false}"""
                    in
                    Decode.decodeString Classification.classificationResultDecoder json
                        |> Result.map .relevance
                        |> Expect.equal (Ok OneStar)
            , test "decodes death_after_therapy true" <|
                \_ ->
                    let
                        json =
                            """{"relevance": "⭐⭐⭐", "reasoning": "Patient died post-HSCT.", "note": "Excluded due to HSCT.", "death_after_therapy": true}"""
                    in
                    Decode.decodeString Classification.classificationResultDecoder json
                        |> Result.map .deathAfterTherapy
                        |> Expect.equal (Ok True)
            , test "fails on invalid relevance" <|
                \_ ->
                    let
                        json =
                            """{"relevance": "invalid", "reasoning": "test", "note": "", "death_after_therapy": false}"""
                    in
                    Decode.decodeString Classification.classificationResultDecoder json
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            , test "fails on missing field" <|
                \_ ->
                    let
                        json =
                            """{"relevance": "⭐", "reasoning": "test"}"""
                    in
                    Decode.decodeString Classification.classificationResultDecoder json
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]
        , describe "refusalResult"
            [ test "has 3-star relevance" <|
                \_ ->
                    Classification.refusalResult.relevance
                        |> Expect.equal ThreeStars
            , test "is flagged as refusal by isRefusal" <|
                \_ ->
                    Classification.isRefusal Classification.refusalResult
                        |> Expect.equal True
            , test "deathAfterTherapy is False" <|
                \_ ->
                    Classification.refusalResult.deathAfterTherapy
                        |> Expect.equal False
            ]
        , describe "isStarTag"
            [ test "recognizes all star levels" <|
                \_ ->
                    [ "⭐", "⭐⭐", "⭐⭐⭐", "⭐⭐⭐⭐", "⭐⭐⭐⭐⭐" ]
                        |> List.all Classification.isStarTag
                        |> Expect.equal True
            , test "rejects CLAUDE tag" <|
                \_ ->
                    Classification.isStarTag "CLAUDE"
                        |> Expect.equal False
            , test "rejects death_after_therapy tag" <|
                \_ ->
                    Classification.isStarTag "death_after_therapy"
                        |> Expect.equal False
            , test "rejects empty string" <|
                \_ ->
                    Classification.isStarTag ""
                        |> Expect.equal False
            , test "rejects partial star string" <|
                \_ ->
                    Classification.isStarTag "⭐⭐⭐⭐⭐⭐"
                        |> Expect.equal False
            ]
        , describe "isRefusal"
            [ test "normal result is not refusal" <|
                \_ ->
                    Classification.isRefusal
                        { relevance = FiveStars
                        , reasoning = "Relevant article"
                        , note = "Check full text"
                        , deathAfterTherapy = False
                        }
                        |> Expect.equal False
            , test "detects refusal by 'Needs human screening.' note" <|
                \_ ->
                    Classification.isRefusal
                        { relevance = FourStars
                        , reasoning = "Normal-looking reasoning"
                        , note = "Needs human screening."
                        , deathAfterTherapy = False
                        }
                        |> Expect.equal True
            ]
        , describe "userPrompt"
            [ test "formats title, abstract, keywords" <|
                \_ ->
                    Classification.userPrompt
                        { title = "My Title"
                        , abstract = "My Abstract"
                        , keywords = "kw1, kw2"
                        }
                        |> Expect.equal "TITLE: My Title\n\nABSTRACT: My Abstract\n\nKEYWORDS: kw1, kw2"
            , test "handles empty fields" <|
                \_ ->
                    Classification.userPrompt
                        { title = ""
                        , abstract = ""
                        , keywords = ""
                        }
                        |> Expect.equal "TITLE: \n\nABSTRACT: \n\nKEYWORDS: "
            ]
        , describe "relevanceToInt"
            [ test "OneStar is 1" <|
                \_ -> Classification.relevanceToInt OneStar |> Expect.equal 1
            , test "TwoStars is 2" <|
                \_ -> Classification.relevanceToInt TwoStars |> Expect.equal 2
            , test "ThreeStars is 3" <|
                \_ -> Classification.relevanceToInt ThreeStars |> Expect.equal 3
            , test "FourStars is 4" <|
                \_ -> Classification.relevanceToInt FourStars |> Expect.equal 4
            , test "FiveStars is 5" <|
                \_ -> Classification.relevanceToInt FiveStars |> Expect.equal 5
            ]
        , describe "intToRelevance"
            [ test "1 is OneStar" <|
                \_ -> Classification.intToRelevance 1 |> Expect.equal (Just OneStar)
            , test "5 is FiveStars" <|
                \_ -> Classification.intToRelevance 5 |> Expect.equal (Just FiveStars)
            , test "0 is Nothing" <|
                \_ -> Classification.intToRelevance 0 |> Expect.equal Nothing
            , test "6 is Nothing" <|
                \_ -> Classification.intToRelevance 6 |> Expect.equal Nothing
            , test "roundtrip for all relevances" <|
                \_ ->
                    [ OneStar, TwoStars, ThreeStars, FourStars, FiveStars ]
                        |> List.map (\r -> Classification.relevanceToInt r |> Classification.intToRelevance)
                        |> Expect.equal
                            [ Just OneStar, Just TwoStars, Just ThreeStars, Just FourStars, Just FiveStars ]
            ]
        ]
