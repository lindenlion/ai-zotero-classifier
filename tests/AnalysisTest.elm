module AnalysisTest exposing (..)

import Analysis exposing (AnalysisCategory(..))
import Dict
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)


{-| Helper to build a minimal appraisal record for Analysis.compute.
-}
appraisal : Int -> Bool -> { relevance : Int, decision : Bool }
appraisal relevance decision =
    { relevance = relevance, decision = decision }


suite : Test
suite =
    describe "Analysis"
        [ describe "compute — category rules"
            [ test "auto-excluded: 3 stars, 3 exclusions (1+1+1)" <|
                \_ ->
                    let
                        result =
                            Analysis.compute
                                (Dict.fromList
                                    [ ( "a", appraisal 1 False )
                                    , ( "b", appraisal 1 False )
                                    , ( "c", appraisal 1 False )
                                    ]
                                )
                    in
                    Expect.all
                        [ \r -> Expect.equal 3 r.totalStars
                        , \r -> Expect.equal 0 r.inclusions
                        , \r -> Expect.equal 3 r.exclusions
                        , \r -> Expect.equal AutoExcluded r.category
                        ]
                        result
            , test "auto-excluded: 5 stars, 3 exclusions (1+2+2)" <|
                \_ ->
                    Analysis.compute
                        (Dict.fromList
                            [ ( "a", appraisal 1 False )
                            , ( "b", appraisal 2 False )
                            , ( "c", appraisal 2 False )
                            ]
                        )
                        |> .category
                        |> Expect.equal AutoExcluded
            , test "human review: 5 stars, 2 exclusions — the edge case (3+1+1)" <|
                \_ ->
                    let
                        result =
                            Analysis.compute
                                (Dict.fromList
                                    [ ( "a", appraisal 3 True )
                                    , ( "b", appraisal 1 False )
                                    , ( "c", appraisal 1 False )
                                    ]
                                )
                    in
                    Expect.all
                        [ \r -> Expect.equal 5 r.totalStars
                        , \r -> Expect.equal 1 r.inclusions
                        , \r -> Expect.equal 2 r.exclusions
                        , \r -> Expect.equal HumanReview r.category
                        ]
                        result
            , test "human review: 6 stars, 3 exclusions (2+2+2)" <|
                \_ ->
                    Analysis.compute
                        (Dict.fromList
                            [ ( "a", appraisal 2 False )
                            , ( "b", appraisal 2 False )
                            , ( "c", appraisal 2 False )
                            ]
                        )
                        |> .category
                        |> Expect.equal HumanReview
            , test "auto-included: 9 stars, 0 exclusions (3+3+3)" <|
                \_ ->
                    Analysis.compute
                        (Dict.fromList
                            [ ( "a", appraisal 3 True )
                            , ( "b", appraisal 3 True )
                            , ( "c", appraisal 3 True )
                            ]
                        )
                        |> .category
                        |> Expect.equal AutoIncluded
            , test "human review: 10 stars, 1 exclusion (5+3+2)" <|
                \_ ->
                    Analysis.compute
                        (Dict.fromList
                            [ ( "a", appraisal 5 True )
                            , ( "b", appraisal 3 True )
                            , ( "c", appraisal 2 False )
                            ]
                        )
                        |> .category
                        |> Expect.equal HumanReview
            , test "human review: 12 stars, 1 exclusion (5+5+2)" <|
                \_ ->
                    Analysis.compute
                        (Dict.fromList
                            [ ( "a", appraisal 5 True )
                            , ( "b", appraisal 5 True )
                            , ( "c", appraisal 2 False )
                            ]
                        )
                        |> .category
                        |> Expect.equal HumanReview
            , test "auto-included: 10 stars, 0 exclusions (3+3+4)" <|
                \_ ->
                    Analysis.compute
                        (Dict.fromList
                            [ ( "a", appraisal 3 True )
                            , ( "b", appraisal 3 True )
                            , ( "c", appraisal 4 True )
                            ]
                        )
                        |> .category
                        |> Expect.equal AutoIncluded
            , test "auto-included: 15 stars, 0 exclusions (5+5+5)" <|
                \_ ->
                    Analysis.compute
                        (Dict.fromList
                            [ ( "a", appraisal 5 True )
                            , ( "b", appraisal 5 True )
                            , ( "c", appraisal 5 True )
                            ]
                        )
                        |> .category
                        |> Expect.equal AutoIncluded
            , test "boundary: exactly 10 stars, 0 exclusions is auto-included" <|
                \_ ->
                    Analysis.compute
                        (Dict.fromList
                            [ ( "a", appraisal 4 True )
                            , ( "b", appraisal 3 True )
                            , ( "c", appraisal 3 True )
                            ]
                        )
                        |> .category
                        |> Expect.equal AutoIncluded
            , test "boundary: exactly 8 stars, 0 exclusions is human review" <|
                \_ ->
                    Analysis.compute
                        (Dict.fromList
                            [ ( "a", appraisal 3 True )
                            , ( "b", appraisal 3 True )
                            , ( "c", appraisal 2 False )
                            ]
                        )
                        |> .category
                        |> Expect.equal HumanReview
            , test "boundary: exactly 9 stars, 0 exclusions is auto-included" <|
                \_ ->
                    Analysis.compute
                        (Dict.fromList
                            [ ( "a", appraisal 3 True )
                            , ( "b", appraisal 3 True )
                            , ( "c", appraisal 3 True )
                            ]
                        )
                        |> .category
                        |> Expect.equal AutoIncluded
            ]
        , describe "categoryToTag"
            [ test "auto-excluded gets ❌" <|
                \_ ->
                    Analysis.categoryToTag { totalStars = 3, inclusions = 0, exclusions = 3, category = AutoExcluded }
                        |> Expect.equal "❌"
            , test "auto-included gets ✅" <|
                \_ ->
                    Analysis.categoryToTag { totalStars = 15, inclusions = 3, exclusions = 0, category = AutoIncluded }
                        |> Expect.equal "✅"
            , test "human review with 0 exclusions gets ✅ (constructed data)" <|
                \_ ->
                    -- While 0 exclusions + human review can't occur with 3 models (min sum = 9 → auto-included),
                    -- the tag function handles it correctly regardless
                    Analysis.categoryToTag { totalStars = 7, inclusions = 3, exclusions = 0, category = HumanReview }
                        |> Expect.equal "✅"
            , test "human review with 1 exclusion gets ⭕" <|
                \_ ->
                    Analysis.categoryToTag { totalStars = 7, inclusions = 2, exclusions = 1, category = HumanReview }
                        |> Expect.equal "⭕"
            , test "human review with 2 exclusions gets ⭕⭕" <|
                \_ ->
                    Analysis.categoryToTag { totalStars = 5, inclusions = 1, exclusions = 2, category = HumanReview }
                        |> Expect.equal "⭕⭕"
            , test "human review with 3 exclusions gets ⭕⭕⭕" <|
                \_ ->
                    Analysis.categoryToTag { totalStars = 6, inclusions = 0, exclusions = 3, category = HumanReview }
                        |> Expect.equal "⭕⭕⭕"
            ]
        , describe "categoryToCollectionName"
            [ test "auto-excluded" <|
                \_ ->
                    Analysis.categoryToCollectionName { totalStars = 3, inclusions = 0, exclusions = 3, category = AutoExcluded }
                        |> Expect.equal "AI auto-excluded"
            , test "auto-included" <|
                \_ ->
                    Analysis.categoryToCollectionName { totalStars = 15, inclusions = 3, exclusions = 0, category = AutoIncluded }
                        |> Expect.equal "AI auto-included"
            , test "human review with 7 stars" <|
                \_ ->
                    Analysis.categoryToCollectionName { totalStars = 7, inclusions = 2, exclusions = 1, category = HumanReview }
                        |> Expect.equal "Sum of 7 stars"
            , test "human review with 12 stars" <|
                \_ ->
                    Analysis.categoryToCollectionName { totalStars = 12, inclusions = 2, exclusions = 1, category = HumanReview }
                        |> Expect.equal "Sum of 12 stars"
            ]
        , describe "isAnalysisTag"
            [ test "recognises ✅" <|
                \_ -> Analysis.isAnalysisTag "✅" |> Expect.equal True
            , test "recognises ❌" <|
                \_ -> Analysis.isAnalysisTag "❌" |> Expect.equal True
            , test "recognises ⭕" <|
                \_ -> Analysis.isAnalysisTag "⭕" |> Expect.equal True
            , test "recognises ⭕⭕" <|
                \_ -> Analysis.isAnalysisTag "⭕⭕" |> Expect.equal True
            , test "recognises ⭕⭕⭕" <|
                \_ -> Analysis.isAnalysisTag "⭕⭕⭕" |> Expect.equal True
            , test "rejects star tag" <|
                \_ -> Analysis.isAnalysisTag "⭐⭐⭐" |> Expect.equal False
            , test "rejects random text" <|
                \_ -> Analysis.isAnalysisTag "hello" |> Expect.equal False
            ]
        , describe "encode/decode roundtrip"
            [ test "auto-excluded roundtrip" <|
                \_ ->
                    let
                        data =
                            { totalStars = 4, inclusions = 0, exclusions = 3, category = AutoExcluded }
                    in
                    data
                        |> Analysis.encode
                        |> Encode.encode 0
                        |> Decode.decodeString Analysis.decode
                        |> Expect.equal (Ok data)
            , test "human-review roundtrip" <|
                \_ ->
                    let
                        data =
                            { totalStars = 8, inclusions = 2, exclusions = 1, category = HumanReview }
                    in
                    data
                        |> Analysis.encode
                        |> Encode.encode 0
                        |> Decode.decodeString Analysis.decode
                        |> Expect.equal (Ok data)
            , test "auto-included roundtrip" <|
                \_ ->
                    let
                        data =
                            { totalStars = 13, inclusions = 3, exclusions = 0, category = AutoIncluded }
                    in
                    data
                        |> Analysis.encode
                        |> Encode.encode 0
                        |> Decode.decodeString Analysis.decode
                        |> Expect.equal (Ok data)
            ]
        ]
