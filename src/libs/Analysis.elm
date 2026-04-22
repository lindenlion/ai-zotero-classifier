module Analysis exposing
    ( AnalysisCategory(..)
    , AnalysisData
    , categoryToCollectionName
    , categoryToTag
    , compute
    , decode
    , encode
    , isAnalysisTag
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type AnalysisCategory
    = AutoExcluded
    | HumanReview
    | AutoIncluded


type alias AnalysisData =
    { totalStars : Int
    , inclusions : Int
    , exclusions : Int
    , category : AnalysisCategory
    }


{-| Compute decision analysis from a complete set of provider appraisals.
Expects appraisals as (relevance : Int, decision : Bool) pairs extracted from ProviderAppraisal.
-}
compute : Dict String { a | relevance : Int, decision : Bool } -> AnalysisData
compute appraisals =
    let
        values =
            Dict.values appraisals

        totalStars =
            values |> List.map .relevance |> List.sum

        inclusions =
            values |> List.filter .decision |> List.length

        exclusions =
            List.length values - inclusions

        category =
            categorise totalStars exclusions
    in
    { totalStars = totalStars
    , inclusions = inclusions
    , exclusions = exclusions
    , category = category
    }


{-| Determine the analysis category from total stars and exclusion count.

  - Auto-excluded: ≤5 stars AND 3 exclusions
  - Auto-included: ≥9 stars AND 0 exclusions
  - Human review: everything else

-}
categorise : Int -> Int -> AnalysisCategory
categorise totalStars exclusions =
    if totalStars <= 5 && exclusions >= 3 then
        AutoExcluded

    else if totalStars >= 9 && exclusions == 0 then
        AutoIncluded

    else
        HumanReview


{-| The emoji tag for this analysis result.

  - Auto-excluded → ❌
  - Auto-included → ✅
  - Human review → ⭕ repeated by exclusion count (0 exclusions = ✅)

-}
categoryToTag : AnalysisData -> String
categoryToTag data =
    case data.category of
        AutoExcluded ->
            "❌"

        AutoIncluded ->
            "✅"

        HumanReview ->
            if data.exclusions == 0 then
                "✅"

            else
                String.repeat data.exclusions "⭕"


{-| The Zotero collection name for this analysis result.

  - Auto-excluded → "AI auto-excluded"
  - Auto-included → "AI auto-included"
  - Human review → "Sum of N stars"

-}
categoryToCollectionName : AnalysisData -> String
categoryToCollectionName data =
    case data.category of
        AutoExcluded ->
            "AI auto-excluded"

        AutoIncluded ->
            "AI auto-included"

        HumanReview ->
            "Sum of " ++ String.fromInt data.totalStars ++ " stars"


{-| All possible analysis tags (for tag cleaning).
-}
analysisTagList : List String
analysisTagList =
    [ "✅", "❌", "⭕", "⭕⭕", "⭕⭕⭕" ]


isAnalysisTag : String -> Bool
isAnalysisTag tag =
    List.member tag analysisTagList



-- JSON encode/decode


categoryToString : AnalysisCategory -> String
categoryToString cat =
    case cat of
        AutoExcluded ->
            "auto-excluded"

        HumanReview ->
            "human-review"

        AutoIncluded ->
            "auto-included"


stringToCategory : String -> Maybe AnalysisCategory
stringToCategory str =
    case str of
        "auto-excluded" ->
            Just AutoExcluded

        "human-review" ->
            Just HumanReview

        "auto-included" ->
            Just AutoIncluded

        _ ->
            Nothing


encode : AnalysisData -> Encode.Value
encode data =
    Encode.object
        [ ( "totalStars", Encode.int data.totalStars )
        , ( "inclusions", Encode.int data.inclusions )
        , ( "exclusions", Encode.int data.exclusions )
        , ( "category", Encode.string (categoryToString data.category) )
        ]


decode : Decoder AnalysisData
decode =
    Decode.map4 AnalysisData
        (Decode.field "totalStars" Decode.int)
        (Decode.field "inclusions" Decode.int)
        (Decode.field "exclusions" Decode.int)
        (Decode.field "category" categoryDecoder)


categoryDecoder : Decoder AnalysisCategory
categoryDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case stringToCategory str of
                    Just cat ->
                        Decode.succeed cat

                    Nothing ->
                        Decode.fail ("Unknown analysis category: " ++ str)
            )
