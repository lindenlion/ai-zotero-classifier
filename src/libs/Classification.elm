module Classification exposing
    ( ClassificationResult
    , Decision(..)
    , Relevance(..)
    , classificationResultDecoder
    , decisionToString
    , emojiToRelevance
    , intToRelevance
    , isRefusal
    , isStarTag
    , refusalResult
    , relevanceToDecision
    , relevanceToEmoji
    , relevanceToInt
    , userPrompt
    )

import Json.Decode as Decode exposing (Decoder)


type Relevance
    = OneStar
    | TwoStars
    | ThreeStars
    | FourStars
    | FiveStars


type Decision
    = Include
    | Exclude


type alias ClassificationResult =
    { relevance : Relevance
    , reasoning : String
    , note : String
    , deathAfterTherapy : Bool
    }


relevanceToEmoji : Relevance -> String
relevanceToEmoji relevance =
    case relevance of
        OneStar ->
            "⭐"

        TwoStars ->
            "⭐⭐"

        ThreeStars ->
            "⭐⭐⭐"

        FourStars ->
            "⭐⭐⭐⭐"

        FiveStars ->
            "⭐⭐⭐⭐⭐"


emojiToRelevance : String -> Maybe Relevance
emojiToRelevance emoji =
    case emoji of
        "⭐" ->
            Just OneStar

        "⭐⭐" ->
            Just TwoStars

        "⭐⭐⭐" ->
            Just ThreeStars

        "⭐⭐⭐⭐" ->
            Just FourStars

        "⭐⭐⭐⭐⭐" ->
            Just FiveStars

        _ ->
            Nothing


relevanceToInt : Relevance -> Int
relevanceToInt relevance =
    case relevance of
        OneStar ->
            1

        TwoStars ->
            2

        ThreeStars ->
            3

        FourStars ->
            4

        FiveStars ->
            5


intToRelevance : Int -> Maybe Relevance
intToRelevance n =
    case n of
        1 ->
            Just OneStar

        2 ->
            Just TwoStars

        3 ->
            Just ThreeStars

        4 ->
            Just FourStars

        5 ->
            Just FiveStars

        _ ->
            Nothing


relevanceToDecision : Relevance -> Decision
relevanceToDecision relevance =
    case relevance of
        OneStar ->
            Exclude

        TwoStars ->
            Exclude

        ThreeStars ->
            Include

        FourStars ->
            Include

        FiveStars ->
            Include


decisionToString : Decision -> String
decisionToString decision =
    case decision of
        Include ->
            "INCLUDE"

        Exclude ->
            "EXCLUDE"


isStarTag : String -> Bool
isStarTag tag =
    List.member tag [ "⭐", "⭐⭐", "⭐⭐⭐", "⭐⭐⭐⭐", "⭐⭐⭐⭐⭐" ]


isRefusal : ClassificationResult -> Bool
isRefusal result =
    result.note == "Needs human screening."


refusalResult : ClassificationResult
refusalResult =
    { relevance = ThreeStars
    , reasoning = "AI model (Claude Opus 4.6) has refused to process this article, most likely because it triggered some internal content safety rule."
    , note = "Needs human screening."
    , deathAfterTherapy = False
    }


classificationResultDecoder : Decoder ClassificationResult
classificationResultDecoder =
    Decode.map4 ClassificationResult
        (Decode.field "relevance" relevanceDecoder)
        (Decode.field "reasoning" Decode.string)
        (Decode.field "note" Decode.string)
        (Decode.field "death_after_therapy" Decode.bool)


relevanceDecoder : Decoder Relevance
relevanceDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case emojiToRelevance str of
                    Just rel ->
                        Decode.succeed rel

                    Nothing ->
                        Decode.fail ("Unknown relevance value: " ++ str)
            )


userPrompt : { title : String, abstract : String, keywords : String } -> String
userPrompt article =
    "TITLE: "
        ++ article.title
        ++ "\n\nABSTRACT: "
        ++ article.abstract
        ++ "\n\nKEYWORDS: "
        ++ article.keywords
