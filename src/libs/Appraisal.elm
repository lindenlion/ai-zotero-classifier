module Appraisal exposing
    ( ASReviewData
    , AppraisalData
    , ProviderAppraisal
    , decode
    , empty
    , encode
    , fromClassificationResult
    , generateNoteHtml
    , migrateFromLegacy
    , setAppraisal
    )

import Classification exposing (ClassificationResult, Decision(..))
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias AppraisalData =
    { version : Int
    , appraisals : Dict String ProviderAppraisal
    , asreview : Maybe ASReviewData
    }


type alias ProviderAppraisal =
    { relevance : Int
    , decision : Bool
    , reasoning : String
    , note : String
    , deathAfterTherapy : Bool
    , isRefusal : Bool
    , model : String
    , timestamp : String
    }


type alias ASReviewData =
    { decision : Bool
    , user : Int
    , timestamp : String
    , tags : Dict String Bool
    , ranking : String
    , note : String
    }


empty : AppraisalData
empty =
    { version = 1
    , appraisals = Dict.empty
    , asreview = Nothing
    }



-- Encode / Decode


encode : AppraisalData -> Encode.Value
encode data =
    Encode.object
        ([ ( "v", Encode.int data.version )
         , ( "appraisals", Encode.dict identity encodeProviderAppraisal data.appraisals )
         ]
            ++ (case data.asreview of
                    Just asr ->
                        [ ( "asreview", encodeASReviewData asr ) ]

                    Nothing ->
                        []
               )
        )


encodeProviderAppraisal : ProviderAppraisal -> Encode.Value
encodeProviderAppraisal pa =
    Encode.object
        [ ( "relevance", Encode.int pa.relevance )
        , ( "decision", Encode.bool pa.decision )
        , ( "reasoning", Encode.string pa.reasoning )
        , ( "note", Encode.string pa.note )
        , ( "deathAfterTherapy", Encode.bool pa.deathAfterTherapy )
        , ( "isRefusal", Encode.bool pa.isRefusal )
        , ( "model", Encode.string pa.model )
        , ( "timestamp", Encode.string pa.timestamp )
        ]


encodeASReviewData : ASReviewData -> Encode.Value
encodeASReviewData asr =
    Encode.object
        [ ( "decision", Encode.bool asr.decision )
        , ( "user", Encode.int asr.user )
        , ( "timestamp", Encode.string asr.timestamp )
        , ( "tags", Encode.dict identity Encode.bool asr.tags )
        , ( "ranking", Encode.string asr.ranking )
        , ( "note", Encode.string asr.note )
        ]


decode : Decoder AppraisalData
decode =
    Decode.map3 AppraisalData
        (Decode.field "v" Decode.int)
        (Decode.field "appraisals" (Decode.dict providerAppraisalDecoder))
        (Decode.maybe (Decode.field "asreview" asReviewDataDecoder))


providerAppraisalDecoder : Decoder ProviderAppraisal
providerAppraisalDecoder =
    Decode.map8 ProviderAppraisal
        (Decode.field "relevance" Decode.int)
        (Decode.field "decision" Decode.bool)
        (Decode.field "reasoning" Decode.string)
        (Decode.field "note" Decode.string)
        (Decode.field "deathAfterTherapy" Decode.bool)
        (Decode.field "isRefusal" Decode.bool)
        (Decode.field "model" Decode.string)
        (Decode.field "timestamp" Decode.string)


asReviewDataDecoder : Decoder ASReviewData
asReviewDataDecoder =
    Decode.map6 ASReviewData
        (Decode.field "decision" Decode.bool)
        (Decode.field "user" Decode.int)
        (Decode.field "timestamp" Decode.string)
        (Decode.field "tags" (Decode.dict Decode.bool)
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault Dict.empty)
        )
        (Decode.field "ranking" Decode.string
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault "")
        )
        (Decode.field "note" Decode.string
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault "")
        )



-- Conversion


fromClassificationResult : { model : String, timestamp : String } -> ClassificationResult -> ProviderAppraisal
fromClassificationResult meta result =
    let
        decision =
            Classification.relevanceToDecision result.relevance
    in
    { relevance = Classification.relevanceToInt result.relevance
    , decision = decision == Include
    , reasoning = result.reasoning
    , note = result.note
    , deathAfterTherapy = result.deathAfterTherapy
    , isRefusal = Classification.isRefusal result
    , model = meta.model
    , timestamp = meta.timestamp
    }


setAppraisal : String -> ProviderAppraisal -> AppraisalData -> AppraisalData
setAppraisal provider appraisal data =
    { data | appraisals = Dict.insert provider appraisal data.appraisals }



-- Note generation


generateNoteHtml : AppraisalData -> String
generateNoteHtml data =
    let
        disclaimer =
            "<p><em>This note is auto-generated from structured data in the Call Number field. Do not edit manually.</em></p>"

        providerSections =
            data.appraisals
                |> Dict.toList
                |> List.map providerToHtml
                |> String.join "<hr/>"

        asreviewSection =
            case data.asreview of
                Just asr ->
                    "<hr/>" ++ asreviewToHtml asr

                Nothing ->
                    ""
    in
    disclaimer ++ "<hr/>" ++ providerSections ++ asreviewSection


providerToHtml : ( String, ProviderAppraisal ) -> String
providerToHtml ( name, pa ) =
    let
        stars =
            String.repeat pa.relevance "⭐"

        decisionStr =
            if pa.decision then
                "INCLUDE"

            else
                "EXCLUDE"

        header =
            "<h3>" ++ name ++ " (" ++ pa.model ++ ")</h3>"

        decisionLine =
            "<p><strong>Decision:</strong> " ++ decisionStr ++ " (" ++ stars ++ ")</p>"

        todoLine =
            if pa.decision && pa.note /= "" then
                "<p><strong>Todo:</strong> " ++ pa.note ++ "</p>"

            else
                ""

        reasoningLine =
            "<p><em>Reasoning: " ++ pa.reasoning ++ "</em></p>"

        timestampLine =
            if pa.timestamp /= "" then
                "<p><small>" ++ pa.timestamp ++ "</small></p>"

            else
                ""

        refusalLine =
            if pa.isRefusal then
                "<p><strong>⚠ Refusal — needs human screening</strong></p>"

            else
                ""
    in
    header ++ decisionLine ++ refusalLine ++ todoLine ++ reasoningLine ++ timestampLine


asreviewToHtml : ASReviewData -> String
asreviewToHtml asr =
    let
        decisionStr =
            if asr.decision then
                "INCLUDE"

            else
                "EXCLUDE"
    in
    "<h3>ASReview (user "
        ++ String.fromInt asr.user
        ++ ")</h3>"
        ++ "<p><strong>Decision:</strong> "
        ++ decisionStr
        ++ "</p>"
        ++ (if asr.note /= "" then
                "<p>" ++ asr.note ++ "</p>"

            else
                ""
           )
        ++ (if asr.timestamp /= "" then
                "<p><small>" ++ asr.timestamp ++ "</small></p>"

            else
                ""
           )



-- Migration from legacy (v0.1.x) format


migrateFromLegacy :
    { tags : List { tag : String }
    , reasoningNoteHtml : Maybe String
    }
    -> Maybe AppraisalData
migrateFromLegacy { tags, reasoningNoteHtml } =
    let
        hasClaude =
            List.any (\t -> t.tag == "CLAUDE") tags

        starTag =
            tags
                |> List.filter (\t -> Classification.isStarTag t.tag)
                |> List.head
                |> Maybe.andThen (\t -> Classification.emojiToRelevance t.tag)

        deathAfterTherapy =
            List.any (\t -> t.tag == "death_after_therapy") tags
    in
    if hasClaude then
        let
            relevanceInt =
                starTag
                    |> Maybe.map Classification.relevanceToInt
                    |> Maybe.withDefault 3

            decision =
                starTag
                    |> Maybe.map Classification.relevanceToDecision
                    |> Maybe.map (\d -> d == Include)
                    |> Maybe.withDefault True

            ( reasoning, note ) =
                case reasoningNoteHtml of
                    Just html ->
                        ( extractReasoning html, extractTodoNote html )

                    Nothing ->
                        ( "", "" )

            appraisal =
                { relevance = relevanceInt
                , decision = decision
                , reasoning = reasoning
                , note = note
                , deathAfterTherapy = deathAfterTherapy
                , isRefusal = False
                , model = "opus-4-6"
                , timestamp = "31 March 2026"
                }
        in
        Just { version = 1, appraisals = Dict.singleton "claude" appraisal, asreview = Nothing }

    else
        Nothing


extractReasoning : String -> String
extractReasoning html =
    let
        tryMarker marker =
            case String.split marker html of
                _ :: rest :: _ ->
                    rest
                        |> String.split "</em>"
                        |> List.head
                        |> Maybe.withDefault ""
                        |> String.trim

                _ ->
                    ""

        inclusion =
            tryMarker "Inclusion reasoning: "

        exclusion =
            tryMarker "Exclusion reasoning: "
    in
    if inclusion /= "" then
        inclusion

    else
        exclusion


extractTodoNote : String -> String
extractTodoNote html =
    if String.contains "Todo:" html then
        case String.split "Todo:</strong></p><p>" html of
            _ :: rest :: _ ->
                rest
                    |> String.split "</p>"
                    |> List.head
                    |> Maybe.withDefault ""
                    |> String.trim

            _ ->
                ""

    else
        ""
