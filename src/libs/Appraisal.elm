module Appraisal exposing
    ( ASReviewData
    , AppraisalData
    , ProviderAppraisal
    , currentSchemaVersion
    , decode
    , empty
    , encode
    , fromClassificationResult
    , generateNoteHtml
    , migrateFromLegacy
    , migrateToCurrentVersion
    , needsMigration
    , renameKeys
    , setAppraisal
    )

import Analysis
import Classification exposing (ClassificationResult, Decision(..))
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias AppraisalData =
    { version : Int
    , appraisals : Dict String ProviderAppraisal
    , asreview : Maybe ASReviewData
    , analysis : Maybe Analysis.AnalysisData
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
    , renamedFrom : Maybe String
    }


type alias ASReviewData =
    { decision : Bool
    , user : Int
    , timestamp : String
    , tags : Dict String Bool
    , ranking : String
    , note : String
    }


currentSchemaVersion : Int
currentSchemaVersion =
    3


empty : AppraisalData
empty =
    { version = currentSchemaVersion
    , appraisals = Dict.empty
    , asreview = Nothing
    , analysis = Nothing
    }


needsMigration : AppraisalData -> Bool
needsMigration data =
    data.version < currentSchemaVersion


{-| Upgrade AppraisalData from any older version to current.
-}
migrateToCurrentVersion : AppraisalData -> AppraisalData
migrateToCurrentVersion data =
    if data.version >= currentSchemaVersion then
        data

    else
        data
            |> migrateV1toV2
            |> migrateV2toV3


migrateV1toV2 : AppraisalData -> AppraisalData
migrateV1toV2 data =
    if data.version >= 2 then
        data

    else
        { data | version = 2, analysis = Nothing }


{-| v2 -> v3: Add renamedFrom field (Nothing) to all existing appraisals.
-}
migrateV2toV3 : AppraisalData -> AppraisalData
migrateV2toV3 data =
    if data.version >= 3 then
        data

    else
        { data
            | version = 3
            , appraisals =
                Dict.map (\_ a -> { a | renamedFrom = Nothing }) data.appraisals
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
            ++ (case data.analysis of
                    Just analysis ->
                        [ ( "analysis", Analysis.encode analysis ) ]

                    Nothing ->
                        []
               )
        )


encodeProviderAppraisal : ProviderAppraisal -> Encode.Value
encodeProviderAppraisal pa =
    Encode.object
        ([ ( "relevance", Encode.int pa.relevance )
         , ( "decision", Encode.bool pa.decision )
         , ( "reasoning", Encode.string pa.reasoning )
         , ( "note", Encode.string pa.note )
         , ( "deathAfterTherapy", Encode.bool pa.deathAfterTherapy )
         , ( "isRefusal", Encode.bool pa.isRefusal )
         , ( "model", Encode.string pa.model )
         , ( "timestamp", Encode.string pa.timestamp )
         ]
            ++ (case pa.renamedFrom of
                    Just oldKey ->
                        [ ( "renamedFrom", Encode.string oldKey ) ]

                    Nothing ->
                        []
               )
        )


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
    Decode.map4 AppraisalData
        (Decode.field "v" Decode.int)
        (Decode.field "appraisals" (Decode.dict providerAppraisalDecoder))
        (Decode.maybe (Decode.field "asreview" asReviewDataDecoder))
        (Decode.maybe (Decode.field "analysis" Analysis.decode))


providerAppraisalDecoder : Decoder ProviderAppraisal
providerAppraisalDecoder =
    Decode.map8
        (\rel dec reas note death ref model ts ->
            ProviderAppraisal rel dec reas note death ref model ts
        )
        (Decode.field "relevance" Decode.int)
        (Decode.field "decision" Decode.bool)
        (Decode.field "reasoning" Decode.string)
        (Decode.field "note" Decode.string)
        (Decode.field "deathAfterTherapy" Decode.bool)
        (Decode.field "isRefusal" Decode.bool)
        (Decode.field "model" Decode.string)
        (Decode.field "timestamp" Decode.string)
        |> andMap (Decode.maybe (Decode.field "renamedFrom" Decode.string))


{-| Apply an additional decoder to a partially-applied decoder (pipeline style).
-}
andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap argDecoder funcDecoder =
    Decode.map2 (\f a -> f a) funcDecoder argDecoder


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
    , renamedFrom = Nothing
    }


setAppraisal : String -> ProviderAppraisal -> AppraisalData -> AppraisalData
setAppraisal provider appraisal data =
    { data | appraisals = Dict.insert provider appraisal data.appraisals }


{-| Rename appraisal keys based on a list of rename instructions.
Only renames if:
  - The old key exists
  - The new key doesn't (to avoid overwriting)
  - The appraisal's timestamp is before the cutoff (so newer appraisals under the same key are left alone)
-}
renameKeys : List { oldKey : String, newKey : String, before : String } -> AppraisalData -> AppraisalData
renameKeys renames data =
    let
        applyRename rename appraisals =
            case Dict.get rename.oldKey appraisals of
                Just appraisal ->
                    if Dict.member rename.newKey appraisals then
                        appraisals

                    else if appraisal.timestamp < rename.before then
                        appraisals
                            |> Dict.remove rename.oldKey
                            |> Dict.insert rename.newKey { appraisal | renamedFrom = Just rename.oldKey }

                    else
                        appraisals

                Nothing ->
                    appraisals
    in
    { data | appraisals = List.foldl applyRename data.appraisals renames }



-- Note generation


generateNoteHtml : AppraisalData -> String
generateNoteHtml data =
    let
        disclaimer =
            "<p><em>This note is auto-generated from structured data in the Call Number field. Do not edit manually.</em></p>"

        analysisSection =
            case data.analysis of
                Just analysis ->
                    analysisToHtml analysis

                Nothing ->
                    ""

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
    disclaimer ++ analysisSection ++ "<hr/>" ++ providerSections ++ asreviewSection


analysisToHtml : Analysis.AnalysisData -> String
analysisToHtml analysis =
    let
        categoryLabel =
            case analysis.category of
                Analysis.AutoExcluded ->
                    "Auto-excluded"

                Analysis.HumanReview ->
                    "Human review"

                Analysis.AutoIncluded ->
                    "Auto-included"

        tag =
            Analysis.categoryToTag analysis
    in
    "<hr/><h3>Decision Analysis</h3>"
        ++ "<p><strong>Category:</strong> "
        ++ categoryLabel
        ++ " "
        ++ tag
        ++ "</p>"
        ++ "<p>Total stars: "
        ++ String.fromInt analysis.totalStars
        ++ " | Inclusions: "
        ++ String.fromInt analysis.inclusions
        ++ " | Exclusions: "
        ++ String.fromInt analysis.exclusions
        ++ "</p>"


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

        renamedLine =
            case pa.renamedFrom of
                Just oldKey ->
                    "<p><small>Renamed from: " ++ oldKey ++ "</small></p>"

                Nothing ->
                    ""

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
    header ++ renamedLine ++ decisionLine ++ refusalLine ++ todoLine ++ reasoningLine ++ timestampLine


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

            classificationResult =
                { relevance = starTag |> Maybe.withDefault Classification.ThreeStars
                , reasoning = reasoning
                , note = note
                , deathAfterTherapy = deathAfterTherapy
                }

            appraisal =
                { relevance = relevanceInt
                , decision = decision
                , reasoning = reasoning
                , note = note
                , deathAfterTherapy = deathAfterTherapy
                , isRefusal = Classification.isRefusal classificationResult
                , model = "opus-4-6"
                , timestamp = "31 March 2026"
                , renamedFrom = Nothing
                }
        in
        Just { version = 2, appraisals = Dict.singleton "claude" appraisal, asreview = Nothing, analysis = Nothing }

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
        let
            -- Try format: Todo:</strong></p><p>...content...</p><p><em>reasoning
            splitOnBlock =
                case String.split "Todo:</strong></p><p>" html of
                    _ :: rest :: _ ->
                        rest
                            |> String.split "<p><em>"
                            |> List.head
                            |> Maybe.withDefault ""
                            |> stripTrailingCloseTags
                            |> String.trim

                    _ ->
                        ""

            -- Try format: Todo:</strong> ...content...</p>
            splitOnInline =
                case String.split "Todo:</strong>" html of
                    _ :: rest :: _ ->
                        rest
                            |> String.split "<p><em>"
                            |> List.head
                            |> Maybe.withDefault ""
                            |> stripTrailingCloseTags
                            |> String.trim

                    _ ->
                        ""
        in
        if splitOnBlock /= "" then
            splitOnBlock

        else
            splitOnInline

    else
        ""


{-| Strip trailing </p> and </div> tags from extracted content.
-}
stripTrailingCloseTags : String -> String
stripTrailingCloseTags str =
    str
        |> String.trimRight
        |> stripSuffix "</p>"
        |> stripSuffix "</div>"
        |> String.trimRight


stripSuffix : String -> String -> String
stripSuffix suffix str =
    if String.endsWith suffix str then
        String.dropRight (String.length suffix) str

    else
        str
