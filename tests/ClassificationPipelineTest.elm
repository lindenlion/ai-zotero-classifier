module ClassificationPipelineTest exposing (..)

{-| Integration-style tests using elm-program-test.
Tests the classification pipeline logic through a simulated TEA program
that models the same state transitions as the BackendTask script.
-}

import AnthropicApi
import Classification exposing (Decision(..), Relevance(..))
import Html
import Html.Attributes
import Json.Decode as Decode
import ProgramTest exposing (ProgramTest)
import Test exposing (..)
import Test.Html.Selector as Selector
import ZoteroApi


{-| Minimal TEA model that mirrors the script's processing pipeline.
-}
type alias Model =
    { items : List ZoteroApi.ArticleData
    , results : List ( String, Result String Classification.ClassificationResult )
    , stats : Stats
    , phase : Phase
    }


type Phase
    = Idle


type alias Stats =
    { processed : Int
    , included : Int
    , excluded : Int
    , errors : Int
    }


type Msg
    = GotClassificationResult String (Result String Classification.ClassificationResult)


init : () -> ( Model, Cmd Msg )
init () =
    ( { items = []
      , results = []
      , stats = { processed = 0, included = 0, excluded = 0, errors = 0 }
      , phase = Idle
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotClassificationResult itemKey result ->
            let
                newResults =
                    model.results ++ [ ( itemKey, result ) ]

                newStats =
                    case result of
                        Ok classResult ->
                            let
                                decision =
                                    Classification.relevanceToDecision classResult.relevance
                            in
                            case decision of
                                Include ->
                                    { processed = model.stats.processed + 1
                                    , included = model.stats.included + 1
                                    , excluded = model.stats.excluded
                                    , errors = model.stats.errors
                                    }

                                Exclude ->
                                    { processed = model.stats.processed + 1
                                    , included = model.stats.included
                                    , excluded = model.stats.excluded + 1
                                    , errors = model.stats.errors
                                    }

                        Err _ ->
                            { processed = model.stats.processed
                            , included = model.stats.included
                            , excluded = model.stats.excluded
                            , errors = model.stats.errors + 1
                            }
            in
            ( { model | results = newResults, stats = newStats }
            , Cmd.none
            )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.div [ Html.Attributes.id "stats" ]
            [ Html.text
                ("Processed: "
                    ++ String.fromInt model.stats.processed
                    ++ " Included: "
                    ++ String.fromInt model.stats.included
                    ++ " Excluded: "
                    ++ String.fromInt model.stats.excluded
                    ++ " Errors: "
                    ++ String.fromInt model.stats.errors
                )
            ]
        , Html.div [ Html.Attributes.id "results" ]
            (model.results
                |> List.map
                    (\( key, result ) ->
                        Html.div [ Html.Attributes.class "result" ]
                            [ Html.text
                                (key
                                    ++ ": "
                                    ++ (case result of
                                            Ok r ->
                                                Classification.relevanceToEmoji r.relevance
                                                    ++ " "
                                                    ++ Classification.decisionToString (Classification.relevanceToDecision r.relevance)

                                            Err e ->
                                                "ERROR: " ++ e
                                       )
                                )
                            ]
                    )
            )
        ]


start : ProgramTest Model Msg (Cmd Msg)
start =
    ProgramTest.createElement
        { init = init
        , update = update
        , view = view
        }
        |> ProgramTest.start ()


suite : Test
suite =
    describe "Classification Pipeline (elm-program-test)"
        [ test "processes a 5-star inclusion correctly" <|
            \_ ->
                start
                    |> ProgramTest.update
                        (GotClassificationResult "ITEM1"
                            (Ok
                                { relevance = FiveStars
                                , reasoning = "IEI patient died from infection"
                                , note = "Check full text"
                                , deathAfterTherapy = False
                                }
                            )
                        )
                    |> ProgramTest.expectViewHas
                        [ Selector.text "ITEM1: ⭐⭐⭐⭐⭐ INCLUDE" ]
        , test "processes a 1-star exclusion correctly" <|
            \_ ->
                start
                    |> ProgramTest.update
                        (GotClassificationResult "ITEM2"
                            (Ok
                                { relevance = OneStar
                                , reasoning = "No death reported"
                                , note = ""
                                , deathAfterTherapy = False
                                }
                            )
                        )
                    |> ProgramTest.expectViewHas
                        [ Selector.text "ITEM2: ⭐ EXCLUDE" ]
        , test "tracks stats for mixed results" <|
            \_ ->
                start
                    |> ProgramTest.update
                        (GotClassificationResult "A"
                            (Ok
                                { relevance = FiveStars
                                , reasoning = "r"
                                , note = "n"
                                , deathAfterTherapy = False
                                }
                            )
                        )
                    |> ProgramTest.update
                        (GotClassificationResult "B"
                            (Ok
                                { relevance = OneStar
                                , reasoning = "r"
                                , note = ""
                                , deathAfterTherapy = False
                                }
                            )
                        )
                    |> ProgramTest.update
                        (GotClassificationResult "C" (Err "API error"))
                    |> ProgramTest.expectViewHas
                        [ Selector.text "Processed: 2 Included: 1 Excluded: 1 Errors: 1" ]
        , test "handles refusal as 3-star include" <|
            \_ ->
                start
                    |> ProgramTest.update
                        (GotClassificationResult "REF1"
                            (Ok Classification.refusalResult)
                        )
                    |> ProgramTest.expectViewHas
                        [ Selector.text "REF1: ⭐⭐⭐ INCLUDE" ]
        , test "handles error results" <|
            \_ ->
                start
                    |> ProgramTest.update
                        (GotClassificationResult "ERR1" (Err "JSON decode failed"))
                    |> ProgramTest.expectViewHas
                        [ Selector.text "ERR1: ERROR: JSON decode failed" ]
        , test "end-to-end: full pipeline from JSON response to decision" <|
            \_ ->
                let
                    -- Simulate what parseClassificationResponse does
                    apiResponseJson =
                        """
                        {
                            "content": [{"type": "text", "text": "{\\"relevance\\": \\"⭐⭐⭐⭐\\", \\"reasoning\\": \\"DOCK8 patient died from fungal sepsis\\", \\"note\\": \\"Verify infection type in full text\\", \\"death_after_therapy\\": false}"}],
                            "stop_reason": "end_turn",
                            "usage": {"input_tokens": 5000, "output_tokens": 100, "cache_read_input_tokens": 4000}
                        }
                        """

                    parsedResponse =
                        Decode.decodeString AnthropicApi.messageResponseDecoder apiResponseJson

                    classificationResult =
                        parsedResponse
                            |> Result.mapError Decode.errorToString
                            |> Result.andThen
                                (\resp ->
                                    resp.content
                                        |> List.filterMap
                                            (\block ->
                                                case block of
                                                    AnthropicApi.TextBlock t ->
                                                        Just t

                                                    _ ->
                                                        Nothing
                                            )
                                        |> String.concat
                                        |> Decode.decodeString Classification.classificationResultDecoder
                                        |> Result.mapError Decode.errorToString
                                )
                in
                start
                    |> ProgramTest.update
                        (GotClassificationResult "DOCK8_CASE" classificationResult)
                    |> ProgramTest.expectViewHas
                        [ Selector.text "DOCK8_CASE: ⭐⭐⭐⭐ INCLUDE" ]
        ]
