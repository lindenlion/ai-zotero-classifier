module OpenAiApi exposing
    ( ChatResponse
    , FinishReason(..)
    , chatResponseDecoder
    , encodeChatRequest
    , extractText
    )

{-| OpenAI-compatible chat completions API encoder/decoder.
Used for DeepSeek and other providers that follow the OpenAI format.
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type FinishReason
    = Stop
    | LengthLimit
    | ContentFilter
    | OtherReason


type alias ChatResponse =
    { content : String
    , finishReason : FinishReason
    , promptTokens : Int
    , completionTokens : Int
    , cachedTokens : Int
    }



-- Encoders


encodeChatRequest :
    { model : String
    , maxTokens : Int
    , systemPrompt : String
    , userMessage : String
    }
    -> Encode.Value
encodeChatRequest config =
    Encode.object
        [ ( "model", Encode.string config.model )
        , ( "max_tokens", Encode.int config.maxTokens )
        , ( "messages"
          , Encode.list identity
                [ Encode.object
                    [ ( "role", Encode.string "system" )
                    , ( "content", Encode.string config.systemPrompt )
                    ]
                , Encode.object
                    [ ( "role", Encode.string "user" )
                    , ( "content", Encode.string config.userMessage )
                    ]
                ]
          )
        ]



-- Decoders


finishReasonDecoder : Decoder FinishReason
finishReasonDecoder =
    Decode.string
        |> Decode.map
            (\str ->
                case str of
                    "stop" ->
                        Stop

                    "length" ->
                        LengthLimit

                    "content_filter" ->
                        ContentFilter

                    _ ->
                        OtherReason
            )


chatResponseDecoder : Decoder ChatResponse
chatResponseDecoder =
    Decode.map5 ChatResponse
        choiceContentDecoder
        choiceFinishReasonDecoder
        (Decode.at [ "usage", "prompt_tokens" ] Decode.int
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault 0)
        )
        (Decode.at [ "usage", "completion_tokens" ] Decode.int
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault 0)
        )
        (Decode.oneOf
            [ Decode.at [ "usage", "prompt_tokens_details", "cached_tokens" ] Decode.int
            , Decode.succeed 0
            ]
        )


choiceContentDecoder : Decoder String
choiceContentDecoder =
    Decode.at [ "choices" ] (Decode.index 0 (Decode.at [ "message", "content" ] Decode.string))


choiceFinishReasonDecoder : Decoder FinishReason
choiceFinishReasonDecoder =
    Decode.at [ "choices" ] (Decode.index 0 (Decode.field "finish_reason" finishReasonDecoder))


extractText : ChatResponse -> String
extractText response =
    response.content
