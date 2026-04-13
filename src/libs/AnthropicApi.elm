module AnthropicApi exposing
    ( CacheUsage
    , ContentBlock(..)
    , MessageResponse
    , StopReason(..)
    , encodeMessageRequest
    , extractText
    , messageResponseDecoder
    , stopReasonToString
    , stripJsonFences
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type StopReason
    = EndTurn
    | Refusal
    | MaxTokens
    | Other String


type ContentBlock
    = TextBlock String
    | UnknownBlock


type alias CacheUsage =
    { cacheCreationInputTokens : Int
    , cacheReadInputTokens : Int
    }


type alias MessageResponse =
    { content : List ContentBlock
    , stopReason : StopReason
    , usage : CacheUsage
    }



-- Encoders


encodeMessageRequest :
    { model : String
    , maxTokens : Int
    , systemPrompt : String
    , userMessage : String
    }
    -> Encode.Value
encodeMessageRequest config =
    Encode.object
        [ ( "model", Encode.string config.model )
        , ( "max_tokens", Encode.int config.maxTokens )
        , ( "system"
          , Encode.list identity
                [ Encode.object
                    [ ( "type", Encode.string "text" )
                    , ( "text", Encode.string config.systemPrompt )
                    , ( "cache_control"
                      , Encode.object
                            [ ( "type", Encode.string "ephemeral" )
                            , ( "ttl", Encode.string "1h" )
                            ]
                      )
                    ]
                ]
          )
        , ( "messages"
          , Encode.list identity
                [ Encode.object
                    [ ( "role", Encode.string "user" )
                    , ( "content", Encode.string config.userMessage )
                    ]
                ]
          )
        ]



-- Decoders


stopReasonDecoder : Decoder StopReason
stopReasonDecoder =
    Decode.string
        |> Decode.map
            (\str ->
                case str of
                    "end_turn" ->
                        EndTurn

                    "refusal" ->
                        Refusal

                    "max_tokens" ->
                        MaxTokens

                    other ->
                        Other other
            )


contentBlockDecoder : Decoder ContentBlock
contentBlockDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\blockType ->
                case blockType of
                    "text" ->
                        Decode.field "text" Decode.string
                            |> Decode.map TextBlock

                    _ ->
                        Decode.succeed UnknownBlock
            )


cacheUsageDecoder : Decoder CacheUsage
cacheUsageDecoder =
    Decode.map2 CacheUsage
        (Decode.oneOf
            [ Decode.field "cache_creation_input_tokens" Decode.int
            , Decode.succeed 0
            ]
        )
        (Decode.oneOf
            [ Decode.field "cache_read_input_tokens" Decode.int
            , Decode.succeed 0
            ]
        )


messageResponseDecoder : Decoder MessageResponse
messageResponseDecoder =
    Decode.map3 MessageResponse
        (Decode.field "content" (Decode.list contentBlockDecoder))
        (Decode.field "stop_reason" stopReasonDecoder)
        (Decode.field "usage" cacheUsageDecoder)


{-| Extract all text content from a MessageResponse, joining TextBlocks
and ignoring unknown block types.
-}
extractText : MessageResponse -> String
extractText response =
    response.content
        |> List.filterMap
            (\block ->
                case block of
                    TextBlock text ->
                        Just text

                    UnknownBlock ->
                        Nothing
            )
        |> String.concat


{-| Strip markdown JSON fences (```` ```json ... ``` ```` or ```` ``` ... ``` ````)
from a string, leaving the inner JSON content.
-}
stripJsonFences : String -> String
stripJsonFences str =
    let
        stripSuffix s =
            if String.endsWith "```" s then
                String.dropRight 3 s |> String.trimRight

            else
                s
    in
    if String.startsWith "```json" str then
        str |> String.dropLeft 7 |> String.trimLeft |> stripSuffix

    else if String.startsWith "```" str then
        str |> String.dropLeft 3 |> String.trimLeft |> stripSuffix

    else
        str


stopReasonToString : StopReason -> String
stopReasonToString reason =
    case reason of
        EndTurn ->
            "end_turn"

        Refusal ->
            "refusal"

        MaxTokens ->
            "max_tokens"

        Other s ->
            s
