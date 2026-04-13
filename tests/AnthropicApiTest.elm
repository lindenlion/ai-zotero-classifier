module AnthropicApiTest exposing (..)

import AnthropicApi
    exposing
        ( ContentBlock(..)
        , StopReason(..)
        )
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)


suite : Test
suite =
    describe "AnthropicApi"
        [ describe "messageResponseDecoder"
            [ test "decodes successful response with text content" <|
                \_ ->
                    let
                        json =
                            """
                            {
                                "content": [{"type": "text", "text": "Hello world"}],
                                "stop_reason": "end_turn",
                                "usage": {
                                    "input_tokens": 100,
                                    "output_tokens": 50,
                                    "cache_creation_input_tokens": 0,
                                    "cache_read_input_tokens": 4000
                                }
                            }
                            """
                    in
                    Decode.decodeString AnthropicApi.messageResponseDecoder json
                        |> Result.map .stopReason
                        |> Expect.equal (Ok EndTurn)
            , test "decodes refusal stop reason" <|
                \_ ->
                    let
                        json =
                            """
                            {
                                "content": [{"type": "text", "text": "I cannot help with that."}],
                                "stop_reason": "refusal",
                                "usage": {"input_tokens": 10, "output_tokens": 5}
                            }
                            """
                    in
                    Decode.decodeString AnthropicApi.messageResponseDecoder json
                        |> Result.map .stopReason
                        |> Expect.equal (Ok Refusal)
            , test "decodes max_tokens stop reason" <|
                \_ ->
                    let
                        json =
                            """
                            {
                                "content": [{"type": "text", "text": "truncated"}],
                                "stop_reason": "max_tokens",
                                "usage": {"input_tokens": 10, "output_tokens": 1000}
                            }
                            """
                    in
                    Decode.decodeString AnthropicApi.messageResponseDecoder json
                        |> Result.map .stopReason
                        |> Expect.equal (Ok MaxTokens)
            , test "decodes cache usage when present" <|
                \_ ->
                    let
                        json =
                            """
                            {
                                "content": [{"type": "text", "text": "ok"}],
                                "stop_reason": "end_turn",
                                "usage": {
                                    "input_tokens": 100,
                                    "output_tokens": 50,
                                    "cache_creation_input_tokens": 4500,
                                    "cache_read_input_tokens": 0
                                }
                            }
                            """
                    in
                    Decode.decodeString AnthropicApi.messageResponseDecoder json
                        |> Result.map (\r -> ( r.usage.cacheCreationInputTokens, r.usage.cacheReadInputTokens ))
                        |> Expect.equal (Ok ( 4500, 0 ))
            , test "defaults cache usage to 0 when absent" <|
                \_ ->
                    let
                        json =
                            """
                            {
                                "content": [{"type": "text", "text": "ok"}],
                                "stop_reason": "end_turn",
                                "usage": {"input_tokens": 100, "output_tokens": 50}
                            }
                            """
                    in
                    Decode.decodeString AnthropicApi.messageResponseDecoder json
                        |> Result.map (\r -> ( r.usage.cacheCreationInputTokens, r.usage.cacheReadInputTokens ))
                        |> Expect.equal (Ok ( 0, 0 ))
            , test "extracts text from content blocks" <|
                \_ ->
                    let
                        json =
                            """
                            {
                                "content": [
                                    {"type": "text", "text": "part1"},
                                    {"type": "text", "text": "part2"}
                                ],
                                "stop_reason": "end_turn",
                                "usage": {"input_tokens": 10, "output_tokens": 5}
                            }
                            """
                    in
                    Decode.decodeString AnthropicApi.messageResponseDecoder json
                        |> Result.map
                            (\r ->
                                r.content
                                    |> List.filterMap
                                        (\block ->
                                            case block of
                                                TextBlock text ->
                                                    Just text

                                                UnknownBlock ->
                                                    Nothing
                                        )
                            )
                        |> Expect.equal (Ok [ "part1", "part2" ])
            , test "handles unknown content block types" <|
                \_ ->
                    let
                        json =
                            """
                            {
                                "content": [
                                    {"type": "image", "source": {}},
                                    {"type": "text", "text": "hello"}
                                ],
                                "stop_reason": "end_turn",
                                "usage": {"input_tokens": 10, "output_tokens": 5}
                            }
                            """
                    in
                    Decode.decodeString AnthropicApi.messageResponseDecoder json
                        |> Result.map (.content >> List.length)
                        |> Expect.equal (Ok 2)
            ]
        , describe "encodeMessageRequest"
            [ test "includes model and max_tokens" <|
                \_ ->
                    let
                        encoded =
                            AnthropicApi.encodeMessageRequest
                                { model = "claude-opus-4-6"
                                , maxTokens = 1000
                                , systemPrompt = "You are a helper."
                                , userMessage = "Hello"
                                }
                                |> Encode.encode 0
                    in
                    Decode.decodeString
                        (Decode.map2 Tuple.pair
                            (Decode.field "model" Decode.string)
                            (Decode.field "max_tokens" Decode.int)
                        )
                        encoded
                        |> Expect.equal (Ok ( "claude-opus-4-6", 1000 ))
            , test "includes system prompt with cache_control" <|
                \_ ->
                    let
                        encoded =
                            AnthropicApi.encodeMessageRequest
                                { model = "claude-opus-4-6"
                                , maxTokens = 1000
                                , systemPrompt = "System prompt text"
                                , userMessage = "Hello"
                                }
                                |> Encode.encode 0
                    in
                    Decode.decodeString
                        (Decode.at [ "system" ]
                            (Decode.index 0
                                (Decode.map2 Tuple.pair
                                    (Decode.field "text" Decode.string)
                                    (Decode.at [ "cache_control", "type" ] Decode.string)
                                )
                            )
                        )
                        encoded
                        |> Expect.equal (Ok ( "System prompt text", "ephemeral" ))
            , test "includes cache TTL of 1h" <|
                \_ ->
                    let
                        encoded =
                            AnthropicApi.encodeMessageRequest
                                { model = "m"
                                , maxTokens = 1
                                , systemPrompt = "s"
                                , userMessage = "u"
                                }
                                |> Encode.encode 0
                    in
                    Decode.decodeString
                        (Decode.at [ "system" ]
                            (Decode.index 0
                                (Decode.at [ "cache_control", "ttl" ] Decode.string)
                            )
                        )
                        encoded
                        |> Expect.equal (Ok "1h")
            , test "includes user message" <|
                \_ ->
                    let
                        encoded =
                            AnthropicApi.encodeMessageRequest
                                { model = "m"
                                , maxTokens = 1
                                , systemPrompt = "s"
                                , userMessage = "The user message"
                                }
                                |> Encode.encode 0
                    in
                    Decode.decodeString
                        (Decode.at [ "messages" ]
                            (Decode.index 0
                                (Decode.map2 Tuple.pair
                                    (Decode.field "role" Decode.string)
                                    (Decode.field "content" Decode.string)
                                )
                            )
                        )
                        encoded
                        |> Expect.equal (Ok ( "user", "The user message" ))
            ]
        , describe "stopReasonToString"
            [ test "EndTurn" <|
                \_ ->
                    AnthropicApi.stopReasonToString EndTurn
                        |> Expect.equal "end_turn"
            , test "Refusal" <|
                \_ ->
                    AnthropicApi.stopReasonToString Refusal
                        |> Expect.equal "refusal"
            , test "MaxTokens" <|
                \_ ->
                    AnthropicApi.stopReasonToString MaxTokens
                        |> Expect.equal "max_tokens"
            , test "Other preserves value" <|
                \_ ->
                    AnthropicApi.stopReasonToString (Other "custom")
                        |> Expect.equal "custom"
            ]
        , describe "extractText"
            [ test "joins multiple TextBlocks" <|
                \_ ->
                    AnthropicApi.extractText
                        { content = [ TextBlock "Hello ", TextBlock "world" ]
                        , stopReason = EndTurn
                        , usage = { cacheCreationInputTokens = 0, cacheReadInputTokens = 0 }
                        }
                        |> Expect.equal "Hello world"
            , test "ignores UnknownBlocks" <|
                \_ ->
                    AnthropicApi.extractText
                        { content = [ TextBlock "text", UnknownBlock, TextBlock "!" ]
                        , stopReason = EndTurn
                        , usage = { cacheCreationInputTokens = 0, cacheReadInputTokens = 0 }
                        }
                        |> Expect.equal "text!"
            , test "returns empty string for no content" <|
                \_ ->
                    AnthropicApi.extractText
                        { content = []
                        , stopReason = EndTurn
                        , usage = { cacheCreationInputTokens = 0, cacheReadInputTokens = 0 }
                        }
                        |> Expect.equal ""
            ]
        , describe "stripJsonFences"
            [ test "strips ```json fences" <|
                \_ ->
                    AnthropicApi.stripJsonFences "```json\n{\"key\": \"value\"}\n```"
                        |> Expect.equal "{\"key\": \"value\"}"
            , test "strips bare ``` fences" <|
                \_ ->
                    AnthropicApi.stripJsonFences "```\n{\"key\": \"value\"}\n```"
                        |> Expect.equal "{\"key\": \"value\"}"
            , test "passes through plain JSON" <|
                \_ ->
                    AnthropicApi.stripJsonFences "{\"key\": \"value\"}"
                        |> Expect.equal "{\"key\": \"value\"}"
            , test "handles missing closing fence" <|
                \_ ->
                    AnthropicApi.stripJsonFences "```json\n{\"key\": \"value\"}"
                        |> Expect.equal "{\"key\": \"value\"}"
            ]
        ]
