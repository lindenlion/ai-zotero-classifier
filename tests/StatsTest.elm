module StatsTest exposing (..)

import Expect
import Stats
    exposing
        ( CircuitBreakerAction(..)
        , emptyStats
        )
import Test exposing (..)


suite : Test
suite =
    describe "Stats"
        [ describe "recordErrorAndCheck"
            [ test "returns Continue with fewer than 5 errors" <|
                \_ ->
                    let
                        ( _, action ) =
                            Stats.recordErrorAndCheck 1000 emptyStats
                    in
                    Expect.equal Continue action
            , test "returns Continue after 4 errors in 5 minutes" <|
                \_ ->
                    let
                        stats =
                            { emptyStats | recentErrorTimestamps = [ 300000, 200000, 100000 ] }

                        ( _, action ) =
                            Stats.recordErrorAndCheck 400000 stats
                    in
                    Expect.equal Continue action
            , test "returns Abort when 5 errors within 1 minute" <|
                \_ ->
                    let
                        -- 4 errors at 10s, 20s, 30s, 40s; 5th at 50s (all within 50s < 60s)
                        stats =
                            { emptyStats | recentErrorTimestamps = [ 40000, 30000, 20000, 10000 ] }

                        ( _, action ) =
                            Stats.recordErrorAndCheck 50000 stats
                    in
                    Expect.equal Abort action
            , test "returns PauseAndRetry when 5 errors spread over more than 1 minute" <|
                \_ ->
                    let
                        -- Errors at 0s, 30s, 60s, 90s; 5th at 120s (span = 120s > 60s)
                        stats =
                            { emptyStats | recentErrorTimestamps = [ 90000, 60000, 30000, 0 ] }

                        ( _, action ) =
                            Stats.recordErrorAndCheck 120000 stats
                    in
                    Expect.equal PauseAndRetry action
            , test "prunes timestamps older than 5 minutes" <|
                \_ ->
                    let
                        -- Old error at 0ms, now at 301000ms (5min + 1s)
                        stats =
                            { emptyStats | recentErrorTimestamps = [ 0 ] }

                        ( newStats, _ ) =
                            Stats.recordErrorAndCheck 301000 stats
                    in
                    Expect.equal [ 301000 ] newStats.recentErrorTimestamps
            , test "increments error count" <|
                \_ ->
                    let
                        ( newStats, _ ) =
                            Stats.recordErrorAndCheck 1000 emptyStats
                    in
                    Expect.equal 1 newStats.errors
            ]
        , describe "formatErrorTimestamps"
            [ test "formats seconds for recent errors" <|
                \_ ->
                    Stats.formatErrorTimestamps 10000 [ 5000 ]
                        |> Expect.equal "5s ago"
            , test "formats minutes and seconds for older errors" <|
                \_ ->
                    Stats.formatErrorTimestamps 100000 [ 25000 ]
                        |> Expect.equal "1m15s ago"
            , test "shows most recent first, limited to 5" <|
                \_ ->
                    Stats.formatErrorTimestamps 60000 [ 50000, 40000, 30000, 20000, 10000, 0 ]
                        |> Expect.equal "10s ago, 20s ago, 30s ago, 40s ago, 50s ago"
            , test "handles empty list" <|
                \_ ->
                    Stats.formatErrorTimestamps 1000 []
                        |> Expect.equal ""
            ]
        , describe "formatSummary"
            [ test "includes heading and all stats" <|
                \_ ->
                    let
                        stats =
                            { emptyStats | processed = 10, included = 7, excluded = 2, refusals = 1, errors = 0 }

                        result =
                            Stats.formatSummary "TEST HEADING" stats
                    in
                    Expect.all
                        [ \s -> String.contains "TEST HEADING" s |> Expect.equal True
                        , \s -> String.contains "Processed: 10" s |> Expect.equal True
                        , \s -> String.contains "Included:  7" s |> Expect.equal True
                        , \s -> String.contains "Excluded:  2" s |> Expect.equal True
                        , \s -> String.contains "Refusals:  1" s |> Expect.equal True
                        , \s -> String.contains "Errors:    0" s |> Expect.equal True
                        ]
                        result
            ]
        , describe "addStats"
            [ test "sums all numeric fields" <|
                \_ ->
                    let
                        a =
                            { emptyStats | processed = 5, included = 3, excluded = 1, refusals = 1, errors = 0 }

                        b =
                            { emptyStats | processed = 3, included = 2, excluded = 1, refusals = 0, errors = 0 }

                        result =
                            Stats.addStats a b
                    in
                    Expect.all
                        [ \s -> Expect.equal 8 s.processed
                        , \s -> Expect.equal 5 s.included
                        , \s -> Expect.equal 2 s.excluded
                        , \s -> Expect.equal 1 s.refusals
                        , \s -> Expect.equal 0 s.errors
                        ]
                        result
            , test "takes recentErrorTimestamps from second argument" <|
                \_ ->
                    let
                        a =
                            { emptyStats | recentErrorTimestamps = [ 100, 200 ] }

                        b =
                            { emptyStats | recentErrorTimestamps = [ 300 ] }
                    in
                    Stats.addStats a b
                        |> .recentErrorTimestamps
                        |> Expect.equal [ 300 ]
            ]
        ]
