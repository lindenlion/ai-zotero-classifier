module Stats exposing
    ( CircuitBreakerAction(..)
    , Stats
    , addStats
    , emptyStats
    , formatSummary
    , formatErrorTimestamps
    , recordErrorAndCheck
    )


{-| Two-tier stats: article-level outcomes and model-level decision counts.

Article level tracks what happened to each article:
  - processed, completed, partial, failed

Model level tracks individual model call outcomes:
  - included, excluded, refusals, errors

-}
type alias Stats =
    { -- Article-level
      processed : Int
    , completed : Int
    , partial : Int
    , failed : Int

    -- Model-level
    , included : Int
    , excluded : Int
    , refusals : Int
    , errors : Int

    -- Circuit breaker
    , recentErrorTimestamps : List Int
    }


emptyStats : Stats
emptyStats =
    { processed = 0
    , completed = 0
    , partial = 0
    , failed = 0
    , included = 0
    , excluded = 0
    , refusals = 0
    , errors = 0
    , recentErrorTimestamps = []
    }


addStats : Stats -> Stats -> Stats
addStats a b =
    { processed = a.processed + b.processed
    , completed = a.completed + b.completed
    , partial = a.partial + b.partial
    , failed = a.failed + b.failed
    , included = a.included + b.included
    , excluded = a.excluded + b.excluded
    , refusals = a.refusals + b.refusals
    , errors = a.errors + b.errors
    , recentErrorTimestamps = b.recentErrorTimestamps
    }


{-| Record an error, prune timestamps older than 5 minutes, and determine what to do.

  - Fewer than 5 errors in 5 min → Continue
  - 5 errors spread over more than 1 minute → PauseAndRetry (transient issue, worth retrying)
  - 5 errors all within 1 minute → Abort (rapid-fire failure, stop immediately)

-}
type CircuitBreakerAction
    = Continue
    | PauseAndRetry
    | Abort


recordErrorAndCheck : Int -> Stats -> ( Stats, CircuitBreakerAction )
recordErrorAndCheck nowMs stats =
    let
        fiveMinAgo =
            nowMs - (5 * 60 * 1000)

        pruned =
            List.filter (\t -> t > fiveMinAgo) (nowMs :: stats.recentErrorTimestamps)

        newStats =
            { stats
                | recentErrorTimestamps = pruned
            }
    in
    if List.length pruned < 5 then
        ( newStats, Continue )

    else
        let
            oldest =
                List.minimum pruned |> Maybe.withDefault nowMs

            spanMs =
                nowMs - oldest
        in
        if spanMs >= 60000 then
            ( newStats, PauseAndRetry )

        else
            ( newStats, Abort )


formatErrorTimestamps : Int -> List Int -> String
formatErrorTimestamps nowMs timestamps =
    let
        sorted =
            List.sortBy negate timestamps

        format t =
            let
                agoMs =
                    nowMs - t

                agoSec =
                    agoMs // 1000
            in
            if agoSec < 60 then
                String.fromInt agoSec ++ "s ago"

            else
                String.fromInt (agoSec // 60) ++ "m" ++ String.fromInt (modBy 60 agoSec) ++ "s ago"
    in
    sorted
        |> List.take 5
        |> List.map format
        |> String.join ", "


formatSummary : String -> Stats -> String
formatSummary heading stats =
    [ "\n" ++ String.repeat 60 "="
    , heading
    , String.repeat 60 "="
    , "   Articles:  "
        ++ String.fromInt stats.processed
        ++ " processed ("
        ++ String.fromInt stats.completed
        ++ " completed, "
        ++ String.fromInt stats.partial
        ++ " partial, "
        ++ String.fromInt stats.failed
        ++ " failed)"
    , "   Decisions: "
        ++ String.fromInt stats.included
        ++ " include, "
        ++ String.fromInt stats.excluded
        ++ " exclude, "
        ++ String.fromInt stats.refusals
        ++ " refusals, "
        ++ String.fromInt stats.errors
        ++ " errors"
    ]
        |> String.join "\n"
