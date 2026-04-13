module Stats exposing
    ( CircuitBreakerAction(..)
    , Stats
    , addStats
    , emptyStats
    , formatErrorTimestamps
    , formatSummary
    , recordErrorAndCheck
    )


type alias Stats =
    { processed : Int
    , included : Int
    , excluded : Int
    , refusals : Int
    , errors : Int
    , recentErrorTimestamps : List Int
    }


emptyStats : Stats
emptyStats =
    { processed = 0
    , included = 0
    , excluded = 0
    , refusals = 0
    , errors = 0
    , recentErrorTimestamps = []
    }


addStats : Stats -> Stats -> Stats
addStats a b =
    { processed = a.processed + b.processed
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
                | errors = stats.errors + 1
                , recentErrorTimestamps = pruned
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
    , "   Processed: " ++ String.fromInt stats.processed
    , "   Included:  " ++ String.fromInt stats.included
    , "   Excluded:  " ++ String.fromInt stats.excluded
    , "   Refusals:  " ++ String.fromInt stats.refusals
    , "   Errors:    " ++ String.fromInt stats.errors
    ]
        |> String.join "\n"
