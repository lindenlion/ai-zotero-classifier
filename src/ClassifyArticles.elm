module ClassifyArticles exposing (run)

import AnthropicApi
import Appraisal
import BackendTask exposing (BackendTask)
import BackendTask.Env as Env
import BackendTask.File
import BackendTask.Http
import BackendTask.Time
import Classification
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)
import Stats exposing (CircuitBreakerAction(..), Stats, emptyStats)
import Time
import ZoteroApi



-- Config


type alias Config =
    { zoteroLibraryId : String
    , zoteroApiKey : String
    , anthropicApiKey : String
    , anthropicModel : String
    , systemPrompt : String
    }


promptFile : String
promptFile =
    "prompt.txt"



-- Constants


relevantCollection : String
relevantCollection =
    "Claude included"


irrelevantCollection : String
irrelevantCollection =
    "Claude excluded"


processedTag : String
processedTag =
    "CLAUDE"


migrationParentCollection : String
migrationParentCollection =
    "data_schema_migrations"


versionCollectionPrefix : String
versionCollectionPrefix =
    "version_"


versionCollectionName : Int -> String
versionCollectionName n =
    versionCollectionPrefix ++ String.fromInt n



-- CLI


type alias CliOptions =
    { reprocessTag : Maybe String
    , max : Maybe String
    , migrate : Maybe String
    }


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.optionalKeywordArg "reprocess-tag"
                        |> Option.withDescription "Tag to select articles for reprocessing (e.g. 'CLAUDE_retry'). These articles already have CLAUDE; the specified tag will be stripped."
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "max"
                        |> Option.withDescription "Maximum number of articles to process. 0 = all. Skips the interactive prompt."
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "migrate"
                        |> Option.withDescription "Migration-only mode (no AI calls). Optional integer = oldest schema version to migrate from. Omit value to migrate all CLAUDE-tagged articles."
                    )
            )



-- Entry point


run : Script
run =
    Script.withCliOptions program
        (\options ->
            logBanner options
                |> BackendTask.andThen (\_ -> loadConfig)
                |> BackendTask.andThen (\config -> initAndProcess config options)
        )


logBanner : CliOptions -> BackendTask FatalError ()
logBanner options =
    let
        modeMsg =
            case options.migrate of
                Just _ ->
                    "Mode: MIGRATE schema (no AI calls)"

                Nothing ->
                    case options.reprocessTag of
                        Just tag ->
                            "Mode: REPROCESS articles tagged \"" ++ tag ++ "\" (tag will be stripped)"

                        Nothing ->
                            "Mode: process new articles (no CLAUDE tag)"
    in
    Script.log ("🔬 PubMed Article Classifier for IEI Research\n" ++ String.repeat 60 "=" ++ "\n" ++ modeMsg)


initAndProcess : Config -> CliOptions -> BackendTask FatalError ()
initAndProcess config options =
    Script.log ("✓ Loaded configuration for library: " ++ config.zoteroLibraryId)
        |> BackendTask.andThen (\_ -> resolveAllCollections config)
        |> BackendTask.andThen
            (\collections ->
                Script.log
                    ("✓ Relevant collection: "
                        ++ relevantCollection
                        ++ " ("
                        ++ collections.relevantKey
                        ++ ")\n✓ Irrelevant collection: "
                        ++ irrelevantCollection
                        ++ " ("
                        ++ collections.irrelevantKey
                        ++ ")\n✓ Current schema version: "
                        ++ String.fromInt Appraisal.currentSchemaVersion
                        ++ " (collection: "
                        ++ collections.currentVersionKey
                        ++ ")"
                    )
                    |> BackendTask.andThen (\_ -> resolveMaxArticles options.max)
                    |> BackendTask.andThen
                        (\maxArticles ->
                            case options.migrate of
                                Just migrateArg ->
                                    let
                                        fromVersion =
                                            migrateArg
                                                |> String.trim
                                                |> String.toInt
                                    in
                                    migrateAll config collections fromVersion maxArticles

                                Nothing ->
                                    processAll config collections options.reprocessTag maxArticles
                        )
            )


{-| Resolve max articles from --max flag or interactive prompt.

  - Nothing → prompt interactively
  - Just "0" or Just "" → all articles (0)
  - Just positive int → use that value
  - Just negative or non-integer → abort with error

-}
resolveMaxArticles : Maybe String -> BackendTask FatalError Int
resolveMaxArticles maxFlag =
    case maxFlag of
        Nothing ->
            promptForMaxArticles

        Just raw ->
            let
                trimmed =
                    String.trim raw
            in
            if trimmed == "" || trimmed == "0" || trimmed == "all" then
                BackendTask.succeed 0

            else
                case String.toInt trimmed of
                    Just n ->
                        if n > 0 then
                            BackendTask.succeed n

                        else
                            FatalError.fromString ("Invalid --max value: " ++ trimmed ++ ". Must be a positive integer or 0 for all.")
                                |> BackendTask.fail

                    Nothing ->
                        FatalError.fromString ("Invalid --max value: " ++ trimmed ++ ". Must be a positive integer or 0 for all.")
                            |> BackendTask.fail


promptForMaxArticles : BackendTask FatalError Int
promptForMaxArticles =
    Script.question "\nEnter number of articles to process (default 100, 0 for all): "
        |> BackendTask.allowFatal
        |> BackendTask.map
            (\answer ->
                let
                    trimmed =
                        String.trim answer
                in
                if trimmed == "" then
                    100

                else
                    String.toInt trimmed |> Maybe.withDefault 100
            )



-- Config loading


loadConfig : BackendTask FatalError Config
loadConfig =
    BackendTask.map4 Config
        (Env.expect "ZOTERO_LIBRARY_ID" |> BackendTask.allowFatal)
        (Env.expect "ZOTERO_API_KEY" |> BackendTask.allowFatal)
        (Env.expect "ANTHROPIC_API_KEY" |> BackendTask.allowFatal)
        (Env.expect "ANTHROPIC_MODEL" |> BackendTask.allowFatal)
        |> BackendTask.andThen
            (\partialConfig ->
                loadPromptFile
                    |> BackendTask.map (\prompt -> partialConfig prompt)
            )


loadPromptFile : BackendTask FatalError String
loadPromptFile =
    BackendTask.File.rawFile promptFile
        |> BackendTask.onError
            (\_ ->
                BackendTask.fail
                    (FatalError.fromString
                        ("Missing prompt file: " ++ promptFile ++ "\nCreate this file in your project root with the system prompt for article classification.")
                    )
            )



-- Zotero API helpers


type alias Collections =
    { relevantKey : String
    , irrelevantKey : String
    , currentVersionKey : String
    , versionKeys : Dict Int String
    }


zoteroBaseUrl : String -> String
zoteroBaseUrl libraryId =
    "https://api.zotero.org/groups/" ++ libraryId


zoteroHeaders : String -> List ( String, String )
zoteroHeaders apiKey =
    [ ( "Zotero-API-Key", apiKey )
    , ( "Content-Type", "application/json" )
    ]


{-| Make an HTTP request with logging and error recovery.
Logs the HTTP method and label, catches HTTP errors as Err values.
Used for per-article operations that should be skippable on failure.
-}
loggedRequest :
    String
    ->
        { url : String
        , method : String
        , headers : List ( String, String )
        , body : BackendTask.Http.Body
        , retries : Maybe Int
        , timeoutInMs : Maybe Int
        }
    -> BackendTask.Http.Expect a
    -> BackendTask FatalError (Result String a)
loggedRequest label config expect =
    Script.log (config.method ++ " " ++ label)
        |> BackendTask.andThen
            (\_ ->
                BackendTask.Http.request config expect
                    |> BackendTask.map Ok
                    |> BackendTask.onError
                        (\_ ->
                            BackendTask.succeed (Err (config.method ++ " " ++ label ++ " failed"))
                        )
            )


{-| Chain BackendTask-wrapped Results, short-circuiting on Err.
-}
andThenResult : (a -> BackendTask FatalError (Result String b)) -> BackendTask FatalError (Result String a) -> BackendTask FatalError (Result String b)
andThenResult f task =
    task
        |> BackendTask.andThen
            (\result ->
                case result of
                    Err msg ->
                        BackendTask.succeed (Err msg)

                    Ok a ->
                        f a
            )


resolveAllCollections : Config -> BackendTask FatalError Collections
resolveAllCollections config =
    let
        url =
            zoteroBaseUrl config.zoteroLibraryId ++ "/collections"
    in
    Script.log "GET /collections"
        |> BackendTask.andThen
            (\_ ->
                BackendTask.Http.request
                    { url = url
                    , method = "GET"
                    , headers = zoteroHeaders config.zoteroApiKey
                    , body = BackendTask.Http.emptyBody
                    , retries = Just 1
                    , timeoutInMs = Just 30000
                    }
                    (BackendTask.Http.expectJson ZoteroApi.collectionListDecoder)
                    |> BackendTask.allowFatal
            )
        |> BackendTask.andThen
            (\allCollections ->
                BackendTask.map2 Tuple.pair
                    (ensureCollection config relevantCollection allCollections)
                    (ensureCollection config irrelevantCollection allCollections)
                    |> BackendTask.andThen
                        (\( relKey, irrelKey ) ->
                            resolveVersionCollections config allCollections
                                |> BackendTask.map
                                    (\( currentKey, vKeys ) ->
                                        { relevantKey = relKey
                                        , irrelevantKey = irrelKey
                                        , currentVersionKey = currentKey
                                        , versionKeys = vKeys
                                        }
                                    )
                        )
            )


resolveVersionCollections : Config -> List ZoteroApi.ZoteroCollection -> BackendTask FatalError ( String, Dict Int String )
resolveVersionCollections config allCollections =
    ensureCollection config migrationParentCollection allCollections
        |> BackendTask.andThen
            (\parentKey ->
                let
                    currentName =
                        versionCollectionName Appraisal.currentSchemaVersion
                in
                ensureSubCollection config currentName parentKey allCollections
                    |> BackendTask.map
                        (\currentKey ->
                            let
                                existingVersionKeys =
                                    allCollections
                                        |> List.filter (\c -> c.parentCollection == parentKey && String.startsWith versionCollectionPrefix c.name)
                                        |> List.filterMap
                                            (\c ->
                                                c.name
                                                    |> String.dropLeft (String.length versionCollectionPrefix)
                                                    |> String.toInt
                                                    |> Maybe.map (\n -> ( n, c.key ))
                                            )
                                        |> Dict.fromList

                                vKeys =
                                    Dict.insert Appraisal.currentSchemaVersion currentKey existingVersionKeys
                            in
                            ( currentKey, vKeys )
                        )
            )


ensureCollection : Config -> String -> List ZoteroApi.ZoteroCollection -> BackendTask FatalError String
ensureCollection config name allCollections =
    case allCollections |> List.filter (\c -> c.name == name) |> List.head |> Maybe.map .key of
        Just key ->
            BackendTask.succeed key

        Nothing ->
            createCollection config name


ensureSubCollection : Config -> String -> String -> List ZoteroApi.ZoteroCollection -> BackendTask FatalError String
ensureSubCollection config name parentKey allCollections =
    case allCollections |> List.filter (\c -> c.name == name && c.parentCollection == parentKey) |> List.head |> Maybe.map .key of
        Just key ->
            BackendTask.succeed key

        Nothing ->
            createSubCollection config name parentKey


createCollection : Config -> String -> BackendTask FatalError String
createCollection config name =
    let
        url =
            zoteroBaseUrl config.zoteroLibraryId ++ "/collections"
    in
    Script.log ("POST /collections (create: " ++ name ++ ")")
        |> BackendTask.andThen
            (\_ ->
                BackendTask.Http.request
                    { url = url
                    , method = "POST"
                    , headers = zoteroHeaders config.zoteroApiKey
                    , body = BackendTask.Http.jsonBody (ZoteroApi.encodeCreateCollection name)
                    , retries = Nothing
                    , timeoutInMs = Just 30000
                    }
                    (BackendTask.Http.expectJson
                        (Decode.at [ "successful", "0", "key" ] Decode.string)
                    )
                    |> BackendTask.allowFatal
            )
        |> BackendTask.andThen
            (\key ->
                Script.log ("Created new collection: " ++ name)
                    |> BackendTask.map (\_ -> key)
            )


createSubCollection : Config -> String -> String -> BackendTask FatalError String
createSubCollection config name parentKey =
    let
        url =
            zoteroBaseUrl config.zoteroLibraryId ++ "/collections"
    in
    Script.log ("POST /collections (create sub-collection: " ++ name ++ ")")
        |> BackendTask.andThen
            (\_ ->
                BackendTask.Http.request
                    { url = url
                    , method = "POST"
                    , headers = zoteroHeaders config.zoteroApiKey
                    , body = BackendTask.Http.jsonBody (ZoteroApi.encodeCreateSubCollection name parentKey)
                    , retries = Nothing
                    , timeoutInMs = Just 30000
                    }
                    (BackendTask.Http.expectJson
                        (Decode.at [ "successful", "0", "key" ] Decode.string)
                    )
                    |> BackendTask.allowFatal
            )
        |> BackendTask.andThen
            (\key ->
                Script.log ("Created new sub-collection: " ++ name)
                    |> BackendTask.map (\_ -> key)
            )


{-| Fetch articles to process.

  - Normal mode (reprocessTag = Nothing): articles WITHOUT the CLAUDE tag.
  - Reprocess mode (reprocessTag = Just tag): articles WITH both CLAUDE and the given tag.

-}
fetchItems : Config -> Maybe String -> Int -> BackendTask FatalError (List ZoteroApi.ZoteroItem)
fetchItems config reprocessTag limit =
    let
        tagFilter =
            case reprocessTag of
                Nothing ->
                    "tag=-" ++ processedTag

                Just tag ->
                    "tag=" ++ processedTag ++ "&tag=" ++ tag

        url =
            zoteroBaseUrl config.zoteroLibraryId
                ++ "/items?"
                ++ tagFilter
                ++ "&limit="
                ++ String.fromInt limit
                ++ "&itemType=-note"
    in
    Script.log ("GET /items?" ++ tagFilter)
        |> BackendTask.andThen
            (\_ ->
                BackendTask.Http.request
                    { url = url
                    , method = "GET"
                    , headers = zoteroHeaders config.zoteroApiKey
                    , body = BackendTask.Http.emptyBody
                    , retries = Just 1
                    , timeoutInMs = Just 30000
                    }
                    (BackendTask.Http.expectJson ZoteroApi.itemListDecoder)
                    |> BackendTask.allowFatal
            )


{-| PATCH an item's tags and collections in a single request.
No re-fetch needed — we use the version from the batch fetch.
-}
patchItem : Config -> ZoteroApi.ZoteroItem -> { tags : List ZoteroApi.ZoteroTag, collections : List String, callNumber : String } -> BackendTask FatalError (Result String ())
patchItem config item patch =
    loggedRequest ("/items/" ++ item.key)
        { url = zoteroBaseUrl config.zoteroLibraryId ++ "/items/" ++ item.key
        , method = "PATCH"
        , headers =
            ( "If-Unmodified-Since-Version", String.fromInt item.version )
                :: zoteroHeaders config.zoteroApiKey
        , body = BackendTask.Http.jsonBody (ZoteroApi.encodeItemPatch patch)
        , retries = Nothing
        , timeoutInMs = Just 30000
        }
        (BackendTask.Http.expectWhatever ())


getChildNotes : Config -> String -> BackendTask FatalError (Result String (List ZoteroApi.ZoteroNote))
getChildNotes config itemKey =
    loggedRequest ("/items/" ++ itemKey ++ "/children")
        { url =
            zoteroBaseUrl config.zoteroLibraryId
                ++ "/items/"
                ++ itemKey
                ++ "/children?itemType=note"
        , method = "GET"
        , headers = zoteroHeaders config.zoteroApiKey
        , body = BackendTask.Http.emptyBody
        , retries = Just 1
        , timeoutInMs = Just 30000
        }
        (BackendTask.Http.expectJson ZoteroApi.noteListDecoder)


createNote : Config -> String -> String -> BackendTask FatalError (Result String ())
createNote config parentItemKey noteHtml =
    loggedRequest "/items (create note)"
        { url = zoteroBaseUrl config.zoteroLibraryId ++ "/items"
        , method = "POST"
        , headers = zoteroHeaders config.zoteroApiKey
        , body = BackendTask.Http.jsonBody (ZoteroApi.encodeCreateNote parentItemKey noteHtml)
        , retries = Nothing
        , timeoutInMs = Just 30000
        }
        (BackendTask.Http.expectWhatever ())


patchNote : Config -> ZoteroApi.ZoteroNote -> String -> BackendTask FatalError (Result String ())
patchNote config note newContent =
    loggedRequest ("/items/" ++ note.key ++ " (patch note)")
        { url = zoteroBaseUrl config.zoteroLibraryId ++ "/items/" ++ note.key
        , method = "PATCH"
        , headers =
            ( "If-Unmodified-Since-Version", String.fromInt note.version )
                :: zoteroHeaders config.zoteroApiKey
        , body = BackendTask.Http.jsonBody (ZoteroApi.encodeNotePatch newContent)
        , retries = Nothing
        , timeoutInMs = Just 30000
        }
        (BackendTask.Http.expectWhatever ())


deleteItem : Config -> String -> Int -> BackendTask FatalError (Result String ())
deleteItem config itemKey version =
    loggedRequest ("/items/" ++ itemKey)
        { url = zoteroBaseUrl config.zoteroLibraryId ++ "/items/" ++ itemKey
        , method = "DELETE"
        , headers =
            ( "If-Unmodified-Since-Version", String.fromInt version )
                :: zoteroHeaders config.zoteroApiKey
        , body = BackendTask.Http.emptyBody
        , retries = Nothing
        , timeoutInMs = Just 30000
        }
        (BackendTask.Http.expectWhatever ())



-- Anthropic API


classifyArticle : Config -> ZoteroApi.ArticleData -> BackendTask FatalError (Result String Classification.ClassificationResult)
classifyArticle config article =
    let
        userMessage =
            Classification.userPrompt
                { title = article.title
                , abstract = article.abstract
                , keywords = article.keywords
                }

        requestBody =
            AnthropicApi.encodeMessageRequest
                { model = config.anthropicModel
                , maxTokens = 1000
                , systemPrompt = config.systemPrompt
                , userMessage = userMessage
                }
    in
    loggedRequest "Anthropic API"
        { url = "https://api.anthropic.com/v1/messages"
        , method = "POST"
        , headers =
            [ ( "x-api-key", config.anthropicApiKey )
            , ( "anthropic-version", "2023-06-01" )
            , ( "Content-Type", "application/json" )
            ]
        , body = BackendTask.Http.jsonBody requestBody
        , retries = Nothing
        , timeoutInMs = Just 120000
        }
        (BackendTask.Http.expectJson AnthropicApi.messageResponseDecoder)
        |> BackendTask.map (Result.andThen parseClassificationResponse)


parseClassificationResponse : AnthropicApi.MessageResponse -> Result String Classification.ClassificationResult
parseClassificationResponse response =
    case response.stopReason of
        AnthropicApi.Refusal ->
            Ok Classification.refusalResult

        _ ->
            let
                textContent =
                    AnthropicApi.extractText response

                cleanedJson =
                    textContent |> String.trim |> AnthropicApi.stripJsonFences
            in
            case Decode.decodeString Classification.classificationResultDecoder cleanedJson of
                Ok result ->
                    Ok result

                Err err ->
                    Err ("JSON decode error: " ++ Decode.errorToString err ++ " | Raw: " ++ textContent)



-- Item update


isReprocessTag : Maybe String -> String -> Bool
isReprocessTag reprocessTag tagName =
    case reprocessTag of
        Just rt ->
            tagName == rt

        Nothing ->
            False


{-| Update a Zotero item after classification.

1.  Build AppraisalData from existing callNumber (or migrate legacy data).
2.  Insert the new Claude appraisal.
3.  PATCH item: tags, collections, and callNumber (structured JSON).
4.  GET child notes, then create/overwrite the reasoning note.

-}
updateItem :
    Config
    -> Collections
    -> Maybe String
    -> ZoteroApi.ZoteroItem
    -> Classification.ClassificationResult
    -> BackendTask FatalError (Result String ())
updateItem config collections reprocessTag item result =
    BackendTask.Time.now
        |> BackendTask.andThen
            (\now ->
                let
                    timestamp =
                        Iso8601.fromTime now

                    appraisal =
                        Appraisal.fromClassificationResult
                            { model = config.anthropicModel, timestamp = timestamp }
                            result

                    baseAppraisalData =
                        resolveAppraisalData item

                    appraisalData =
                        Appraisal.setAppraisal "claude" appraisal baseAppraisalData

                    callNumberJson =
                        Appraisal.encode appraisalData
                            |> Encode.encode 0

                    decision =
                        Classification.relevanceToDecision result.relevance

                    targetCollectionKey =
                        case decision of
                            Classification.Include ->
                                collections.relevantKey

                            Classification.Exclude ->
                                collections.irrelevantKey

                    -- Strip old star tags, death_after_therapy, CLAUDE, and the reprocess tag
                    cleanedTags =
                        item.data.tags
                            |> List.filter
                                (\t ->
                                    not (Classification.isStarTag t.tag)
                                        && t.tag
                                        /= "death_after_therapy"
                                        && t.tag
                                        /= processedTag
                                        && not (isReprocessTag reprocessTag t.tag)
                                )

                    newTags =
                        cleanedTags
                            ++ [ { tag = processedTag }
                               , { tag = Classification.relevanceToEmoji result.relevance }
                               ]
                            ++ (if result.deathAfterTherapy then
                                    [ { tag = "death_after_therapy" } ]

                                else
                                    []
                               )

                    -- Add target + version collections, remove old version keys
                    allVersionKeys =
                        Dict.values collections.versionKeys

                    collectionsWithoutOldVersions =
                        item.data.collections
                            |> List.filter (\c -> not (List.member c allVersionKeys))

                    newCollections =
                        collectionsWithoutOldVersions
                            ++ [ targetCollectionKey, collections.currentVersionKey ]
                            |> dedup

                    noteHtml =
                        Appraisal.generateNoteHtml appraisalData
                in
                -- Request 1: PATCH tags + collections + callNumber
                patchItem config item { tags = newTags, collections = newCollections, callNumber = callNumberJson }
                    -- Request 2: GET child notes
                    |> andThenResult (\_ -> getChildNotes config item.key)
                    -- Request 3: create or overwrite reasoning note
                    |> andThenResult (\childNotes -> handleNotes config item.key childNotes noteHtml)
            )


{-| Resolve the base AppraisalData for an item.

  - If callNumber has valid JSON, decode it.
  - If callNumber is empty but item has legacy CLAUDE data, migrate from tags.
  - Otherwise, start empty.

Note: legacy migration without note HTML — the full migration including note
content happens lazily when we GET child notes during reprocessing.

-}
resolveAppraisalData : ZoteroApi.ZoteroItem -> Appraisal.AppraisalData
resolveAppraisalData item =
    (case Decode.decodeString Appraisal.decode item.data.callNumber of
        Ok data ->
            data

        Err _ ->
            Appraisal.migrateFromLegacy
                { tags = item.data.tags
                , reasoningNoteHtml = Nothing
                }
                |> Maybe.withDefault Appraisal.empty
    )
        |> Appraisal.migrateToCurrentVersion


{-| Resolve AppraisalData with note content for full legacy migration.
-}
resolveAppraisalDataWithNotes : ZoteroApi.ZoteroItem -> Maybe String -> Appraisal.AppraisalData
resolveAppraisalDataWithNotes item reasoningNoteHtml =
    (case Decode.decodeString Appraisal.decode item.data.callNumber of
        Ok data ->
            data

        Err _ ->
            Appraisal.migrateFromLegacy
                { tags = item.data.tags
                , reasoningNoteHtml = reasoningNoteHtml
                }
                |> Maybe.withDefault Appraisal.empty
    )
        |> Appraisal.migrateToCurrentVersion


{-| Deduplicate a list preserving order.
-}
dedup : List String -> List String
dedup list =
    List.foldl
        (\item ( seen, acc ) ->
            if List.member item seen then
                ( seen, acc )

            else
                ( item :: seen, acc ++ [ item ] )
        )
        ( [], [] )
        list
        |> Tuple.second


{-| Handle note creation or overwrite.
The note is always fully regenerated from AppraisalData.

  - 0 existing reasoning notes → create new
  - 1 existing reasoning note → overwrite with new content
  - 2+ existing reasoning notes → overwrite first, delete extras

-}
handleNotes : Config -> String -> List ZoteroApi.ZoteroNote -> String -> BackendTask FatalError (Result String ())
handleNotes config parentItemKey childNotes noteHtml =
    let
        reasoningNotes =
            List.filter ZoteroApi.isReasoningNote childNotes
    in
    case reasoningNotes of
        [] ->
            createNote config parentItemKey noteHtml

        [ single ] ->
            patchNote config single noteHtml

        first :: rest ->
            patchNote config first noteHtml
                |> andThenResult (\_ -> deleteExtraNotes config rest)


deleteExtraNotes : Config -> List ZoteroApi.ZoteroNote -> BackendTask FatalError (Result String ())
deleteExtraNotes config notes =
    case notes of
        [] ->
            BackendTask.succeed (Ok ())

        note :: rest ->
            deleteItem config note.key note.version
                |> andThenResult (\_ -> deleteExtraNotes config rest)



-- Migration


{-| Fetch articles for migration.

  - No fromVersion: fetch all articles with CLAUDE tag.
  - Just N: fetch from version\_N, version\_(N+1), ..., version\_(current-1) collections.

-}
fetchItemsForMigration : Config -> Collections -> Maybe Int -> Int -> BackendTask FatalError (List ZoteroApi.ZoteroItem)
fetchItemsForMigration config collections fromVersion limit =
    let
        startVersion =
            fromVersion |> Maybe.withDefault 0

        -- Version 0 = legacy articles with CLAUDE tag but no version collection.
        -- These can only be found by tag search, not by collection.
        needsTagFetch =
            startVersion <= 0

        -- Versions 1+ have their own version collections
        collectionVersionsToFetch =
            List.range (max 1 startVersion) (Appraisal.currentSchemaVersion - 1)

        collectionKeysToFetch =
            collectionVersionsToFetch
                |> List.filterMap (\v -> Dict.get v collections.versionKeys)
    in
    -- Step 1: fetch legacy (tag-based) items if needed
    (if needsTagFetch then
        let
            url =
                zoteroBaseUrl config.zoteroLibraryId
                    ++ "/items?tag="
                    ++ processedTag
                    ++ "&limit="
                    ++ String.fromInt limit
                    ++ "&itemType=-note"
        in
        Script.log ("GET /items?tag=" ++ processedTag ++ " (migration: legacy v0)")
            |> BackendTask.andThen
                (\_ ->
                    BackendTask.Http.request
                        { url = url
                        , method = "GET"
                        , headers = zoteroHeaders config.zoteroApiKey
                        , body = BackendTask.Http.emptyBody
                        , retries = Just 1
                        , timeoutInMs = Just 30000
                        }
                        (BackendTask.Http.expectJson ZoteroApi.itemListDecoder)
                        |> BackendTask.allowFatal
                )

     else
        BackendTask.succeed []
    )
        -- Step 2: also fetch from version collections (for versions 1..current-1)
        |> BackendTask.andThen
            (\tagItems ->
                fetchItemsFromCollections config collectionKeysToFetch limit tagItems
            )


fetchItemsFromCollections : Config -> List String -> Int -> List ZoteroApi.ZoteroItem -> BackendTask FatalError (List ZoteroApi.ZoteroItem)
fetchItemsFromCollections config collectionKeys limit acc =
    case collectionKeys of
        [] ->
            BackendTask.succeed acc

        key :: rest ->
            let
                remaining =
                    limit - List.length acc

                url =
                    zoteroBaseUrl config.zoteroLibraryId
                        ++ "/collections/"
                        ++ key
                        ++ "/items?limit="
                        ++ String.fromInt (min 100 remaining)
                        ++ "&itemType=-note"
            in
            if remaining <= 0 then
                BackendTask.succeed acc

            else
                Script.log ("GET /collections/" ++ key ++ "/items (migration)")
                    |> BackendTask.andThen
                        (\_ ->
                            BackendTask.Http.request
                                { url = url
                                , method = "GET"
                                , headers = zoteroHeaders config.zoteroApiKey
                                , body = BackendTask.Http.emptyBody
                                , retries = Just 1
                                , timeoutInMs = Just 30000
                                }
                                (BackendTask.Http.expectJson ZoteroApi.itemListDecoder)
                                |> BackendTask.allowFatal
                        )
                    |> BackendTask.andThen
                        (\items ->
                            fetchItemsFromCollections config rest limit (acc ++ items)
                        )


{-| Migrate a single item: upgrade callNumber schema and create new note.
No AI calls, no tag changes. Existing notes are kept for comparison — since
the source of truth is now the structured callNumber data, old notes are
harmless clutter that can be cleaned up later.
-}
migrateItem : Config -> Collections -> ZoteroApi.ZoteroItem -> BackendTask FatalError (Result String ())
migrateItem config collections item =
    -- First GET child notes so we can use note content for legacy migration
    getChildNotes config item.key
        |> andThenResult
            (\childNotes ->
                let
                    reasoningNotes =
                        List.filter ZoteroApi.isReasoningNote childNotes

                    reasoningNoteHtml =
                        reasoningNotes |> List.head |> Maybe.map .note

                    appraisalData =
                        resolveAppraisalDataWithNotes item reasoningNoteHtml

                    callNumberJson =
                        Appraisal.encode appraisalData
                            |> Encode.encode 0

                    -- Remove old version collection keys, add current
                    allVersionKeys =
                        Dict.values collections.versionKeys

                    collectionsWithoutOldVersions =
                        item.data.collections
                            |> List.filter (\c -> not (List.member c allVersionKeys))

                    newCollections =
                        (collectionsWithoutOldVersions ++ [ collections.currentVersionKey ])
                            |> dedup

                    noteHtml =
                        Appraisal.generateNoteHtml appraisalData
                in
                -- PATCH callNumber + collections (keep existing tags)
                patchItem config item { tags = item.data.tags, collections = newCollections, callNumber = callNumberJson }
                    -- Always create a new note; keep existing ones for comparison
                    |> andThenResult (\_ -> createNote config item.key noteHtml)
            )


{-| Check if an item already has current schema version in callNumber.
-}
isAlreadyCurrentVersion : ZoteroApi.ZoteroItem -> Bool
isAlreadyCurrentVersion item =
    case Decode.decodeString Appraisal.decode item.data.callNumber of
        Ok data ->
            not (Appraisal.needsMigration data)

        Err _ ->
            False


migrateAll : Config -> Collections -> Maybe Int -> Int -> BackendTask FatalError ()
migrateAll config collections fromVersion maxArticles =
    migrateAllHelper config collections fromVersion maxArticles 1 emptyStats


migrateAllHelper : Config -> Collections -> Maybe Int -> Int -> Int -> Stats -> BackendTask FatalError ()
migrateAllHelper config collections fromVersion maxArticles batchNum totalStats =
    let
        batchSize =
            min 100
                (if maxArticles > 0 then
                    maxArticles - totalStats.processed

                 else
                    100
                )

        batchHeader =
            "\n" ++ String.repeat 60 "=" ++ "\nMIGRATION BATCH " ++ String.fromInt batchNum ++ "\n" ++ String.repeat 60 "="
    in
    Script.log batchHeader
        |> BackendTask.andThen (\_ -> fetchItemsForMigration config collections fromVersion batchSize)
        |> BackendTask.map (List.filter (\item -> not (isAlreadyCurrentVersion item)))
        |> BackendTask.andThen
            (\items ->
                Script.log ("\n🔄 Migrating " ++ String.fromInt (List.length items) ++ " articles...")
                    |> BackendTask.andThen (\_ -> migrateBatch config collections items 1 (List.length items) emptyStats)
            )
        |> BackendTask.andThen
            (\batchStats ->
                let
                    newTotal =
                        Stats.addStats totalStats batchStats
                in
                Script.log (Stats.formatSummary ("📊 Migration batch " ++ String.fromInt batchNum) batchStats)
                    |> BackendTask.andThen
                        (\_ ->
                            if batchStats.processed == 0 then
                                Script.log "\n✓ All articles migrated!"
                                    |> BackendTask.andThen (\_ -> printFinalSummary newTotal)

                            else if maxArticles > 0 && newTotal.processed >= maxArticles then
                                Script.log ("\n✓ Reached requested limit of " ++ String.fromInt maxArticles ++ " articles.")
                                    |> BackendTask.andThen (\_ -> printFinalSummary newTotal)

                            else
                                Script.log "\nWaiting before next batch..."
                                    |> BackendTask.andThen (\_ -> migrateAllHelper config collections fromVersion maxArticles (batchNum + 1) newTotal)
                        )
            )


migrateBatch :
    Config
    -> Collections
    -> List ZoteroApi.ZoteroItem
    -> Int
    -> Int
    -> Stats
    -> BackendTask FatalError Stats
migrateBatch config collections items idx total stats =
    case items of
        [] ->
            BackendTask.succeed stats

        item :: rest ->
            let
                article =
                    ZoteroApi.articleDataFromItem item
            in
            Script.log ("\n[" ++ String.fromInt idx ++ "/" ++ String.fromInt total ++ "] " ++ String.left 60 article.title ++ "...")
                |> BackendTask.andThen
                    (\_ ->
                        migrateItem config collections item
                            |> BackendTask.andThen
                                (\result ->
                                    case result of
                                        Ok _ ->
                                            Script.log "  ✓ Migrated"
                                                |> BackendTask.map (\_ -> ( { stats | processed = stats.processed + 1 }, Continue ))

                                        Err errMsg ->
                                            handleArticleError ("Migration failed — " ++ errMsg) stats
                                )
                    )
                |> BackendTask.andThen
                    (\( newStats, errAction ) ->
                        case errAction of
                            Abort ->
                                logCircuitBreaker "\n⛔ Circuit breaker: 5 HTTP errors in under 1 minute — aborting" newStats
                                    |> BackendTask.map (\_ -> newStats)

                            PauseAndRetry ->
                                logCircuitBreaker "\n⏸️  Circuit breaker: 5 HTTP errors in the last 5 minutes — taking a 5-minute break" newStats
                                    |> BackendTask.andThen (\_ -> Script.sleep 300000)
                                    |> BackendTask.andThen (\_ -> Script.log "\n▶️  Resuming after break...")
                                    |> BackendTask.andThen (\_ -> migrateBatch config collections (item :: rest) idx total { newStats | recentErrorTimestamps = [] })

                            Continue ->
                                migrateBatch config collections rest (idx + 1) total newStats
                    )



-- Batch processing (classification)


processAll : Config -> Collections -> Maybe String -> Int -> BackendTask FatalError ()
processAll config collections reprocessTag maxArticles =
    processAllHelper config collections reprocessTag maxArticles 1 emptyStats


processAllHelper : Config -> Collections -> Maybe String -> Int -> Int -> Stats -> BackendTask FatalError ()
processAllHelper config collections reprocessTag maxArticles batchNum totalStats =
    let
        batchSize =
            min 100
                (if maxArticles > 0 then
                    maxArticles - totalStats.processed

                 else
                    100
                )

        batchHeader =
            "\n" ++ String.repeat 60 "=" ++ "\nBATCH " ++ String.fromInt batchNum ++ "\n" ++ String.repeat 60 "="
    in
    Script.log batchHeader
        |> BackendTask.andThen (\_ -> fetchItems config reprocessTag batchSize)
        |> BackendTask.andThen
            (\items ->
                Script.log ("\n📚 Processing " ++ String.fromInt (List.length items) ++ " articles...")
                    |> BackendTask.andThen (\_ -> processBatch config collections reprocessTag items 1 (List.length items) emptyStats)
            )
        |> BackendTask.andThen
            (\batchStats ->
                let
                    newTotal =
                        Stats.addStats totalStats batchStats
                in
                Script.log (Stats.formatSummary ("📊 Batch " ++ String.fromInt batchNum) batchStats)
                    |> BackendTask.andThen
                        (\_ ->
                            if batchStats.processed == 0 then
                                Script.log "\n✓ All articles processed!"
                                    |> BackendTask.andThen (\_ -> printFinalSummary newTotal)

                            else if maxArticles > 0 && newTotal.processed >= maxArticles then
                                Script.log ("\n✓ Reached requested limit of " ++ String.fromInt maxArticles ++ " articles.")
                                    |> BackendTask.andThen (\_ -> printFinalSummary newTotal)

                            else
                                Script.log "\nWaiting before next batch..."
                                    |> BackendTask.andThen (\_ -> processAllHelper config collections reprocessTag maxArticles (batchNum + 1) newTotal)
                        )
            )


processBatch :
    Config
    -> Collections
    -> Maybe String
    -> List ZoteroApi.ZoteroItem
    -> Int
    -> Int
    -> Stats
    -> BackendTask FatalError Stats
processBatch config collections reprocessTag items idx total stats =
    case items of
        [] ->
            BackendTask.succeed stats

        item :: rest ->
            let
                article =
                    ZoteroApi.articleDataFromItem item
            in
            Script.log ("\n[" ++ String.fromInt idx ++ "/" ++ String.fromInt total ++ "] " ++ String.left 60 article.title ++ "...")
                |> BackendTask.andThen (\_ -> processArticle config collections reprocessTag item stats)
                |> BackendTask.andThen
                    (\( newStats, errAction ) ->
                        case errAction of
                            Abort ->
                                logCircuitBreaker "\n⛔ Circuit breaker: 5 HTTP errors in under 1 minute — aborting" newStats
                                    |> BackendTask.map (\_ -> newStats)

                            PauseAndRetry ->
                                logCircuitBreaker "\n⏸️  Circuit breaker: 5 HTTP errors in the last 5 minutes — taking a 5-minute break" newStats
                                    |> BackendTask.andThen (\_ -> Script.sleep 300000)
                                    |> BackendTask.andThen (\_ -> Script.log "\n▶️  Resuming after break...")
                                    |> BackendTask.andThen (\_ -> processBatch config collections reprocessTag (item :: rest) idx total { newStats | recentErrorTimestamps = [] })

                            Continue ->
                                processBatch config collections reprocessTag rest (idx + 1) total newStats
                    )


{-| Process a single article: classify then update Zotero.
Returns updated stats and the circuit breaker action to take.
-}
processArticle :
    Config
    -> Collections
    -> Maybe String
    -> ZoteroApi.ZoteroItem
    -> Stats
    -> BackendTask FatalError ( Stats, CircuitBreakerAction )
processArticle config collections reprocessTag item stats =
    let
        article =
            ZoteroApi.articleDataFromItem item
    in
    classifyArticle config article
        |> BackendTask.andThen
            (\classResult ->
                case classResult of
                    Ok result ->
                        let
                            hsctEmoji =
                                if result.deathAfterTherapy then
                                    " 🍎"

                                else
                                    ""

                            decision =
                                Classification.relevanceToDecision result.relevance
                        in
                        Script.log ("  → " ++ Classification.relevanceToEmoji result.relevance ++ hsctEmoji ++ ": " ++ result.reasoning ++ "   → TODO: " ++ result.note)
                            |> BackendTask.andThen (\_ -> updateItem config collections reprocessTag item result)
                            |> BackendTask.andThen
                                (\updateResult ->
                                    case updateResult of
                                        Ok _ ->
                                            let
                                                newStats =
                                                    { stats | processed = stats.processed + 1 }

                                                withDecision =
                                                    if Classification.isRefusal result then
                                                        { newStats | refusals = newStats.refusals + 1 }

                                                    else
                                                        case decision of
                                                            Classification.Include ->
                                                                { newStats | included = newStats.included + 1 }

                                                            Classification.Exclude ->
                                                                { newStats | excluded = newStats.excluded + 1 }
                                            in
                                            BackendTask.succeed ( withDecision, Continue )

                                        Err updateErr ->
                                            handleArticleError ("Update failed — " ++ updateErr) stats
                                )

                    Err errMsg ->
                        handleArticleError errMsg stats
            )


logCircuitBreaker : String -> Stats -> BackendTask FatalError ()
logCircuitBreaker message stats =
    BackendTask.Time.now
        |> BackendTask.andThen
            (\now ->
                let
                    nowMs =
                        Time.posixToMillis now
                in
                Script.log (message ++ "\n   Recent errors: " ++ Stats.formatErrorTimestamps nowMs stats.recentErrorTimestamps)
            )


handleArticleError : String -> Stats -> BackendTask FatalError ( Stats, CircuitBreakerAction )
handleArticleError errMsg stats =
    Script.log ("  ✗ Skipping — " ++ errMsg)
        |> BackendTask.andThen
            (\_ ->
                BackendTask.Time.now
                    |> BackendTask.map
                        (\now ->
                            let
                                nowMs =
                                    Time.posixToMillis now
                            in
                            Stats.recordErrorAndCheck nowMs stats
                        )
            )


printFinalSummary : Stats -> BackendTask FatalError ()
printFinalSummary stats =
    Script.log (Stats.formatSummary "FINAL SUMMARY" stats)
