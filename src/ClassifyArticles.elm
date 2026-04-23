module ClassifyArticles exposing (run)

import Analysis
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
import OpenAiApi
import Pages.Script as Script exposing (Script)
import Set
import Stats exposing (CircuitBreakerAction(..), Stats, emptyStats)
import Time
import ZoteroApi



-- Config


type alias Config =
    { zoteroLibraryId : String
    , zoteroApiKey : String
    , sourceCollection : String
    , systemPrompt : String
    , models : List ModelConfig
    }


type alias ModelConfig =
    { key : String
    , apiFormat : ApiFormat
    , model : String
    , apiKey : String
    , baseUrl : String
    , enabled : Bool
    , maxTokens : Int
    }


type ApiFormat
    = Anthropic
    | OpenAi


{-| Runtime configuration resolved from CLI flags and config.
-}
type alias RunConfig =
    { selectedModels : List ModelConfig
    , reprocess : Bool
    }


configJsonFile : String
configJsonFile =
    "config.json"


promptFile : String
promptFile =
    "prompt.txt"


secretsFile : String
secretsFile =
    "secrets.txt"



-- Constants


migrationParentCollection : String
migrationParentCollection =
    "data_schema_migrations"


versionCollectionPrefix : String
versionCollectionPrefix =
    "version_"


versionCollectionName : Int -> String
versionCollectionName n =
    versionCollectionPrefix ++ String.fromInt n


{-| Collection names for a model (e.g. "Claude included", "Claude excluded").
-}
modelRelevantCollection : ModelConfig -> String
modelRelevantCollection m =
    capitalize m.key ++ " included"


modelIrrelevantCollection : ModelConfig -> String
modelIrrelevantCollection m =
    capitalize m.key ++ " excluded"


analysisAutoIncludedCollection : String
analysisAutoIncludedCollection =
    "AI auto-included"


analysisAutoExcludedCollection : String
analysisAutoExcludedCollection =
    "AI auto-excluded"


capitalize : String -> String
capitalize s =
    case String.uncons s of
        Just ( first, rest ) ->
            String.fromChar (Char.toUpper first) ++ rest

        Nothing ->
            s



-- CLI


type alias CliOptions =
    { models : Maybe String
    , reprocess : Maybe String
    , max : Maybe String
    , migrate : Maybe String
    }


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.optionalKeywordArg "models"
                        |> Option.withDescription "Comma-separated model keys to run (default: all enabled models in config.json). Can only be combined with --max."
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "reprocess"
                        |> Option.withDescription "Comma-separated model keys to force re-run, overwriting existing appraisals. Can only be combined with --max."
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "max"
                        |> Option.withDescription "Maximum number of articles to process. 0 = all. Skips the interactive prompt. Can be combined with all other options."
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "migrate"
                        |> Option.withDescription "Migration-only mode (no AI calls). Optional integer = oldest schema version to migrate from. Can only be combined with --max. "
                    )
            )



-- Entry point


run : Script
run =
    Script.withCliOptions program
        (\options ->
            loadConfig
                |> BackendTask.andThen
                    (\config ->
                        resolveRunConfig config options
                            |> BackendTask.andThen
                                (\runConfig ->
                                    logBanner options runConfig
                                        |> BackendTask.andThen (\_ -> initAndProcess config runConfig options)
                                )
                    )
        )


logBanner : CliOptions -> RunConfig -> BackendTask FatalError ()
logBanner options runConfig =
    let
        modelNames =
            runConfig.selectedModels
                |> List.map (\m -> m.key ++ " (" ++ m.model ++ ")")
                |> String.join ", "

        modeMsg =
            case options.migrate of
                Just _ ->
                    "Mode: MIGRATE schema (no AI calls)"

                Nothing ->
                    if runConfig.reprocess then
                        "Mode: REPROCESS (overwrite existing appraisals)\nModels: " ++ modelNames

                    else
                        "Mode: process new articles\nModels: " ++ modelNames
    in
    Script.log ("🔬 PubMed Article Classifier for IEI Research\n" ++ String.repeat 60 "=" ++ "\n" ++ modeMsg)


{-| Validate CLI flags and resolve the run configuration.
-}
resolveRunConfig : Config -> CliOptions -> BackendTask FatalError RunConfig
resolveRunConfig config options =
    let
        allModelKeys =
            config.models |> List.map .key |> Set.fromList

        enabledModels =
            config.models |> List.filter .enabled
    in
    -- Validate mutual exclusion
    case ( options.models, options.reprocess, options.migrate ) of
        ( Nothing, Nothing, Just _ ) ->
            BackendTask.succeed { selectedModels = [], reprocess = False }

        ( Just modelsRaw, Nothing, Nothing ) ->
            let
                keys =
                    parseCommaList modelsRaw

                unknown =
                    keys |> List.filter (\k -> not (Set.member k allModelKeys))
            in
            if not (List.isEmpty unknown) then
                BackendTask.fail
                    (FatalError.fromString
                        ("Unknown model key(s) in --models: "
                            ++ String.join ", " unknown
                            ++ "\nAvailable: "
                            ++ String.join ", " (Set.toList allModelKeys)
                        )
                    )

            else
                let
                    keySet =
                        Set.fromList keys

                    selected =
                        config.models |> List.filter (\m -> Set.member m.key keySet)
                in
                if List.isEmpty selected then
                    BackendTask.fail (FatalError.fromString "--models must specify at least one model key.")

                else
                    BackendTask.succeed { selectedModels = selected, reprocess = False }

        ( Nothing, Just reprocessRaw, Nothing ) ->
            let
                keys =
                    parseCommaList reprocessRaw

                unknown =
                    keys |> List.filter (\k -> not (Set.member k allModelKeys))
            in
            if not (List.isEmpty unknown) then
                BackendTask.fail
                    (FatalError.fromString
                        ("Unknown model key(s) in --reprocess: "
                            ++ String.join ", " unknown
                            ++ "\nAvailable: "
                            ++ String.join ", " (Set.toList allModelKeys)
                        )
                    )

            else
                let
                    keySet =
                        Set.fromList keys

                    selected =
                        config.models |> List.filter (\m -> Set.member m.key keySet)
                in
                if List.isEmpty selected then
                    BackendTask.fail (FatalError.fromString "--reprocess must specify at least one model key.")

                else
                    BackendTask.succeed { selectedModels = selected, reprocess = True }

        ( Nothing, Nothing, Nothing ) ->
            if List.isEmpty enabledModels then
                BackendTask.fail (FatalError.fromString "No enabled models in config.json. Set \"enabled\": true on at least one model.")

            else
                BackendTask.succeed { selectedModels = enabledModels, reprocess = False }

        _ ->
            BackendTask.fail (FatalError.fromString "Invalid combination of CLI options.")

parseCommaList : String -> List String
parseCommaList raw =
    raw
        |> String.split ","
        |> List.map String.trim
        |> List.filter (\s -> s /= "")


initAndProcess : Config -> RunConfig -> CliOptions -> BackendTask FatalError ()
initAndProcess config runConfig options =
    Script.log ("✓ Loaded configuration for library: " ++ config.zoteroLibraryId)
        |> BackendTask.andThen (\_ -> resolveAllCollections config)
        |> BackendTask.andThen
            (\collections ->
                let
                    collectionSummary =
                        collections.modelCollections
                            |> List.map
                                (\mc ->
                                    "  " ++ mc.modelKey ++ ": included=" ++ mc.relevantKey ++ " excluded=" ++ mc.irrelevantKey
                                )
                            |> String.join "\n"
                in
                Script.log
                    ("✓ Source collection: "
                        ++ config.sourceCollection
                        ++ " ("
                        ++ collections.sourceKey
                        ++ ")\n✓ Model collections:\n"
                        ++ collectionSummary
                        ++ "\n✓ Current schema version: "
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
                                    processAll config runConfig collections maxArticles
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
    loadConfigJson
        |> BackendTask.andThen
            (\cfg ->
                resolveZoteroApiKey
                    |> BackendTask.andThen
                        (\zoteroApiKey ->
                            resolveModelApiKeys cfg.models
                                |> BackendTask.andThen
                                    (\resolvedModels ->
                                        loadPromptFile
                                            |> BackendTask.map
                                                (\prompt ->
                                                    { zoteroLibraryId = cfg.zoteroLibraryId
                                                    , zoteroApiKey = zoteroApiKey
                                                    , sourceCollection = cfg.sourceCollection
                                                    , systemPrompt = prompt
                                                    , models = resolvedModels
                                                    }
                                                )
                                    )
                        )
            )


{-| Raw config.json structure before API key resolution.
-}
type alias ConfigJson =
    { zoteroLibraryId : String
    , sourceCollection : String
    , models : List ConfigModelJson
    }


type alias ConfigModelJson =
    { key : String
    , apiFormat : String
    , model : String
    , apiKeyEnvVar : String
    , baseUrl : String
    , enabled : Bool
    , maxTokens : Int
    }


loadConfigJson : BackendTask FatalError ConfigJson
loadConfigJson =
    BackendTask.File.rawFile configJsonFile
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\content ->
                case Decode.decodeString configJsonDecoder content of
                    Ok cfg ->
                        if List.isEmpty cfg.models then
                            BackendTask.fail (FatalError.fromString "config.json must define at least one model.")

                        else
                            BackendTask.succeed cfg

                    Err err ->
                        BackendTask.fail
                            (FatalError.fromString
                                ("Failed to parse " ++ configJsonFile ++ ": " ++ Decode.errorToString err)
                            )
            )
        |> BackendTask.onError
            (\_ ->
                BackendTask.fail
                    (FatalError.fromString
                        ("Missing " ++ configJsonFile ++ ". Copy config.json.template to config.json and fill in your values.")
                    )
            )


configJsonDecoder : Decode.Decoder ConfigJson
configJsonDecoder =
    Decode.map3 ConfigJson
        (Decode.field "zoteroLibraryId" Decode.string)
        (Decode.field "sourceCollection" Decode.string)
        (Decode.field "models" (Decode.list configModelJsonDecoder))


configModelJsonDecoder : Decode.Decoder ConfigModelJson
configModelJsonDecoder =
    Decode.map7 ConfigModelJson
        (Decode.field "key" Decode.string)
        (Decode.field "apiFormat" Decode.string)
        (Decode.field "model" Decode.string)
        (Decode.field "apiKeyEnvVar" Decode.string)
        (Decode.field "baseUrl" Decode.string)
        (Decode.field "enabled" Decode.bool)
        (Decode.field "maxTokens" Decode.int)


{-| Resolve ZOTERO\_API\_KEY from env or secrets.txt fallback.
-}
resolveZoteroApiKey : BackendTask FatalError String
resolveZoteroApiKey =
    loadSecretsFile
        |> BackendTask.andThen
            (\fileVars ->
                resolveEnvVar "ZOTERO_API_KEY" fileVars
            )


{-| Resolve a single env var with secrets.txt fallback.
-}
resolveEnvVar : String -> Dict String String -> BackendTask FatalError String
resolveEnvVar name fileVars =
    Env.get name
        |> BackendTask.andThen
            (\envVal ->
                case envVal of
                    Just v ->
                        BackendTask.succeed v

                    Nothing ->
                        case Dict.get name fileVars of
                            Just v ->
                                BackendTask.succeed v

                            Nothing ->
                                BackendTask.fail
                                    (FatalError.fromString
                                        ("Missing environment variable: "
                                            ++ name
                                            ++ "\nSet it in your shell environment or in "
                                            ++ secretsFile
                                        )
                                    )
            )


{-| Resolve API keys for all configured models from env vars.
-}
resolveModelApiKeys : List ConfigModelJson -> BackendTask FatalError (List ModelConfig)
resolveModelApiKeys models =
    loadSecretsFile
        |> BackendTask.andThen
            (\fileVars ->
                resolveModelApiKeysHelper models fileVars []
            )


resolveModelApiKeysHelper : List ConfigModelJson -> Dict String String -> List ModelConfig -> BackendTask FatalError (List ModelConfig)
resolveModelApiKeysHelper models fileVars acc =
    case models of
        [] ->
            BackendTask.succeed (List.reverse acc)

        m :: rest ->
            resolveEnvVar m.apiKeyEnvVar fileVars
                |> BackendTask.andThen
                    (\apiKey ->
                        let
                            apiFormat =
                                case m.apiFormat of
                                    "anthropic" ->
                                        Anthropic

                                    _ ->
                                        OpenAi

                            modelConfig =
                                { key = m.key
                                , apiFormat = apiFormat
                                , model = m.model
                                , apiKey = apiKey
                                , baseUrl = m.baseUrl
                                , enabled = m.enabled
                                , maxTokens = m.maxTokens
                                }
                        in
                        resolveModelApiKeysHelper rest fileVars (modelConfig :: acc)
                    )


{-| Parse secrets.txt as KEY=VALUE lines (ignoring comments and blank lines).
-}
loadSecretsFile : BackendTask FatalError (Dict String String)
loadSecretsFile =
    BackendTask.File.rawFile secretsFile
        |> BackendTask.map parseSecretsFile
        |> BackendTask.onError (\_ -> BackendTask.succeed Dict.empty)


parseSecretsFile : String -> Dict String String
parseSecretsFile content =
    content
        |> String.lines
        |> List.filterMap
            (\line ->
                let
                    trimmed =
                        String.trim line
                in
                if String.startsWith "#" trimmed || trimmed == "" then
                    Nothing

                else
                    case String.split "=" trimmed of
                        key :: rest ->
                            Just ( String.trim key, String.trim (String.join "=" rest) )

                        [] ->
                            Nothing
            )
        |> Dict.fromList


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


type alias ModelCollectionKeys =
    { modelKey : String
    , relevantKey : String
    , irrelevantKey : String
    }


type alias Collections =
    { sourceKey : String
    , modelCollections : List ModelCollectionKeys
    , currentVersionKey : String
    , versionKeys : Dict Int String
    , analysisAutoIncludedKey : String
    , analysisAutoExcludedKey : String
    , analysisStarSumKeys : Dict Int String
    }


{-| Look up the collection keys for a specific model.
-}
collectionsForModel : String -> Collections -> Maybe ModelCollectionKeys
collectionsForModel modelKey collections =
    collections.modelCollections
        |> List.filter (\mc -> mc.modelKey == modelKey)
        |> List.head


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
loggedRequest label reqConfig expect =
    Script.log (reqConfig.method ++ " " ++ label)
        |> BackendTask.andThen
            (\_ ->
                BackendTask.Http.request reqConfig expect
                    |> BackendTask.map Ok
                    |> BackendTask.onError
                        (\_ ->
                            BackendTask.succeed (Err (reqConfig.method ++ " " ++ label ++ " failed"))
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


{-| Fetch all collections from Zotero, paginating through 100 at a time.
-}
fetchAllCollections : Config -> Int -> List ZoteroApi.ZoteroCollection -> BackendTask FatalError (List ZoteroApi.ZoteroCollection)
fetchAllCollections config start acc =
    let
        url =
            zoteroBaseUrl config.zoteroLibraryId ++ "/collections?limit=100&start=" ++ String.fromInt start
    in
    Script.log ("GET /collections?limit=100&start=" ++ String.fromInt start)
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
            (\batch ->
                let
                    all =
                        acc ++ batch
                in
                if List.length batch < 100 then
                    BackendTask.succeed all

                else
                    fetchAllCollections config (start + 100) all
            )


resolveAllCollections : Config -> BackendTask FatalError Collections
resolveAllCollections config =
    fetchAllCollections config 0 []
        |> BackendTask.andThen
            (\allCollections ->
                -- Check for duplicate collection names among collections we need
                let
                    starSumNames =
                        List.range 5 12 |> List.map (\n -> "Sum of " ++ String.fromInt n ++ " stars")

                    neededNames =
                        [ config.sourceCollection, migrationParentCollection, analysisAutoIncludedCollection, analysisAutoExcludedCollection ]
                            ++ starSumNames
                            ++ List.concatMap (\m -> [ modelRelevantCollection m, modelIrrelevantCollection m ]) config.models
                in
                case findDuplicateCollections neededNames allCollections of
                    Just errorMsg ->
                        BackendTask.fail (FatalError.fromString errorMsg)

                    Nothing ->
                        resolveSourceAndModelCollections config allCollections
                            |> BackendTask.andThen
                                (\( sourceKey, modelColls ) ->
                                    resolveVersionCollections config allCollections
                                        |> BackendTask.andThen
                                            (\( currentKey, vKeys ) ->
                                                resolveAnalysisCollections config allCollections
                                                    |> BackendTask.map
                                                        (\analysisCols ->
                                                            { sourceKey = sourceKey
                                                            , modelCollections = modelColls
                                                            , currentVersionKey = currentKey
                                                            , versionKeys = vKeys
                                                            , analysisAutoIncludedKey = analysisCols.autoIncludedKey
                                                            , analysisAutoExcludedKey = analysisCols.autoExcludedKey
                                                            , analysisStarSumKeys = analysisCols.starSumKeys
                                                            }
                                                        )
                                            )
                                )
            )


{-| Check if any needed collection name appears more than once in Zotero.
Returns an error message if duplicates found, Nothing if all clear.
-}
findDuplicateCollections : List String -> List ZoteroApi.ZoteroCollection -> Maybe String
findDuplicateCollections neededNames allCollections =
    let
        neededSet =
            Set.fromList neededNames

        duplicates =
            allCollections
                |> List.filter (\c -> Set.member c.name neededSet)
                |> List.foldl
                    (\c acc ->
                        Dict.update c.name
                            (\existing ->
                                case existing of
                                    Just count ->
                                        Just (count + 1)

                                    Nothing ->
                                        Just 1
                            )
                            acc
                    )
                    Dict.empty
                |> Dict.filter (\_ count -> count > 1)
    in
    if Dict.isEmpty duplicates then
        Nothing

    else
        let
            dupeList =
                duplicates
                    |> Dict.toList
                    |> List.map (\( name, count ) -> "\"" ++ name ++ "\" (" ++ String.fromInt count ++ " collections)")
                    |> String.join ", "
        in
        Just ("Duplicate collection names found in Zotero: " ++ dupeList ++ ". Each collection name used by this script must be unique.")


{-| Resolve source collection and per-model collections.
-}
resolveSourceAndModelCollections : Config -> List ZoteroApi.ZoteroCollection -> BackendTask FatalError ( String, List ModelCollectionKeys )
resolveSourceAndModelCollections config allCollections =
    ensureCollection config config.sourceCollection allCollections
        |> BackendTask.andThen
            (\sourceKey ->
                resolveModelCollectionsHelper config.models config allCollections []
                    |> BackendTask.map (\modelColls -> ( sourceKey, modelColls ))
            )


{-| Resolve included/excluded collections for each configured model.
Always resolves for ALL models in config, not just selected ones.
-}
resolveModelCollectionsHelper : List ModelConfig -> Config -> List ZoteroApi.ZoteroCollection -> List ModelCollectionKeys -> BackendTask FatalError (List ModelCollectionKeys)
resolveModelCollectionsHelper models config allCollections acc =
    case models of
        [] ->
            BackendTask.succeed (List.reverse acc)

        m :: rest ->
            BackendTask.map2 Tuple.pair
                (ensureCollection config (modelRelevantCollection m) allCollections)
                (ensureCollection config (modelIrrelevantCollection m) allCollections)
                |> BackendTask.andThen
                    (\( relKey, irrelKey ) ->
                        resolveModelCollectionsHelper rest
                            config
                            allCollections
                            ({ modelKey = m.key
                             , relevantKey = relKey
                             , irrelevantKey = irrelKey
                             }
                                :: acc
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


{-| Resolve the two fixed analysis collections and all "Sum of N stars" collections.
Star sums 5-15 cover every possible human review bucket.
-}
resolveAnalysisCollections : Config -> List ZoteroApi.ZoteroCollection -> BackendTask FatalError { autoIncludedKey : String, autoExcludedKey : String, starSumKeys : Dict Int String }
resolveAnalysisCollections config allCollections =
    BackendTask.map2 Tuple.pair
        (ensureCollection config analysisAutoIncludedCollection allCollections)
        (ensureCollection config analysisAutoExcludedCollection allCollections)
        |> BackendTask.andThen
            (\( autoInclKey, autoExclKey ) ->
                resolveStarSumCollections config allCollections (List.range 5 12) Dict.empty
                    |> BackendTask.map
                        (\starSumKeys ->
                            { autoIncludedKey = autoInclKey
                            , autoExcludedKey = autoExclKey
                            , starSumKeys = starSumKeys
                            }
                        )
            )


resolveStarSumCollections : Config -> List ZoteroApi.ZoteroCollection -> List Int -> Dict Int String -> BackendTask FatalError (Dict Int String)
resolveStarSumCollections config allCollections remaining acc =
    case remaining of
        [] ->
            BackendTask.succeed acc

        n :: rest ->
            let
                name =
                    "Sum of " ++ String.fromInt n ++ " stars"
            in
            ensureCollection config name allCollections
                |> BackendTask.andThen
                    (\key ->
                        resolveStarSumCollections config allCollections rest (Dict.insert n key acc)
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


{-| Fetch articles from the source collection.
-}
fetchFromSourceCollection : Config -> Collections -> Int -> BackendTask FatalError (List ZoteroApi.ZoteroItem)
fetchFromSourceCollection config collections limit =
    let
        url =
            zoteroBaseUrl config.zoteroLibraryId
                ++ "/collections/"
                ++ collections.sourceKey
                ++ "/items/top?limit="
                ++ String.fromInt (min 100 limit)
    in
    Script.log ("GET /collections/" ++ collections.sourceKey ++ "/items/top (source)")
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



-- AI API calls


{-| Classify an article with a specific model, dispatching to the right API format.
-}
classifyArticle : ModelConfig -> String -> ZoteroApi.ArticleData -> BackendTask FatalError (Result String Classification.ClassificationResult)
classifyArticle modelConfig systemPrompt article =
    let
        userMessage =
            Classification.userPrompt
                { title = article.title
                , abstract = article.abstract
                , keywords = article.keywords
                }
    in
    case modelConfig.apiFormat of
        Anthropic ->
            classifyWithAnthropic modelConfig systemPrompt userMessage

        OpenAi ->
            classifyWithOpenAi modelConfig systemPrompt userMessage


classifyWithAnthropic : ModelConfig -> String -> String -> BackendTask FatalError (Result String Classification.ClassificationResult)
classifyWithAnthropic modelConfig systemPrompt userMessage =
    let
        requestBody =
            AnthropicApi.encodeMessageRequest
                { model = modelConfig.model
                , maxTokens = modelConfig.maxTokens
                , systemPrompt = systemPrompt
                , userMessage = userMessage
                }
    in
    loggedRequest (modelConfig.key ++ " (Anthropic API)")
        { url = modelConfig.baseUrl ++ "/v1/messages"
        , method = "POST"
        , headers =
            [ ( "x-api-key", modelConfig.apiKey )
            , ( "anthropic-version", "2023-06-01" )
            , ( "Content-Type", "application/json" )
            ]
        , body = BackendTask.Http.jsonBody requestBody
        , retries = Nothing
        , timeoutInMs = Just 120000
        }
        (BackendTask.Http.expectJson AnthropicApi.messageResponseDecoder)
        |> BackendTask.andThen
            (\result ->
                case result of
                    Ok response ->
                        logCacheUsage modelConfig.key (anthropicCacheLog response.usage)
                            |> BackendTask.map (\_ -> Result.andThen parseAnthropicResponse (Ok response))

                    Err err ->
                        BackendTask.succeed (Err err)
            )


classifyWithOpenAi : ModelConfig -> String -> String -> BackendTask FatalError (Result String Classification.ClassificationResult)
classifyWithOpenAi modelConfig systemPrompt userMessage =
    let
        requestBody =
            OpenAiApi.encodeChatRequest
                { model = modelConfig.model
                , maxTokens = modelConfig.maxTokens
                , systemPrompt = systemPrompt
                , userMessage = userMessage
                }
    in
    loggedRequest (modelConfig.key ++ " (OpenAI-compatible API)")
        { url = modelConfig.baseUrl ++ "/chat/completions"
        , method = "POST"
        , headers =
            [ ( "Authorization", "Bearer " ++ modelConfig.apiKey )
            , ( "Content-Type", "application/json" )
            ]
        , body = BackendTask.Http.jsonBody requestBody
        , retries = Nothing
        , timeoutInMs = Just 120000
        }
        (BackendTask.Http.expectJson OpenAiApi.chatResponseDecoder)
        |> BackendTask.andThen
            (\result ->
                case result of
                    Ok response ->
                        logCacheUsage modelConfig.key (openAiCacheLog response)
                            |> BackendTask.map (\_ -> Result.andThen parseOpenAiResponse (Ok response))

                    Err err ->
                        BackendTask.succeed (Err err)
            )


parseAnthropicResponse : AnthropicApi.MessageResponse -> Result String Classification.ClassificationResult
parseAnthropicResponse response =
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


parseOpenAiResponse : OpenAiApi.ChatResponse -> Result String Classification.ClassificationResult
parseOpenAiResponse response =
    case response.finishReason of
        OpenAiApi.ContentFilter ->
            Ok Classification.refusalResult

        _ ->
            let
                textContent =
                    OpenAiApi.extractText response

                cleanedJson =
                    textContent |> String.trim |> AnthropicApi.stripJsonFences
            in
            case Decode.decodeString Classification.classificationResultDecoder cleanedJson of
                Ok result ->
                    Ok result

                Err err ->
                    Err ("JSON decode error: " ++ Decode.errorToString err ++ " | Raw: " ++ textContent)


{-| Log cache usage for a model, if any cached tokens were used.
-}
logCacheUsage : String -> Maybe String -> BackendTask FatalError ()
logCacheUsage modelKey maybeCacheInfo =
    case maybeCacheInfo of
        Just info ->
            Script.log ("  💾 [" ++ modelKey ++ "] " ++ info)

        Nothing ->
            BackendTask.succeed ()


anthropicCacheLog : AnthropicApi.CacheUsage -> Maybe String
anthropicCacheLog usage =
    if usage.cacheReadInputTokens > 0 then
        Just ("cache hit: " ++ String.fromInt usage.cacheReadInputTokens ++ " tokens read from cache")

    else if usage.cacheCreationInputTokens > 0 then
        Just ("cache miss: " ++ String.fromInt usage.cacheCreationInputTokens ++ " tokens written to cache")

    else
        Nothing


openAiCacheLog : OpenAiApi.ChatResponse -> Maybe String
openAiCacheLog response =
    if response.cachedTokens > 0 then
        Just
            ("cache hit: "
                ++ String.fromInt response.cachedTokens
                ++ "/"
                ++ String.fromInt response.promptTokens
                ++ " prompt tokens cached"
            )

    else
        Nothing


{-| Classify an article with all applicable models in parallel using BackendTask.andMap.
Returns a list of (modelKey, Result) pairs.
-}
classifyWithAllModels :
    Config
    -> ZoteroApi.ArticleData
    -> List ModelConfig
    -> BackendTask FatalError (List ( String, Result String Classification.ClassificationResult ))
classifyWithAllModels config article models =
    let
        classifyOne m =
            classifyArticle m config.systemPrompt article
                |> BackendTask.map (\result -> ( m.key, result ))
    in
    combineBackendTasks (List.map classifyOne models)


{-| Combine a list of BackendTasks into a BackendTask of a list, running in parallel.
Uses BackendTask.andMap for parallel execution.
-}
combineBackendTasks : List (BackendTask FatalError a) -> BackendTask FatalError (List a)
combineBackendTasks tasks =
    List.foldl
        (\task acc ->
            BackendTask.succeed (\list item -> list ++ [ item ])
                |> BackendTask.andMap acc
                |> BackendTask.andMap task
        )
        (BackendTask.succeed [])
        tasks



-- Article screening logic


{-| Determine which models need to screen this article.

  - Reprocess mode: all selected models (force re-run)
  - Normal mode: only selected models without an existing appraisal

-}
modelsForArticle : RunConfig -> ZoteroApi.ZoteroItem -> List ModelConfig
modelsForArticle runConfig item =
    if runConfig.reprocess then
        runConfig.selectedModels

    else
        let
            existingAppraisalKeys =
                case Decode.decodeString Appraisal.decode item.data.callNumber of
                    Ok data ->
                        data.appraisals |> Dict.keys |> Set.fromList

                    Err _ ->
                        Set.empty
        in
        runConfig.selectedModels
            |> List.filter (\m -> not (Set.member m.key existingAppraisalKeys))


{-| Compute the minimum star rating across existing tags and new results.
The most conservative model wins — returns the lowest relevance.
-}
minStarRelevance : List ZoteroApi.ZoteroTag -> List Classification.ClassificationResult -> Maybe Classification.Relevance
minStarRelevance existingTags newResults =
    let
        existingStars =
            existingTags
                |> List.filterMap (\t -> Classification.emojiToRelevance t.tag)
                |> List.map Classification.relevanceToInt

        newStars =
            newResults
                |> List.map (\r -> Classification.relevanceToInt r.relevance)

        allStars =
            existingStars ++ newStars

        finalInt =
            List.minimum allStars
                |> Maybe.withDefault 0
    in
    Classification.intToRelevance finalInt


{-| Check if death_after_therapy is flagged by ANY appraisal (existing or new).
-}
anyDeathAfterTherapy : Appraisal.AppraisalData -> List Classification.ClassificationResult -> Bool
anyDeathAfterTherapy existingData newResults =
    let
        existingHasIt =
            existingData.appraisals
                |> Dict.values
                |> List.any .deathAfterTherapy

        newHasIt =
            newResults |> List.any .deathAfterTherapy
    in
    existingHasIt || newHasIt



-- Item update


{-| Update a Zotero item after classification by one or more models.

1.  Build AppraisalData from existing callNumber (or start empty).
2.  Insert all new model appraisals.
3.  PATCH item: tags, collections, and callNumber (structured JSON).
4.  GET child notes, then create/overwrite the reasoning note.

-}
updateItem :
    Config
    -> Collections
    -> ZoteroApi.ZoteroItem
    -> List ( String, Classification.ClassificationResult )
    -> Bool
    -> BackendTask FatalError (Result String ())
updateItem config collections item modelResults allSucceeded =
    BackendTask.Time.now
        |> BackendTask.andThen
            (\now ->
                let
                    timestamp =
                        Iso8601.fromTime now

                    modelConfigFor key =
                        config.models
                            |> List.filter (\m -> m.key == key)
                            |> List.head
                            |> Maybe.map .model
                            |> Maybe.withDefault key

                    -- Don't attempt legacy migration — just start empty if callNumber is invalid
                    baseAppraisalData =
                        case Decode.decodeString Appraisal.decode item.data.callNumber of
                            Ok data ->
                                Appraisal.migrateToCurrentVersion data

                            Err _ ->
                                Appraisal.empty

                    -- Insert all model appraisals
                    appraisalDataWithoutAnalysis =
                        List.foldl
                            (\( key, result ) acc ->
                                let
                                    appraisal =
                                        Appraisal.fromClassificationResult
                                            { model = modelConfigFor key, timestamp = timestamp }
                                            result
                                in
                                Appraisal.setAppraisal key appraisal acc
                            )
                            baseAppraisalData
                            modelResults

                    -- Compute analysis if all enabled models have appraisals
                    enabledModelKeys =
                        config.models
                            |> List.filter .enabled
                            |> List.map .key
                            |> Set.fromList

                    existingAppraisalKeys =
                        appraisalDataWithoutAnalysis.appraisals
                            |> Dict.keys
                            |> Set.fromList

                    allModelsComplete =
                        Set.diff enabledModelKeys existingAppraisalKeys |> Set.isEmpty

                    analysis =
                        if allModelsComplete && not (Set.isEmpty enabledModelKeys) then
                            Just (Analysis.compute appraisalDataWithoutAnalysis.appraisals)

                        else
                            Nothing

                    appraisalData =
                        { appraisalDataWithoutAnalysis | analysis = analysis }

                    callNumberJson =
                        Appraisal.encode appraisalData
                            |> Encode.encode 0

                    newResultValues =
                        List.map Tuple.second modelResults

                    -- Star tag: min across existing + new, most conservative wins
                    starRelevance =
                        minStarRelevance item.data.tags newResultValues

                    -- death_after_therapy: check ALL appraisals (existing + new)
                    hasDeath =
                        anyDeathAfterTherapy appraisalData newResultValues

                    -- Build per-model target collections from results
                    perModelCollectionKeys =
                        modelResults
                            |> List.filterMap
                                (\( key, result ) ->
                                    collectionsForModel key collections
                                        |> Maybe.map
                                            (\mc ->
                                                case Classification.relevanceToDecision result.relevance of
                                                    Classification.Include ->
                                                        mc.relevantKey

                                                    Classification.Exclude ->
                                                        mc.irrelevantKey
                                            )
                                )

                    -- Clean tags: remove star tags, death_after_therapy, and analysis tags (we'll re-add the right ones)
                    cleanedTags =
                        item.data.tags
                            |> List.filter
                                (\t ->
                                    not (Classification.isStarTag t.tag)
                                        && not (Analysis.isAnalysisTag t.tag)
                                        && t.tag
                                        /= "death_after_therapy"
                                )

                    analysisTag =
                        analysis
                            |> Maybe.map (\a -> [ { tag = Analysis.categoryToTag a } ])
                            |> Maybe.withDefault []

                    newTags =
                        cleanedTags
                            ++ (case starRelevance of
                                    Just stars ->
                                        [ { tag = Classification.relevanceToEmoji stars } ]

                                    Nothing ->
                                        []
                               )
                            ++ (if hasDeath then
                                    [ { tag = "death_after_therapy" } ]

                                else
                                    []
                               )
                            ++ analysisTag

                    -- Collections: add per-model targets + version, remove source if all succeeded
                    allVersionKeys =
                        Dict.values collections.versionKeys

                    -- Also strip any existing analysis collection keys before re-adding
                    allAnalysisCollectionKeys =
                        [ collections.analysisAutoIncludedKey, collections.analysisAutoExcludedKey ]
                            ++ Dict.values collections.analysisStarSumKeys

                    baseCollections =
                        item.data.collections
                            |> List.filter
                                (\c ->
                                    not (List.member c allVersionKeys)
                                        && not (List.member c allAnalysisCollectionKeys)
                                )

                    collectionsWithoutSource =
                        if allSucceeded then
                            baseCollections |> List.filter (\c -> c /= collections.sourceKey)

                        else
                            baseCollections

                    analysisCollectionKey =
                        analysis
                            |> Maybe.andThen
                                (\a ->
                                    case a.category of
                                        Analysis.AutoIncluded ->
                                            Just collections.analysisAutoIncludedKey

                                        Analysis.AutoExcluded ->
                                            Just collections.analysisAutoExcludedKey

                                        Analysis.HumanReview ->
                                            Dict.get a.totalStars collections.analysisStarSumKeys
                                )
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []

                    newCollections =
                        (collectionsWithoutSource
                            ++ perModelCollectionKeys
                            ++ ( collections.currentVersionKey :: analysisCollectionKey )
                        )
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


{-| Resolve the base AppraisalData for an item (migration mode).

  - If callNumber has valid JSON, decode it.
  - If callNumber is empty but item has legacy CLAUDE data, migrate from tags.
  - Otherwise, start empty.

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


{-| Handle note creation or update.
Only touches the auto-generated note (identified by the disclaimer marker).
Legacy reasoning notes and user-created notes are never modified or deleted.

  - No existing auto-generated note → create new
  - Existing auto-generated note → overwrite with fresh content

-}
handleNotes : Config -> String -> List ZoteroApi.ZoteroNote -> String -> BackendTask FatalError (Result String ())
handleNotes config parentItemKey childNotes noteHtml =
    let
        autoGeneratedNote =
            childNotes
                |> List.filter (\n -> String.contains "auto-generated from structured data" n.note)
                |> List.head
    in
    case autoGeneratedNote of
        Nothing ->
            createNote config parentItemKey noteHtml

        Just existing ->
            patchNote config existing noteHtml



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

        collectionKeysToFetch =
            List.range startVersion (Appraisal.currentSchemaVersion - 1)
                |> List.filterMap (\v -> Dict.get v collections.versionKeys)
    in
    fetchItemsFromCollections config collectionKeysToFetch limit []


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
                        ++ "/items/top?limit="
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


{-| Prepared migration data for a single item, ready for batch upload.
-}
type alias MigrationPatch =
    { itemKey : String
    , itemVersion : Int
    , tags : List ZoteroApi.ZoteroTag
    , collections : List String
    , callNumber : String
    , noteHtml : String
    }


{-| Prepare migration data for a single item by fetching child notes
and computing the new callNumber and note HTML. No writes happen here.
-}
prepareMigrateItem : Config -> Collections -> ZoteroApi.ZoteroItem -> BackendTask FatalError (Result String MigrationPatch)
prepareMigrateItem config collections item =
    getChildNotes config item.key
        |> andThenResult
            (\childNotes ->
                let
                    reasoningNotes =
                        List.filter ZoteroApi.isReasoningNote childNotes

                    reasoningNoteHtml =
                        reasoningNotes |> List.head |> Maybe.map .note

                    baseAppraisalData =
                        resolveAppraisalDataWithNotes item reasoningNoteHtml

                    -- Compute analysis if all enabled models have appraisals
                    enabledModelKeys =
                        config.models
                            |> List.filter .enabled
                            |> List.map .key
                            |> Set.fromList

                    existingAppraisalKeys =
                        baseAppraisalData.appraisals
                            |> Dict.keys
                            |> Set.fromList

                    allModelsComplete =
                        Set.diff enabledModelKeys existingAppraisalKeys |> Set.isEmpty

                    analysis =
                        if allModelsComplete && not (Set.isEmpty enabledModelKeys) then
                            Just (Analysis.compute baseAppraisalData.appraisals)

                        else
                            Nothing

                    appraisalData =
                        { baseAppraisalData | analysis = analysis }

                    callNumberJson =
                        Appraisal.encode appraisalData
                            |> Encode.encode 0

                    allVersionKeys =
                        Dict.values collections.versionKeys

                    allAnalysisCollectionKeys =
                        [ collections.analysisAutoIncludedKey, collections.analysisAutoExcludedKey ]
                            ++ Dict.values collections.analysisStarSumKeys

                    collectionsWithoutOldVersionsOrAnalysis =
                        item.data.collections
                            |> List.filter
                                (\c ->
                                    not (List.member c allVersionKeys)
                                        && not (List.member c allAnalysisCollectionKeys)
                                )

                    analysisCollectionKey =
                        analysis
                            |> Maybe.andThen
                                (\a ->
                                    case a.category of
                                        Analysis.AutoIncluded ->
                                            Just collections.analysisAutoIncludedKey

                                        Analysis.AutoExcluded ->
                                            Just collections.analysisAutoExcludedKey

                                        Analysis.HumanReview ->
                                            Dict.get a.totalStars collections.analysisStarSumKeys
                                )
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []

                    newCollections =
                        (collectionsWithoutOldVersionsOrAnalysis
                            ++ ( collections.currentVersionKey :: analysisCollectionKey )
                        )
                            |> dedup

                    -- Clean and rebuild tags with analysis tag
                    cleanedTags =
                        item.data.tags
                            |> List.filter (\t -> not (Analysis.isAnalysisTag t.tag))

                    analysisTag =
                        analysis
                            |> Maybe.map (\a -> [ { tag = Analysis.categoryToTag a } ])
                            |> Maybe.withDefault []

                    newTags =
                        cleanedTags ++ analysisTag
                in
                BackendTask.succeed
                    (Ok
                        { itemKey = item.key
                        , itemVersion = item.version
                        , tags = newTags
                        , collections = newCollections
                        , callNumber = callNumberJson
                        , noteHtml = Appraisal.generateNoteHtml appraisalData
                        }
                    )
            )


{-| Send a batch of item patches via POST /items (max 50 per request).
-}
batchPatchItems : Config -> List MigrationPatch -> BackendTask FatalError (Result String ())
batchPatchItems config patches =
    let
        encoded =
            ZoteroApi.encodeBatchItemPatch
                (List.map
                    (\p ->
                        { key = p.itemKey
                        , version = p.itemVersion
                        , tags = p.tags
                        , collections = p.collections
                        , callNumber = p.callNumber
                        }
                    )
                    patches
                )
    in
    loggedRequest ("POST /items (batch update " ++ String.fromInt (List.length patches) ++ " items)")
        { url = zoteroBaseUrl config.zoteroLibraryId ++ "/items"
        , method = "POST"
        , headers = zoteroHeaders config.zoteroApiKey
        , body = BackendTask.Http.jsonBody encoded
        , retries = Nothing
        , timeoutInMs = Just 60000
        }
        (BackendTask.Http.expectWhatever ())


{-| Send a batch of new notes via POST /items (max 50 per request).
-}
batchCreateNotes : Config -> List MigrationPatch -> BackendTask FatalError (Result String ())
batchCreateNotes config patches =
    let
        encoded =
            ZoteroApi.encodeBatchCreateNotes
                (List.map (\p -> { parentItemKey = p.itemKey, noteHtml = p.noteHtml }) patches)
    in
    loggedRequest ("POST /items (batch create " ++ String.fromInt (List.length patches) ++ " notes)")
        { url = zoteroBaseUrl config.zoteroLibraryId ++ "/items"
        , method = "POST"
        , headers = zoteroHeaders config.zoteroApiKey
        , body = BackendTask.Http.jsonBody encoded
        , retries = Nothing
        , timeoutInMs = Just 60000
        }
        (BackendTask.Http.expectWhatever ())


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
                let
                    count =
                        List.length items
                in
                Script.log ("\n🔄 Migrating " ++ String.fromInt count ++ " articles...")
                    |> BackendTask.andThen (\_ -> migrateBatch config collections items emptyStats)
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


{-| Migrate a batch of items: prepare patches one-by-one (fetching child notes),
then upload in bulk via Zotero's multi-object write API (up to 50 per request).
-}
migrateBatch :
    Config
    -> Collections
    -> List ZoteroApi.ZoteroItem
    -> Stats
    -> BackendTask FatalError Stats
migrateBatch config collections items stats =
    prepareMigrationPatches config collections items 1 (List.length items) []
        |> BackendTask.andThen
            (\patches ->
                let
                    patchCount =
                        List.length patches

                    failCount =
                        List.length items - patchCount
                in
                sendMigrationChunks config patches
                    |> BackendTask.map
                        (\writeResult ->
                            case writeResult of
                                Ok _ ->
                                    { stats
                                        | processed = stats.processed + patchCount
                                        , completed = stats.completed + patchCount
                                        , failed = stats.failed + failCount
                                        , errors = stats.errors + failCount
                                    }

                                Err _ ->
                                    { stats
                                        | processed = stats.processed + List.length items
                                        , failed = stats.failed + List.length items
                                        , errors = stats.errors + List.length items
                                    }
                        )
            )


prepareMigrationPatches :
    Config
    -> Collections
    -> List ZoteroApi.ZoteroItem
    -> Int
    -> Int
    -> List MigrationPatch
    -> BackendTask FatalError (List MigrationPatch)
prepareMigrationPatches config collections items idx total acc =
    case items of
        [] ->
            BackendTask.succeed (List.reverse acc)

        item :: rest ->
            let
                article =
                    ZoteroApi.articleDataFromItem item
            in
            Script.log ("[" ++ String.fromInt idx ++ "/" ++ String.fromInt total ++ "] Preparing: " ++ String.left 60 article.title ++ "...")
                |> BackendTask.andThen (\_ -> prepareMigrateItem config collections item)
                |> BackendTask.andThen
                    (\result ->
                        case result of
                            Ok patch ->
                                prepareMigrationPatches config collections rest (idx + 1) total (patch :: acc)

                            Err errMsg ->
                                Script.log ("  ⚠ Skipped: " ++ errMsg)
                                    |> BackendTask.andThen (\_ -> prepareMigrationPatches config collections rest (idx + 1) total acc)
                    )


sendMigrationChunks : Config -> List MigrationPatch -> BackendTask FatalError (Result String ())
sendMigrationChunks config patches =
    let
        chunks =
            chunk 50 patches
    in
    sendChunksHelper config chunks 1 (List.length chunks)


sendChunksHelper : Config -> List (List MigrationPatch) -> Int -> Int -> BackendTask FatalError (Result String ())
sendChunksHelper config chunks idx total =
    case chunks of
        [] ->
            BackendTask.succeed (Ok ())

        batch :: rest ->
            Script.log ("\n📤 Uploading chunk " ++ String.fromInt idx ++ "/" ++ String.fromInt total ++ " (" ++ String.fromInt (List.length batch) ++ " items)...")
                |> BackendTask.andThen (\_ -> batchPatchItems config batch)
                |> andThenResult (\_ -> batchCreateNotes config batch)
                |> andThenResult (\_ -> sendChunksHelper config rest (idx + 1) total)


{-| Split a list into chunks of at most n elements.
-}
chunk : Int -> List a -> List (List a)
chunk n list =
    if List.isEmpty list then
        []

    else
        List.take n list :: chunk n (List.drop n list)



-- Batch processing (classification)


processAll : Config -> RunConfig -> Collections -> Int -> BackendTask FatalError ()
processAll config runConfig collections maxArticles =
    processAllHelper config runConfig collections maxArticles 1 emptyStats


processAllHelper : Config -> RunConfig -> Collections -> Int -> Int -> Stats -> BackendTask FatalError ()
processAllHelper config runConfig collections maxArticles batchNum totalStats =
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
        |> BackendTask.andThen (\_ -> fetchFromSourceCollection config collections batchSize)
        |> BackendTask.andThen
            (\items ->
                Script.log ("\n📚 Processing " ++ String.fromInt (List.length items) ++ " articles...")
                    |> BackendTask.andThen (\_ -> processBatch config runConfig collections items 1 (List.length items) emptyStats)
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
                                    |> BackendTask.andThen (\_ -> processAllHelper config runConfig collections maxArticles (batchNum + 1) newTotal)
                        )
            )


processBatch :
    Config
    -> RunConfig
    -> Collections
    -> List ZoteroApi.ZoteroItem
    -> Int
    -> Int
    -> Stats
    -> BackendTask FatalError Stats
processBatch config runConfig collections items idx total stats =
    case items of
        [] ->
            BackendTask.succeed stats

        item :: rest ->
            let
                article =
                    ZoteroApi.articleDataFromItem item
            in
            Script.log ("\n[" ++ String.fromInt idx ++ "/" ++ String.fromInt total ++ "] " ++ String.left 60 article.title ++ "...")
                |> BackendTask.andThen (\_ -> processArticle config runConfig collections item stats)
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
                                    |> BackendTask.andThen (\_ -> processBatch config runConfig collections (item :: rest) idx total { newStats | recentErrorTimestamps = [] })

                            Continue ->
                                processBatch config runConfig collections rest (idx + 1) total newStats
                    )


{-| Process a single article: classify with all applicable models in parallel, then update Zotero.
Returns updated stats and the circuit breaker action to take.
-}
processArticle :
    Config
    -> RunConfig
    -> Collections
    -> ZoteroApi.ZoteroItem
    -> Stats
    -> BackendTask FatalError ( Stats, CircuitBreakerAction )
processArticle config runConfig collections item stats =
    let
        article =
            ZoteroApi.articleDataFromItem item

        applicableModels =
            modelsForArticle runConfig item

        modelNames =
            applicableModels |> List.map .key |> String.join ", "
    in
    if List.isEmpty applicableModels then
        -- All selected models have already screened this item — run analysis + migrate schema, then remove from source
        Script.log "  → Already screened by all selected models, updating analysis and removing from source"
            |> BackendTask.andThen
                (\_ ->
                    updateItem config collections item [] True
                        |> BackendTask.map (\_ -> ( stats, Continue ))
                )

    else
        Script.log ("  → Screening with: " ++ modelNames)
            |> BackendTask.andThen (\_ -> classifyWithAllModels config article applicableModels)
            |> BackendTask.andThen
                (\results ->
                    let
                        successes =
                            results
                                |> List.filterMap
                                    (\( key, r ) ->
                                        case r of
                                            Ok result ->
                                                Just ( key, result )

                                            Err _ ->
                                                Nothing
                                    )

                        failures =
                            results
                                |> List.filterMap
                                    (\( key, r ) ->
                                        case r of
                                            Err msg ->
                                                Just ( key, msg )

                                            Ok _ ->
                                                Nothing
                                    )

                        allSucceeded =
                            List.isEmpty failures
                    in
                    logModelResults successes failures
                        |> BackendTask.andThen
                            (\_ ->
                                if List.isEmpty successes then
                                    let
                                        errMsg =
                                            failures |> List.map (\( k, msg ) -> k ++ ": " ++ msg) |> String.join "; "

                                        failedStats =
                                            updateStatsFromResults stats [] failures
                                    in
                                    handleArticleError errMsg failedStats

                                else
                                    updateItem config collections item successes allSucceeded
                                        |> BackendTask.andThen
                                            (\updateResult ->
                                                case updateResult of
                                                    Ok _ ->
                                                        let
                                                            newStats =
                                                                updateStatsFromResults stats successes failures
                                                        in
                                                        if allSucceeded then
                                                            BackendTask.succeed ( newStats, Continue )

                                                        else
                                                            let
                                                                failMsg =
                                                                    failures |> List.map Tuple.first |> String.join ", "
                                                            in
                                                            Script.log ("  ⚠ Partial: " ++ failMsg ++ " failed, but item updated with successful results (kept in source)")
                                                                |> BackendTask.map (\_ -> ( newStats, Continue ))

                                                    Err updateErr ->
                                                        handleArticleError ("Update failed — " ++ updateErr)
                                                            { stats
                                                                | processed = stats.processed + 1
                                                                , failed = stats.failed + 1
                                                                , errors = stats.errors + 1
                                                            }
                                            )
                            )
                )


{-| Log classification results for each model.
-}
logModelResults : List ( String, Classification.ClassificationResult ) -> List ( String, String ) -> BackendTask FatalError ()
logModelResults successes failures =
    let
        successLogs =
            successes
                |> List.map
                    (\( key, result ) ->
                        let
                            hsctEmoji =
                                if result.deathAfterTherapy then
                                    " 🍎"

                                else
                                    ""
                        in
                        "  → [" ++ key ++ "] " ++ Classification.relevanceToEmoji result.relevance ++ hsctEmoji ++ ": " ++ result.reasoning
                    )

        failLogs =
            failures |> List.map (\( key, msg ) -> "  ✗ [" ++ key ++ "] " ++ msg)

        allLogs =
            successLogs ++ failLogs
    in
    Script.log (String.join "\n" allLogs)


{-| Update stats based on all model results for a single article.
Counts every model's decision individually, and tracks the article-level outcome.
-}
updateStatsFromResults : Stats -> List ( String, Classification.ClassificationResult ) -> List ( String, String ) -> Stats
updateStatsFromResults stats successes failures =
    let
        countDecisions s results =
            case results of
                [] ->
                    s

                ( _, result ) :: rest ->
                    let
                        updated =
                            if Classification.isRefusal result then
                                { s | refusals = s.refusals + 1 }

                            else
                                case Classification.relevanceToDecision result.relevance of
                                    Classification.Include ->
                                        { s | included = s.included + 1 }

                                    Classification.Exclude ->
                                        { s | excluded = s.excluded + 1 }
                    in
                    countDecisions updated rest

        errorCount =
            List.length failures

        articleOutcome =
            if List.isEmpty failures then
                { stats | completed = stats.completed + 1 }

            else if List.isEmpty successes then
                { stats | failed = stats.failed + 1 }

            else
                { stats | partial = stats.partial + 1 }
    in
    countDecisions
        { articleOutcome
            | processed = articleOutcome.processed + 1
            , errors = articleOutcome.errors + errorCount
        }
        successes


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
