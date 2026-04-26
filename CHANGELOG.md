# Changelog

## v0.6.0 — Unexpected legacy

### Appraisal key renaming via `rename_from`

When swapping models (e.g. upgrading from `deepseek-reasoner` to `deepseek-v4-pro`), previous appraisals would be orphaned under the old key. Now you can tell the system to rename them.

Add `"rename_from"` with a timestamp cutoff to a model entry in `config.json`:
```json
{
  "key": "deepseek-legacy",
  "rename_from": { "key": "deepseek", "before": "2026-04-24T00:00:00.000Z" },
  "enabled": false,
  ...
}
```

Renames are applied automatically whenever an article is processed (classification or migration), **before** new appraisals are inserted. This means:
1. Old `"deepseek"` appraisal (timestamped before the cutoff) gets renamed to `"deepseek-legacy"`
2. New model (e.g. `"deepseek-v4-pro"`) can then write its appraisal under `"deepseek"` without collision
3. No previous work is lost — the old appraisal is preserved under its new key

The `before` timestamp ensures that if a newer model reuses the same key, its appraisals won't be accidentally renamed. Only appraisals created before the cutoff are affected.

Safety: won't overwrite if the target key already has an appraisal.

### Schema version 3

- Bumped `callNumber` schema from v2 to v3
- `ProviderAppraisal` gains a `renamedFrom` field (`Maybe String`) — records the original key when an appraisal was renamed, so the provenance is always traceable
- Shown in the auto-generated Zotero note as "Renamed from: deepseek"
- v2 → v3 migration sets `renamedFrom = Nothing` on all existing appraisals
- `renamedFrom` is only written to JSON when present (no bloat for non-renamed appraisals)

### Auto-generated note deduplication


- `handleNotes` now finds ALL auto-generated notes (not just the first), patches one, and deletes the rest
- Detection uses both content markers ("auto-generated from structured data", "Inclusion/Exclusion reasoning") and the new `"auto-generated"` tag
- All auto-generated notes (new and patched) now get an `"auto-generated"` tag for reliable future detection
- `deleteNote` sends `DELETE` with `If-Unmodified-Since-Version` for safe concurrent access
- `ZoteroNote` type now includes `tags` for tag-based detection
- Batch note handling uses library version from `Last-Modified-Version` response header for batch deletes

### Enabled-only analysis and collections

Analysis, star tags, and model collections now only consider **enabled** models. Disabled/legacy model appraisals are preserved in the data but don't influence decisions or placement.

- `Analysis.compute` receives only enabled appraisals — disabled models don't affect star totals, inclusion/exclusion counts, or auto-include/auto-exclude categorization
- `modelsForArticle` applies renames and migration before checking existing appraisals — correctly identifies when a renamed model key needs re-screening
- Multiple `rename_from` entries targeting the same source key are sorted by `before` timestamp (oldest-first) to ensure each rename matches the correct appraisal

### Collection management overhaul

All managed collections (model included/excluded, analysis, version, star sum) are now stripped and rebuilt from source of truth on every patch. No more stale collection memberships.

- `allManagedCollectionKeys` set built at startup from all models (enabled + disabled) plus analysis and version collections
- Both classification and migration paths strip all managed collections, then re-add only what's current
- Collections only created for enabled models — disabled model collections are left as-is but items are removed from them
- Per-model collections rebuilt from all enabled appraisals (not just current run's results)

### Context-aware star tags

Star tags now reflect the analysis category and only consider enabled models:

- **Auto-included** (unanimous agreement): minimum stars — shows the weakest endorsement
- **Auto-excluded** (unanimous rejection): maximum stars — shows the strongest objection
- **Human review** (mixed signals): mean stars rounded to nearest integer
- Recalculated on every patch (classification and migration), so changing models always produces correct tags
- No star tag if analysis hasn't been computed yet


## v0.5.0 — The "take a random handful and look closely" release

### New `--random-sample` flag

Create random, non-overlapping sample collections straight from the CLI:

```bash
# Draw 100 random articles from the entire library
elm-pages run src/ClassifyArticles.elm --random-sample 100

# Draw 50 from a specific collection (use Zotero collection key)
elm-pages run src/ClassifyArticles.elm --random-sample 50 --from ABC12345
```

This creates a Zotero collection called "Random 100" (or "Random 50") with randomly selected articles. Run it again and you get "Random 100 (2)" — no overlap guaranteed.

**How it works:**
- With `--from`: draws only from items in that specific Zotero collection
- Without `--from`: draws from all top-level items in the library (excluding notes), fetched via Zotero's `format=versions` endpoint — one request, no pagination needed
- Finds all existing collections with "Random" or "random" in the name (e.g. "Random 100 (QC) Rayyan")
- Fetches keys from each of those collections and builds an exclusion set
- Shuffles the remaining pool using `Random.step` with a time-based seed (pure shuffle, no `Cmd` needed — take that, Elm Architecture!)
- Creates a new collection and batch-adds the selected articles

**Details:**
- `elm/random` promoted from indirect to direct dependency
- Auto-numbers collection names when duplicates exist: "Random 100" → "Random 100 (2)" → "Random 100 (3)"
- `--from` validates the collection key exists before proceeding
- Cannot be combined with `--models`, `--reprocess`, or `--migrate`; `--from` is only valid with `--random-sample`
- Items fetched and updated in chunks of 50 (Zotero's batch limit)
- Only needs Zotero credentials — no AI API keys required

### Bug fix: trashed collections no longer haunt the living

Collections in Zotero's trash were being treated as if they still existed. The collection decoder now parses the `deleted` field (handles both `bool` and `int` representations from the Zotero API), and `fetchAllCollections` filters out trashed collections. This affects all collection lookups — duplicate detection, source/model collection resolution, Random collection exclusion, and name availability checks.

## v0.4.3 - No more hidden surprises

### Cache hit logging

API responses now log cache usage per model, so you can tell if you're actually saving money or just sending vibes:
- **Anthropic**: logs cache hits (tokens read) and misses (tokens written to cache) from the explicit `cache_control` system
- **OpenAI-compatible** (Gemini, DeepSeek): logs `cached_tokens` from `prompt_tokens_details` when present — Gemini's implicit caching and DeepSeek's automatic prefix cache both report through this field

Example log output:
```
  💾 [claude] cache hit: 4521 tokens read from cache
  💾 [gemini] cache hit: 4096/4832 prompt tokens cached
```

## v0.4.2 — Stats that actually tell the truth

### Two-tier stats reporting

Previously, stats used only the first successful model's decision — if Model A included, Model B refused, and Model C errored, you'd just see "included: 1". The error and refusal were invisible. Not ideal when you're trying to figure out why your screening run looked weird.

Stats now track two levels:

**Article-level** — what happened to your articles:
- `processed`: total articles attempted
- `completed`: all models succeeded, article removed from source
- `partial`: some models failed, article kept in source for retry
- `failed`: all models failed, article skipped entirely

**Model-level** — how did the models actually perform:
- `include/exclude/refusals/errors`: counts every individual model call outcome

New summary format:
```
Articles:  25 processed (20 completed, 3 partial, 2 failed)
Decisions: 55 include, 12 exclude, 3 refusals, 5 errors
```

With a single model, article and decision counts naturally align — so it's not noisier unless you're running multiple models.

Also fixed: `recordErrorAndCheck` (circuit breaker) no longer sneakily increments the error count — callers now handle error counting explicitly, which avoids double-counting when model errors are already tracked by `updateStatsFromResults`.


## v0.4.1 — Escape from the never-ending migration loop

### Bug fix

- added /top to the article fetch query for the migration path, to prevent looping through child item attachments.


## v0.4.0 — The Triage Tribunal

The one where three AI models stop just filing opinions and start actually reaching a verdict. Every article now gets a score, a category, and a collection — no more squinting at individual appraisals wondering what it all means.

### Decision Analysis (Inline)

- Automatic decision analysis computed whenever all enabled models have appraised an article — no manual step needed
- New `Analysis.elm` module handles all scoring logic
- Aggregate scores stored in callNumber JSON:
  - `totalStars`: sum of relevance across all models (range 3–15)
  - `inclusions`: count of models that voted INCLUDE (relevance ≥ 3)
  - `exclusions`: count of models that voted EXCLUDE
  - `category`: one of `auto-excluded`, `human-review`, or `auto-included`

### Category Rules

- **Auto-excluded** (≤5 stars AND 3 exclusions): unanimous rejection — tagged ❌, moved to "AI auto-excluded" collection
- **Human review** (everything else): needs a human eye — sorted into "Sum of N stars" collections (one per star total, 5–15)
  - Tagged by exclusion count: ⭕⭕⭕ (3), ⭕⭕ (2), ⭕ (1)
  - Covers the edge case of 5 stars with < 3 exclusions (the lone dissenter)
- **Auto-included** (≥9 stars AND 0 exclusions): strong consensus — tagged ✅, moved to "AI auto-included" collection

### Collections

- Two new fixed collections created at startup: "AI auto-included" and "AI auto-excluded"
- Eight "Sum of N stars" collections (5 through 12) created at startup for human review triage — 13+ stars is impossible with any exclusions (max with 1 exclusion = 5+5+2 = 12)
- All new collections checked for duplicate names alongside existing model collections
- Per-model included/excluded collections continue to work as before

### Schema v2

- `AppraisalData` gains an `analysis : Maybe AnalysisData` field
- Schema version bumped from 1 to 2
- v1 → v2 migration sets `analysis = Nothing` (analysis computed on next full classification)
- Backwards-compatible: `analysis` field is optional in JSON decode

### Tags

- New analysis emoji tags: ✅, ❌, ⭕, ⭕⭕, ⭕⭕⭕
- Tags cleaned (stripped and re-added) alongside existing star tags and `death_after_therapy`
- Existing minimum-star-rating tag behaviour unchanged

### Auto-generated Notes

- Decision analysis summary section added to the HTML note (category, tag, star total, inclusion/exclusion counts)
- Appears between the disclaimer and per-model appraisal sections

### Bug Fixes

- Source collection fetch now uses `/items/top` instead of `/items` — PDF attachments and other child items no longer get sent to AI models for classification (they were just confusing the poor things)
- Collection fetch now paginates through all results (100 per page) instead of silently truncating — libraries with more than 100 collections no longer get duplicate collections created on every run
- Pre-classified articles (already screened by all models) now trigger analysis computation when encountered in the source collection — no more silent removal without scoring
- Migration mode (`--migrate`) now computes analysis for articles that have all enabled model appraisals, adds analysis tags and sorts into analysis collections

### Tests

- New `AnalysisTest.elm` with 20 tests covering:
  - All category boundary conditions (auto-excluded, human review, auto-included)
  - The 5-star edge case (≤5 stars with <3 exclusions → human review)
  - Tag generation for every exclusion count
  - Collection name generation
  - `isAnalysisTag` recognition
  - JSON encode/decode roundtrip for all categories

## v0.3.0 — The Model Parliament

The one where we stopped assuming Claude is the only game in town and let multiple AI models screen articles side by side. Democracy in action, or at least a vigorous peer review.

### Collection-Based Screening Workflow
- New source collection model: a human fills a Zotero collection (e.g. "Screening queue") with articles to screen
- Articles removed from source collection only when ALL selected models succeed — partial failures stay queued for retry
- No more processed tags — the appraisals dict in `callNumber` is the sole source of truth
- Per-model included/excluded collections (e.g. "Claude included", "Deepseek excluded") created automatically
- Duplicate collection name detection: fatal error if any needed collection name appears more than once in your library

### Runtime Model Selection
- `--models claude,deepseek` CLI flag selects which models to run (default: all enabled models in `config.json`)
- `--reprocess claude` forces re-run of specified models, overwriting existing appraisals
- `--models` and `--reprocess` cannot be combined — the script will politely explain why
- `--max` can be combined with any flag; `--migrate` is its own mode and cannot combine with `--models` or `--reprocess`

### Multi-Model Screening
- New `config.json` config file defines AI models, each with a key, API format, model name, base URL, and API key env var name
- Supported API formats: `anthropic` (Claude) and `openai` (OpenAI-compatible — DeepSeek, etc.)
- Multiple models run in parallel per article using `BackendTask.andMap` — no waiting in line
- Each model stores its appraisal under its own key in the `callNumber` JSON (e.g. `"claude"`, `"deepseek"`)
- Per-model `enabled` field in `config.json` — disable a model without removing it from config

### Star Tag Logic
- Star rating = minimum across all existing + new appraisals — the most conservative model wins
- If Claude gives 5 stars but DeepSeek gives 1 star, the article gets 1 star (trust the sceptic)
- `death_after_therapy` tag added if ANY model flags it, removed only when none do

### New Module: OpenAiApi.elm
- Encoder/decoder for OpenAI-compatible chat completions API
- Handles system/user message format, finish_reason parsing, and token usage
- Used for DeepSeek and any future OpenAI-compatible providers

### Configuration Overhaul
- `config.json` replaces `ANTHROPIC_API_KEY` and `ANTHROPIC_MODEL` env vars for model config
- New `sourceCollection` field in `config.json` names the collection to fetch articles from
- Zotero library ID now lives in `config.json` (not a secret, just user-specific)
- All model fields are required (no optional fields): `key`, `apiFormat`, `model`, `apiKeyEnvVar`, `baseUrl`, `enabled`, `maxTokens`
- `maxTokens` per model — no more hardcoded token limits;
- API keys still resolved from env vars (names configured per model in `config.json`)
- `ZOTERO_API_KEY` still from `.env` / `secrets.txt` as before
- Fallback key file renamed from `config.txt` to `secrets.txt` — because that's what it contains
- Template provided: `config.json.template`

### Housekeeping
- Articles already fully screened are quietly cleaned out of the source collection during processing — no wasted API calls
- Partial success handling: if one model fails but another succeeds, successful results are written and the article stays in the source collection for retry
- Classification logging now shows per-model results with `[model]` prefixes
- Removed all processed-tag logic (`modelProcessedTag`, `allProcessedTags`, tag-based fetching)
- 145 tests passing, zero elm-review errors

## v0.2.0 — The Great Migration

The one where we stopped trusting HTML notes and started putting real data in real fields.

### Structured Appraisal Data
- Classification results now stored as structured JSON in Zotero's `callNumber` field — the source of truth has left the HTML building
- Multi-provider support: appraisals keyed by provider name (e.g. "claude", "deepseek"), ready for the day we let multiple AIs argue about immunology papers
- ASReview data slot for human-in-the-loop integration
- Schema versioning (currently v1) with forward migration support

### Migration Mode
- New `--migrate` flag for upgrading legacy articles to structured format without making AI calls
- `--migrate 0` targets articles in the `version_0` collection; `--migrate N` targets articles in version N collections
- Batch writes via Zotero's multi-object API — up to 50 items per request instead of one-by-one
- Existing notes preserved for comparison — old notes are never modified or deleted
- Hardened Todo extraction from legacy HTML: multi-paragraph, inline, and div-wrapped variants now handled correctly

### Version Collection Tracking
- Articles tracked in Zotero sub-collections (`data_schema_migrations/version_N`) for efficient future upgrades
- Classification mode automatically manages version collections alongside relevant/irrelevant sorting

### External Prompt
- System prompt moved from hardcoded Elm string to external `prompt.txt` file
- Loaded at runtime via `BackendTask.File` — iterate on prompts without recompiling
- Clear error message if prompt file is missing

### Configuration
- Environment variables now fall back to `config.txt` if not set in the shell — same `KEY=VALUE` format as `.env_template`
- Helpful error messages listing exactly which variables are missing and where to set them

### Housekeeping
- Notes are never deleted or overwritten — only auto-generated notes are updated, user notes are untouched
- Fixed elm-review false positive on elm-pages entry module export
- Removed unused `Config` parameter from `parseClassificationResponse`
- Added `parentCollection` support to `ZoteroCollection` type and decoder (handles Zotero's quirky `false` vs string response)
- Added `encodeCreateSubCollection` and batch write encoders for multi-object API
- Fixed legacy migration hardcoding `isRefusal = False` instead of checking the migrated content
- 145 tests passing, zero elm-review errors

## v0.1.1 — The Paperwork Release

The code didn't change, but the documentation got a makeover worthy of a journal resubmission.

- Added user-facing README with setup and usage instructions for non-developers (no Elm knowledge required, just Node.js and a dream)
- Moved developer docs to DEVELOPMENT.md — project structure, devbox setup, and all npm commands in one place
- Slimmed CLAUDE.md down to house rules only; it now points to DEVELOPMENT.md for the rest
- Added CHANGELOG.md — you're reading it, well done
- Added MIT License — because sharing is caring
- Bumped version to 0.1.1
- Built `ai-zotero-classifier_0.1.1.mjs`

## v0.1.0 — Initial Release

The one where an Elm script classifies PubMed articles with Claude's help so you don't have to read 10,000 abstracts on a Friday night.

- Elm-pages v3 script that fetches unprocessed articles from Zotero, sends them to Claude for classification, and sorts them into `IEI_Relevant` / `IEI_Irrelevant` collections
- Built-in classification prompt for fatal infections in patients with Inborn Errors of Immunity (IEI), including a reference list of several hundred gene loci and disease names
- 5-star relevance scoring: 3+ stars = included, below = excluded
- Automatic tagging (`CLAUDE`) so the script knows what it has already processed — safe to stop and restart
- Reviewer notes attached to included articles explaining what to check in full text
- Circuit breaker logic to bail out if too many API errors pile up
- Batch processing with `--max` flag or interactive prompt
- Bundled production build (`build/ai-zotero-classifier_0.1.0.mjs`) — just needs Node.js 20+
- Test suite covering classification logic, Anthropic API codecs, Zotero API codecs, and statistics
- elm-review config with NoUnused, NoDebug, and Simplify rules
