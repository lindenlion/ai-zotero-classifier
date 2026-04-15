# Changelog

## v0.3.0 — The Model Parliament

The one where we stopped assuming Claude is the only game in town and let multiple AI models screen articles side by side. Democracy in action, or at least a vigorous peer review.

### Multi-Model Screening
- New `config.json` config file defines AI models, each with a key, API format, model name, base URL, and API key env var name
- Supported API formats: `anthropic` (Claude) and `openai` (OpenAI-compatible — DeepSeek, etc.)
- Multiple models run in parallel per article using `BackendTask.andMap` — no waiting in line
- Each model stores its appraisal under its own key in the `callNumber` JSON (e.g. `"claude"`, `"deepseek"`)
- Per-model processed tags (e.g. `CLAUDE`, `DEEPSEEK`) — articles only screened by models that haven't seen them yet
- Per-model Zotero collections for include/exclude sorting (e.g. "Claude included", "Deepseek included")
- First model's decision drives star tags and primary collection placement

### New Module: OpenAiApi.elm
- Encoder/decoder for OpenAI-compatible chat completions API
- Handles system/user message format, finish_reason parsing, and token usage
- Used for DeepSeek and any future OpenAI-compatible providers

### Configuration Overhaul
- `config.json` replaces `ANTHROPIC_API_KEY` and `ANTHROPIC_MODEL` env vars for model config
- Zotero library ID now lives in `config.json` (not a secret, just user-specific)
- All model fields are required (no optional fields): `key`, `apiFormat`, `model`, `apiKeyEnvVar`, `baseUrl`
- API keys still resolved from env vars (names configured per model in `config.json`)
- `ZOTERO_API_KEY` still from `.env` / `secrets.txt` as before
- Fallback key file renamed from `config.txt` to `secrets.txt` — because that's what it contains
- Template provided: `config.json.template`

### Fetch Logic
- Fetches articles per-model (missing that model's tag), merges and deduplicates
- Per-article, only runs models whose processed tag is absent — efficient incremental screening
- Reprocess mode still works: runs all configured models regardless of existing tags

### Housekeeping
- Partial success handling: if one model fails but another succeeds, the item is still updated
- Classification logging now shows per-model results with `[model]` prefixes
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
