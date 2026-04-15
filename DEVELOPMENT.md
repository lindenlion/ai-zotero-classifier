# Development

Automated classification of PubMed articles for IEI (Inborn Errors of Immunity) research using Claude AI and the Zotero API. Written in Elm as an [elm-pages v3](https://elm-pages.com) script (3.3.3).

## Project Structure

```
src/ClassifyArticles.elm          Main script (CLI, API orchestration, migration)
src/libs/
  Appraisal.elm                   Structured appraisal schema, encode/decode, note generation
  Classification.elm              Relevance types, decision logic, JSON decoders
  ZoteroApi.elm                   Zotero item/collection types, JSON codecs, article extraction
  AnthropicApi.elm                Anthropic Messages API types, request encoder, response decoder
  OpenAiApi.elm                   OpenAI-compatible chat API encoder/decoder (DeepSeek, etc.)
  Stats.elm                       Batch statistics and circuit breaker logic
config.json                       Model configuration: which AI models to use (from template)
prompt.txt                        System prompt for article classification (loaded at runtime)
tests/                            Tests (elm-test-rs)
review/                           elm-review config (NoUnused, NoDebug, Simplify rules)
build/                            Bundled production scripts
```

## Setup

### 1. Install Devbox

```bash
curl -fsSL https://get.jetify.com/devbox | bash
devbox shell
npm install   # only needed once
```

### 2. Configure Environment

```bash
cp .env_template .env
cp config.json.template config.json
```

Fill in `.env` with API keys (loaded automatically by devbox shell, or save as `secrets.txt`):
- `ZOTERO_API_KEY` — create at https://www.zotero.org/settings/keys/new (read/write + notes access)
- `ANTHROPIC_API_KEY` — from https://console.anthropic.com/settings/keys
- `DEEPSEEK_API_KEY` — from https://platform.deepseek.com/api_keys

Fill in `config.json` with model configuration:
- `zoteroLibraryId` — your library/group id from https://www.zotero.org/mylibrary
- `models` — array of AI models to use for screening. Each model needs:
  - `key` — identifier used in appraisals and as processed tag (e.g. "claude", "deepseek")
  - `apiFormat` — `"anthropic"` or `"openai"` (OpenAI-compatible, e.g. DeepSeek)
  - `model` — exact model name sent in API calls (e.g. "claude-opus-4-6", "deepseek-reasoner")
  - `apiKeyEnvVar` — name of the env var holding the API key (e.g. "ANTHROPIC_API_KEY")
  - `baseUrl` — API base URL (e.g. "https://api.anthropic.com", "https://api.deepseek.com")

## Commands

```bash
npm run classify                    # run classifier (prompted for batch size)
npm run classify -- --max 50        # process 50 articles
npm run classify:all                # process all unprocessed articles
npm run classify -- --migrate       # migrate all legacy articles (no AI calls)
npm run classify -- --migrate 0     # migrate from version 0 (legacy)
npm run test                        # run all tests (elm-test-rs)
npm run review                      # run elm-review
npm run review:fix                  # auto-fix elm-review errors
npm run review:deps                 # check for unused Elm dependencies
npm run format                      # auto-format src/ and tests/ with elm-format
npm run build                       # bundle optimized script to ./build/
```

## Links

- [Zotero API docs](https://www.zotero.org/support/dev/web_api/v3/start)
- [Anthropic API docs](https://docs.anthropic.com/)
