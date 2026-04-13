# Development

Automated classification of PubMed articles for IEI (Inborn Errors of Immunity) research using Claude AI and the Zotero API. Written in Elm as an [elm-pages v3](https://elm-pages.com) script (3.3.3).

## Project Structure

```
src/ClassifyArticles.elm          Main script
src/libs/
  Classification.elm              Relevance types, decision logic, system prompt, JSON decoders
  ZoteroApi.elm                   Zotero item/collection types, JSON codecs, article extraction
  AnthropicApi.elm                Anthropic Messages API types, request encoder, response decoder
  Stats.elm                       Batch statistics and circuit breaker logic
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
```

Fill in `.env` (loaded automatically by devbox shell):
- `ZOTERO_LIBRARY_ID` — your library/group id from https://www.zotero.org/mylibrary
- `ZOTERO_API_KEY` — create at https://www.zotero.org/settings/keys/new (read/write + notes access)
- `ANTHROPIC_API_KEY` — from https://console.anthropic.com/settings/keys
- `ANTHROPIC_MODEL` — e.g. `claude-sonnet-4-6`

## Commands

```bash
npm run classify                    # run classifier (prompted for batch size)
npm run classify -- --max 50        # process 50 articles
npm run classify:all                # process all unprocessed articles
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
