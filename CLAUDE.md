# CLAUDE.md

## Rules
- Never read or write the Elm package cache (~/.elm)
- NPM is available via devbox shell: nodejs@20 or newer

## Project Structure
- elm-pages v3 script project (3.3.3)
- Main script: `src/ClassifyArticles.elm`
- Shared pure modules (types, decoders, encoders, business logic): `src/libs/`
  - `Classification.elm` — relevance types, decision logic, system prompt, JSON decoders
  - `ZoteroApi.elm` — Zotero item/collection types, JSON codecs, article extraction
  - `AnthropicApi.elm` — Anthropic Messages API types, request encoder, response decoder
  - `Stats.elm` — batch statistics and circuit breaker logic
- Tests: `tests/`
- elm-review config: `review/` (NoUnused, NoDebug, Simplify rules)

## Development shell
```bash
devbox shell                        # start development shell
> npm install # only needed once
```
This ensures that all development related tools are installed in the compatible version for this project.


### Dev commands
```bash
npm run classify                    # run classifier (default 100 articles)
npm run classify -- --max 50        # process 50 articles, skip prompt
npm run classify:all                # process all articles, skip prompt
npm run test                        # run all tests (elm-test-rs)
npm run review                      # run elm-review
npm run review:fix                  # auto-fix elm-review errors
npm run review:deps                 # check for unused Elm dependencies
npm run format                      # auto-format src/ and tests/ with elm-format
npm run build                       # bundle optimized script to ./build/
```

## Required Environment Variables
Set in `.env` (loaded by devbox shell): `ZOTERO_LIBRARY_ID`, `ZOTERO_API_KEY`, `ANTHROPIC_API_KEY`, `ANTHROPIC_MODEL`. See `.env_template` for format.
