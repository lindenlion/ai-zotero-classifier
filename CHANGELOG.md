# Changelog

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
