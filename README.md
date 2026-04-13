# AI Zotero Classifier

Automated classification of PubMed articles for IEI (Inborn Errors of Immunity) research using Claude AI and the Zotero API. Written in Elm as an [elm-pages v3](https://elm-pages.com) script.

## Setup

### 1. Install Devbox

```bash
curl -fsSL https://get.jetify.com/devbox | bash
devbox shell
npm install
```

### 2. Configure Environment

```bash
cp .env_template .env
```

Fill in `.env`:
- `ZOTERO_LIBRARY_ID` — your library/group id from https://www.zotero.org/mylibrary
- `ZOTERO_API_KEY` — create at https://www.zotero.org/settings/keys/new (read/write + notes access)
- `ANTHROPIC_API_KEY` — from https://console.anthropic.com/settings/keys
- `ANTHROPIC_MODEL` — e.g. `claude-sonnet-4-6`

## Usage

```bash
npm run classify              # process articles (max number will be asked in prompt)
npm run classify -- --max 50  # process 50 articles
npm run classify:all          # process all unprocessed articles
```

## How It Works

1. Fetches unprocessed articles from Zotero (those without the `CLAUDE` tag)
2. Classifies each article using Claude AI against IEI inclusion/exclusion criteria
3. Moves articles into `IEI_Relevant` or `IEI_Irrelevant` collections
4. Tags articles as processed and attaches classification notes

Progress is saved per-article, so you can stop and resume anytime.

## Development

```bash
npm run test        # run tests (elm-test-rs)
npm run review      # run elm-review
npm run review:fix  # auto-fix elm-review errors
npm run format      # auto-format with elm-format
npm run build       # bundle optimized script to ./build/
```

## Links

- [Zotero API docs](https://www.zotero.org/support/dev/web_api/v3/start)
- [Anthropic API docs](https://docs.anthropic.com/)
