# AI Zotero Classifier

An automated tool that screens PubMed articles in your Zotero library using one or more AI models (Claude, DeepSeek, and other OpenAI-compatible providers). It reads article titles and abstracts, classifies them according to your research criteria defined in `prompt.txt`, and sorts them into Zotero collections — so you don't have to review thousands of abstracts by hand.

When several models are configured, every article is screened by each of them and the verdicts are combined into a single decision (auto-include, auto-exclude, or "needs a human eye").

## Requirements

- [Node.js](https://nodejs.org/) version 20 or newer
- A [Zotero](https://www.zotero.org/) account with a library containing the PubMed articles you want to screen
- A Zotero API key (see below)
- An API key for at least one supported AI provider (Anthropic, DeepSeek, or any OpenAI-compatible service)

## Getting Started

### 1. Download the script

Download the latest `ai-zotero-classifier_<version>.mjs` from the `build/` folder (or from the [Releases](https://github.com/lindenlion/ai-zotero-classifier/releases) page).

### 2. Set your API keys

API keys can be supplied as environment variables, or saved in a `secrets.txt` file (same `KEY=VALUE` format as `.env_template`) in the working directory. Environment variables take precedence.

```
ZOTERO_API_KEY=your_zotero_api_key
ANTHROPIC_API_KEY=your_anthropic_api_key
DEEPSEEK_API_KEY=your_deepseek_api_key
```

- **ZOTERO_API_KEY** — create a new key at https://www.zotero.org/settings/keys/new. Grant it "Allow library access" with read/write permissions and "Allow notes access".
- **ANTHROPIC_API_KEY** — create one at https://console.anthropic.com/settings/keys. You need API credits on your Anthropic account.
- **DEEPSEEK_API_KEY** — create one at https://platform.deepseek.com/api_keys. Only required if you enable a DeepSeek model.

The script tells you exactly which keys are missing and where to set them.

### 3. Configure models and library

Copy `config.json.template` to `config.json` and fill it in:

- **`zoteroLibraryId`** — your Zotero user or group library ID. Find it at https://www.zotero.org/mylibrary (the first number in the URL).
- **`sourceCollection`** — the name of the Zotero collection that holds the articles to screen (you populate this manually, e.g. "Screening queue").
- **`models`** — an array of AI models to use for screening. Each model needs:
  - `key` — short identifier used in appraisals and collection names (e.g. `"claude"`, `"deepseek"`)
  - `apiFormat` — `"anthropic"` or `"openai"` (the latter covers any OpenAI-compatible API, e.g. DeepSeek)
  - `model` — exact model name sent to the provider (e.g. `"claude-opus-4-6"`, `"deepseek-v4-pro"`)
  - `apiKeyEnvVar` — the name of the env var holding the API key (e.g. `"ANTHROPIC_API_KEY"`)
  - `baseUrl` — the API base URL
  - `enabled` — `true` to include in the default model set, `false` to leave out unless explicitly selected with `--models`
  - `maxTokens` — maximum output tokens per call

### 4. Write a prompt file

Create a `prompt.txt` file in the same directory as the script. This file is the system prompt that tells the AI how to classify articles. See the included `prompt.txt` for an example tailored to fatal infections in IEI patients, or write your own for your research domain.

The prompt should instruct the model to respond in JSON with `relevance` (1–5 star rating), `reasoning`, `note` (reviewer instructions for the full-text stage), and `death_after_therapy` fields. Articles scoring 3 stars or higher are treated as included; below that, excluded.

### 5. Run the script

```bash
# Process articles (you'll be prompted for a batch size)
node ai-zotero-classifier_0.6.0.mjs

# Process a specific number of articles
node ai-zotero-classifier_0.6.0.mjs --max 50

# Process all unscreened articles in the source collection
node ai-zotero-classifier_0.6.0.mjs --max 0

# Run only specific models (comma-separated)
node ai-zotero-classifier_0.6.0.mjs --models claude

# Force re-run a model, overwriting its existing appraisals
node ai-zotero-classifier_0.6.0.mjs --reprocess deepseek

# Migrate legacy articles to the current structured format (no AI calls)
node ai-zotero-classifier_0.6.0.mjs --migrate

# Create a "Random N" sample collection
node ai-zotero-classifier_0.6.0.mjs --random-sample 100
node ai-zotero-classifier_0.6.0.mjs --random-sample 50 --from COLLECTION_KEY
```

The script is safe to stop and restart. Each appraisal is stored on the article itself (in Zotero's `callNumber` field), and articles already screened by every enabled model are quietly skipped.

## What the Script Does

For each article in the source collection, the script:

1. Sends the title and abstract to every enabled AI model in parallel.
2. Stores each model's structured appraisal (relevance, reasoning, reviewer note) as JSON in the article's `callNumber` field, keyed by model.
3. Computes a combined decision once all enabled models have voted, and sorts the article into the appropriate collection:
   - **AI auto-included** — strong consensus to include (≥9 stars, no exclusions)
   - **AI auto-excluded** — unanimous rejection (≤5 stars, all models excluded)
   - **Sum of N stars** — everything in between, queued for human review and bucketed by total stars
4. Mirrors each model's vote in per-model collections (e.g. "Claude included", "Deepseek excluded").
5. Tags the article with star totals, exclusion-count emojis, and any flags raised by the prompt (e.g. `death_after_therapy`).
6. Attaches a single auto-generated note summarising the decision and every model's reasoning.

Articles are removed from the source collection only once every enabled model has succeeded — partial failures stay queued for retry.

## Classification Criteria

Classification criteria live entirely in `prompt.txt`. The included example prompt is designed for **fatal infections in patients with Inborn Errors of Immunity (IEI)**, but you can replace it with your own criteria for any systematic review.

## API Costs

AI API usage is billed per token, so cost depends on the models chosen and the length of the abstracts. As a rough guide, screening around 10,000 articles with Claude Sonnet costs in the range of $50–150. More advanced models (or running several models per article) will increase that figure proportionally.

## For Developers

See [DEVELOPMENT.md](DEVELOPMENT.md) for build instructions, test commands, and project structure.
