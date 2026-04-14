# AI Zotero Classifier

An automated tool that screens PubMed articles in your Zotero library using Claude AI. It reads article titles and abstracts, classifies them according to your research criteria defined in `prompt.txt`, and sorts them into Zotero collections -- so you don't have to review thousands of abstracts by hand.

## Requirements

- [Node.js](https://nodejs.org/) version 20 or newer
- A [Zotero](https://www.zotero.org/) account with a group library containing PubMed articles
- A Zotero API key (see below)
- An [Anthropic API key](https://console.anthropic.com/settings/keys) for Claude

## Getting Started

### 1. Download the script

Download `ai-zotero-classifier_0.1.1.mjs` from the `build/` folder (or from the [Releases](https://github.com/lindenlion/ai-zotero-classifier/releases) page).

### 2. Configure

The script needs four variables. You can set them as environment variables or put them in a `config.txt` file (same format as `.env_template`):

```
ZOTERO_LIBRARY_ID=your_library_id
ZOTERO_API_KEY=your_zotero_api_key
ANTHROPIC_API_KEY=your_anthropic_api_key
ANTHROPIC_MODEL=claude-sonnet-4-6
```

Environment variables take precedence over `config.txt`. The script checks both and tells you exactly which variables are missing.

- **ZOTERO_LIBRARY_ID** -- Your Zotero user or group library ID. Find it at https://www.zotero.org/mylibrary (the first number in the url).
- **ZOTERO_API_KEY** -- Create a new key at https://www.zotero.org/settings/keys/new. Grant it "Allow library access" with read/write permissions and "Allow notes access".
- **ANTHROPIC_API_KEY** -- Create one at https://console.anthropic.com/settings/keys. You need API credits on your Anthropic account.
- **ANTHROPIC_MODEL** -- The Claude model to use (e.g. `claude-sonnet-4-6` or `claude-opus-4-6`).

### 3. Create a prompt file

Create a `prompt.txt` file in your current working as the script. This file contains the system prompt that tells Claude how to classify articles. See the included `prompt.txt` for an example tailored to fatal infections in IEI patients, or write your own for your research domain.

### 4. Run the script

```bash
# Process articles (you will be asked how many articles to process at most)
node ai-zotero-classifier_0.2.0.mjs

# Process a specific number of articles
node ai-zotero-classifier_0.2.0.mjs --max=50

# Process all unprocessed articles
node ai-zotero-classifier_0.2.0.mjs --max=0

# Migrate legacy articles to structured format (no AI calls)
node ai-zotero-classifier_0.2.0.mjs --migrate
```

The script is safe to stop and restart. It tags each processed article with `CLAUDE`, so it will automatically skip articles it has already classified. The final count is only printed to the terminal if the script is not interrupted. 

## What the Script Does

For each unprocessed article in your Zotero library, the script:

1. Sends the title and abstract to Claude for classification.
2. Stores the structured appraisal (relevance, reasoning, reviewer note) as JSON in the article's `callNumber` field.
3. Moves the article into either a **Claude included** or **Claude excluded** collection (created automatically).
4. Tags the article as processed.
5. Attaches a human-readable note summarising the classification.

## Classification Criteria

Classification criteria are defined in `prompt.txt`. The included example prompt is designed for **fatal infections in patients with Inborn Errors of Immunity (IEI)**, but you can replace it with your own criteria for any systematic review.

The prompt should instruct Claude to respond in JSON with `relevance` (star rating), `reasoning`, `note` (reviewer instructions for full-text stage), and `death_after_therapy` fields. Each article receives a relevance score from 1 to 5 stars. Articles scoring 3 stars or higher are included; those below are excluded.

## API Costs

Claude API usage is billed per token. Costs depend on the model chosen and the length of the abstracts. As a rough guide, classifying around 10,000 articles with Claude Sonnet costs in the range of $50-150. More advanced or expensive models may increase this number.

## For Developers

See [DEVELOPMENT.md](DEVELOPMENT.md) for build instructions, test commands, and project structure.
