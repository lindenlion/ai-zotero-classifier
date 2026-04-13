# AI Zotero Classifier

An automated tool that screens PubMed articles in your Zotero library using Claude AI. It reads article titles and abstracts, classifies them according to built-in research criteria, and sorts them into Zotero collections -- so you don't have to review thousands of abstracts by hand.

## Requirements

- [Node.js](https://nodejs.org/) version 20 or newer
- A [Zotero](https://www.zotero.org/) account with a group library containing PubMed articles
- A Zotero API key (see below)
- An [Anthropic API key](https://console.anthropic.com/settings/keys) for Claude

## Getting Started

### 1. Download the script

Download `ai-zotero-classifier_0.1.1.mjs` from the `build/` folder (or from the [Releases](https://github.com/lindenlion/ai-zotero-classifier/releases) page).

### 2. Set environment variables

The script needs four environment variables. Set them in your terminal before running:

```bash
export ZOTERO_LIBRARY_ID="your_library_id"
export ZOTERO_API_KEY="your_zotero_api_key"
export ANTHROPIC_API_KEY="your_anthropic_api_key"
export ANTHROPIC_MODEL="claude-sonnet-4-6"
```

- **ZOTERO_LIBRARY_ID** -- Your Zotero user or group library ID. Find it at https://www.zotero.org/mylibrary (the first number in the url).
- **ZOTERO_API_KEY** -- Create a new key at https://www.zotero.org/settings/keys/new. Grant it "Allow library access" with read/write permissions and "Allow notes access".
- **ANTHROPIC_API_KEY** -- Create one at https://console.anthropic.com/settings/keys. You need API credits on your Anthropic account.
- **ANTHROPIC_MODEL** -- The Claude model to use (e.g. `claude-sonnet-4-6` or `claude-opus-4-6`).

### 3. Run the script

```bash
# Process articles (you will be prompted for how many)
node build/ai-zotero-classifier_0.1.0.mjs

# Process a specific number of articles
node build/ai-zotero-classifier_0.1.0.mjs --max 50

# Process all unprocessed articles
node build/ai-zotero-classifier_0.1.0.mjs --max 0
```

The script is safe to stop and restart. It tags each processed article with `CLAUDE`, so it will automatically skip articles it has already classified.

## What the Script Does

For each unprocessed article in your Zotero library, the script:

1. Sends the title and abstract to Claude for classification.
2. Based on the response, moves the article into either an **IEI_Relevant** or **IEI_Irrelevant** collection (created automatically if they don't exist).
3. Tags the article as processed.
4. For included articles, attaches a note explaining what a human reviewer should check in the full text.

## Classification Criteria

This version has a classification prompt built into the script, designed for a specific research domain: **fatal infections in patients with Inborn Errors of Immunity (IEI)**.

An article is classified as **relevant** if the abstract suggests:

- At least one human has died (at any age)
- The death is related to an infection (bacterial, viral, fungal, parasitic, or indirect such as EBV-triggered neoplasms)
- The patient has an inborn error of immunity per the IUIS 2024 classification (excluding cystic fibrosis and G6PD deficiency)

An article is classified as **irrelevant** if:

- The immune deficiency is secondary (e.g. HIV/AIDS)
- Death occurred after stem cell transplantation (HSCT) or therapeutic immunosuppression
- Death occurred before birth
- The cause of death is clearly unrelated to infection

When in doubt, the classifier errs on the side of inclusion and flags uncertainty for human review.

The prompt also includes a reference list of several hundred IEI-related gene loci and disease names to help Claude recognise relevant conditions.

Each article receives a relevance score from 1 to 5 stars. Articles scoring 3 stars or higher are included; those below are excluded.

## API Costs

Claude API usage is billed per token. Costs depend on the model chosen and the length of the abstracts. As a rough guide, classifying around 10,000 articles with Claude Sonnet costs in the range of $50-150. Smaller or cheaper models reduce this.

## Future Plans

Future versions will decouple the classification prompt from the source code, allowing you to supply your own criteria via an input file. This will make the tool usable for any systematic review, not just IEI research.

## For Developers

See [DEVELOPMENT.md](DEVELOPMENT.md) for build instructions, test commands, and project structure.
