# PubMed Article Classifier for IEI Research

Automated classification of PubMed articles using Claude AI via Zotero API.

## Setup

### 1. Install Devbox

This project uses [Devbox](https://www.jetify.com/devbox) to manage the development environment (Python, Claude Code).

```bash
# Install devbox
curl -fsSL https://get.jetify.com/devbox | bash

# Enter the devbox shell (installs packages, loads .env, activates venv automatically)
devbox shell
```

### 2. Set Environment Variables

Copy `.env_template` to `.env` and fill in your credentials:

```bash
cp .env_template .env
```

Edit `.env` with your values:

```
ZOTERO_LIBRARY_ID=your_library_id
ZOTERO_API_KEY=your_zotero_api_key
ANTHROPIC_API_KEY=your_anthropic_api_key
ANTHROPIC_MODEL=claude-opus-4-6
```

The devbox shell automatically exports these variables on startup.

**Getting your Zotero credentials:**
- Library ID: Go to https://www.zotero.org/settings/keys - your userID is your library ID
- API Key: Create a new key at https://www.zotero.org/settings/keys/new
  - Give it a descriptive name (e.g., "PubMed Classifier")
  - Personal Library: Check "Allow library access" with "Read/Write"
  - Check "Allow notes access"

**Getting your Anthropic API key:**
- Go to https://console.anthropic.com/settings/keys
- Create a new API key

### 3. Install Python Dependencies

```bash
pip install -r requirements.txt
```

### 4. Create Zotero Collections

In your Zotero library, create two collections (or the script will create them):
- `Claude included` - for included articles
- `Claude excluded` - for excluded articles

You can customize these names in the script by editing the constants at the top.

## Usage

### Basic Usage (Process 100 articles)

```bash
python pubmed_classifier.py
```

When prompted, enter batch size (default 100) or 0 to process all.

### Process All Articles

The script will automatically process all unprocessed articles in batches:

```bash
python pubmed_classifier.py
# Enter 0 when prompted for batch size
```

### Process Specific Batch Size

```bash
python pubmed_classifier.py
# Enter desired number (e.g., 50, 200)
```

## How It Works

1. **Fetches unprocessed articles** from Zotero (those without the `CLAUDE` tag)
2. **Classifies each article** using Claude AI based on your inclusion/exclusion criteria
3. **Updates Zotero**:
   - Adds article to `IEI_Relevant` or `IEI_Irrelevant` collection
   - Adds `CLAUDE` tag to mark as processed
   - Appends a note with reasoning and reviewer guidance (for INCLUDE decisions)

## Classification Criteria

**INCLUDE if:**
- At least one human died (any age)
- Death related to infection (bacteria, virus, fungi, parasites)
- Patient has inborn error of immunity (IUIS 2024 classification)
- Excludes: cystic fibrosis, G6PD deficiency

**EXCLUDE if:**
- Secondary immune deficiency (e.g., AIDS)
- Death after HSCT or therapeutic immunosuppression
- Death before birth
- Death clearly unrelated to infection
- Genetic characterization study with no outcome data

## Customization

Edit the script to customize:
- Collection names (lines 18-19)
- Tag name (line 20)
- Classification prompt (lines 22-71)
- Batch size defaults
- API rate limiting delays

## Rate Limits & Costs

**Claude API:**
- Opus 4.6: ~$15 per million input tokens, ~$75 per million output tokens
- Estimated cost for 11,550 articles: ~$200-500 depending on abstract length
- Script includes 1-second delays between requests

**Zotero API:**
- Free tier: 10 requests/second
- Script respects rate limits

## Progress Tracking

The script prints:
- Current article being processed
- Classification decision and reasoning
- Batch summaries
- Final totals

Progress is saved after each article, so you can stop and resume anytime.

## Resuming Interrupted Processing

Simply run the script again - it automatically skips articles with the `CLAUDE` tag.

## Troubleshooting

**"Missing required environment variables"**
- Make sure all three environment variables are set

**"Collection not found"**
- Collections will be auto-created on first run
- Or manually create them in Zotero

**API errors**
- Check your API keys are valid
- Ensure you have API credits (Anthropic)
- Check internet connection

**Rate limiting**
- Script includes delays, but you can increase them if needed
- Edit the `time.sleep()` values in the script

## Monitoring Progress

In Zotero, you can:
- View collections to see classified articles
- Search for `CLAUDE` tag to see all processed articles
- Read attached notes to see classification reasoning

## Example Output

```
🔬 PubMed Article Classifier for IEI Research
============================================================
✓ Connected to Zotero library: 12345678
✓ Relevant collection: IEI_Relevant (ABC123XYZ)
✓ Irrelevant collection: IEI_Irrelevant (DEF456UVW)

Enter batch size (default 100, 0 for all): 50

📚 Processing 50 articles...

[1/50] Comprehensive analyses and characterization of haemophag...
  → INCLUDE: Nine patients with familial HLH, unclear if deaths occurred...

[2/50] UNC13D is the predominant causative gene with recurrent...
  → EXCLUDE: Genetic study, no outcome data mentioned in abstract...

📊 Summary:
   Processed: 50
   Included: 12
   Excluded: 38
   Errors: 0
```

## Advanced: Custom Classification Logic

To modify the classification logic:
1. Edit the `CLASSIFICATION_PROMPT` variable
2. Adjust the JSON response format if needed
3. Update the `classify_article()` method

## Support

For issues with:
- **Zotero API**: https://www.zotero.org/support/dev/web_api/v3/start
- **Anthropic API**: https://docs.anthropic.com/
- **This script**: Check the error messages and troubleshooting section above
