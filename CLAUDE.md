# CLAUDE.md

## Rules
- Never read or write the Elm package cache (~/.elm)
- Read DEVELOPMENT.md for project structure, setup, and available commands
- Log all code changes to CHANGELOG.md in significant detail.
- Add some humour where appropriate, but no sarcasm please!

## Configuration
- API keys live in `.env` (loaded by devbox shell) or `secrets.txt`. Required: `ZOTERO_API_KEY`, plus the per-model keys named in `config.json` (e.g. `ANTHROPIC_API_KEY`, `DEEPSEEK_API_KEY`). See `.env_template` for format.
- Library ID, source collection, and AI model configuration live in `config.json`. See `config.json.template`.
