#!/usr/bin/env bash
# PostToolUse hook (Edit, Write): lint the R file just changed, so a lint
# is fixed while the change is fresh rather than at the pre-PR check.
# It runs only when TFL_HOOK_RSCRIPT names the Rscript to use -- the one
# whose library is your work library (see the article on developing with AI
# agents) -- so it never loads packages from your own library by accident.
# Lints go back to the agent (exit 2, on stderr); the edit itself stands.

[ -n "${TFL_HOOK_RSCRIPT:-}" ] || exit 0

input=$(cat)
path=$(printf '%s' "$input" | grep -o '"file_path"[[:space:]]*:[[:space:]]*"[^"]*"' | head -n 1 |
  sed 's/^"file_path"[[:space:]]*:[[:space:]]*"//; s/"$//' | tr '\\' '/' | sed 's#//*#/#g')

case "$path" in
  */R/*.R|*/tests/testthat/*.R) ;;
  *) exit 0 ;;
esac

"$TFL_HOOK_RSCRIPT" --vanilla "$(dirname "$0")/lint-file.R" "$path"
