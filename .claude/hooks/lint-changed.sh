#!/usr/bin/env bash
# PostToolUse hook (Edit, Write): lint the R file just changed under R/, so a lint
# is fixed while the change is fresh rather than at the pre-PR check.
# (.lintr leaves tests and vignettes out, as CONTRIBUTING.md does.)
# It runs only when TFL_HOOK_RSCRIPT names the Rscript to use; the library
# it reads is the one the session's environment sets (R_LIBS_USER), so set
# the work library there too (see the article on developing with AI agents).
# Lints go back to the agent (exit 2, on stderr); the edit itself stands.

[ -n "${TFL_HOOK_RSCRIPT:-}" ] || exit 0

input=$(cat)
path=$(printf '%s' "$input" | grep -o '"file_path"[[:space:]]*:[[:space:]]*"[^"]*"' | head -n 1 |
  sed 's/^"file_path"[[:space:]]*:[[:space:]]*"//; s/"$//' | tr '\\' '/' | sed 's#//*#/#g')

case "$path" in
  */R/*.R) ;;
  *) exit 0 ;;
esac

"$TFL_HOOK_RSCRIPT" --vanilla "$(dirname "$0")/lint-file.R" "$path"
