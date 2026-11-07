#!/usr/bin/env bash
# PreToolUse hook (Edit, Write): a pull request leaves NEWS.md and the
# version in DESCRIPTION alone -- they are written when a release is made
# (CONTRIBUTING.md).  An edit to either is refused, with the reason, so the
# agent puts the change's summary in the PR description instead.
# A maintainer making a release sets TFL_ALLOW_RELEASE_EDIT=1.

[ "${TFL_ALLOW_RELEASE_EDIT:-}" = "1" ] && exit 0

input=$(cat)
# the edited file's path, as the tool gives it (absolute; backslashes on Windows)
path=$(printf '%s' "$input" | grep -o '"file_path"[[:space:]]*:[[:space:]]*"[^"]*"' | head -n 1 |
  sed 's/^"file_path"[[:space:]]*:[[:space:]]*"//; s/"$//' | tr '\\' '/')

case "$path" in
  */NEWS.md)
    echo "NEWS.md is written when a release is made, not in a pull request: put the change's summary in the PR description (\"User-visible change (for NEWS)\")." >&2
    exit 2 ;;
  */DESCRIPTION)
    if printf '%s' "$input" | grep -q 'Version:'; then
      echo "The version in DESCRIPTION changes only for a release, not in a pull request. Edit the other fields without touching the Version: line." >&2
      exit 2
    fi ;;
esac
exit 0
