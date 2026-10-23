#!/bin/bash
# PostToolUse hook for Edit|Write: check column names in the edited R file
# against the data the file reads (scripts/colcheck-lib.R). Exits before
# starting R unless the edited file is an .R file or a literate one
# (R Markdown, Quarto, Sweave), whose chunks are checked.
input="$(cat)"
case "$input" in
  *'"file_path"'*) ;;
  *) exit 0 ;;
esac
printf '%s' "$input" | grep -qE '"file_path": *"[^"]*\.(R|r|Rmd|rmd|Rmarkdown|qmd|Rnw|rnw)"' || exit 0
command -v Rscript >/dev/null 2>&1 || exit 0
here="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}" 2>/dev/null || echo "${BASH_SOURCE[0]}")")" && pwd)"
out="$(printf '%s' "$input" | Rscript --vanilla "$here/../scripts/hook-post-edit.R" 2>/dev/null)"
rc=$?
if [ "$rc" -ne 0 ]; then
  # R itself failed (a broken library, a missing package, a bug in the
  # checker) and its stderr goes nowhere: say so, since silence means
  # checked and clean. The file path is the one string put into the JSON.
  file="$(printf '%s' "$input" | sed -n 's/.*"file_path": *"\([^"]*\)".*/\1/p' | head -1 | sed 's/[\\"]/\\&/g')"
  printf '{"hookSpecificOutput":{"hookEventName":"PostToolUse","additionalContext":"r-lsp plugin: the post-edit check did not run on %s (Rscript exited with status %s before it could report), so the file is unchecked. Run `r-colcheck %s` yourself; it prints the error."}}\n' "$file" "$rc" "$file"
elif [ -n "$out" ]; then
  printf '%s\n' "$out"
fi
exit 0
