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
printf '%s' "$input" | Rscript --vanilla "$here/../scripts/hook-post-edit.R" 2>/dev/null
exit 0
