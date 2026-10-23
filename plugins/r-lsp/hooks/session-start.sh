#!/bin/bash
# SessionStart hook. Prints project context for Claude (stdout becomes
# context) and warms the language server's index cache in the background.
# Does nothing in projects without R files, or without R installed.
root="${CLAUDE_PROJECT_DIR:-$PWD}"
here="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}" 2>/dev/null || echo "${BASH_SOURCE[0]}")")" && pwd)"
command -v Rscript >/dev/null 2>&1 || exit 0
if ! find "$root" -maxdepth 3 \( -name '*.R' -o -name '*.r' -o -name '*.Rmd' -o -name '*.qmd' -o -name '*.Rmarkdown' -o -name '*.Rnw' \) \
     -not -path '*/renv/*' -not -path '*/.git/*' -print -quit 2>/dev/null | grep -q .; then
  exit 0
fi
Rscript --vanilla "$here/../scripts/session-context.R" "$root" 2>/dev/null
# Background, with stdout/stderr closed so the hook returns at once.
( Rscript --vanilla "$here/../scripts/prewarm-index.R" "$root" >/dev/null 2>&1 & )
exit 0
