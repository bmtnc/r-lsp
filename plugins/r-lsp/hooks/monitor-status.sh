#!/bin/bash
# Plugin monitor: forward the language server's index events for this
# project to Claude as notifications. The server appends to
# $CLAUDE_PLUGIN_DATA/status.log (passed as the first argument, because
# monitors do not receive the plugin variables in their environment); each
# line carries the project directory in brackets, so sessions in other
# projects are filtered out.
data_dir="$1"
[ -n "$data_dir" ] || exit 0
# Nothing to watch outside an R project: the same test the session-start
# hook applies (an R, Rmd or qmd file within three levels, renv and git
# internals excepted, and Rscript on PATH), so a session in a directory
# with no R code runs no monitor at all.
command -v Rscript >/dev/null 2>&1 || exit 0
find "$PWD" -maxdepth 3 \( -name '*.R' -o -name '*.r' -o -name '*.Rmd' -o -name '*.qmd' \) \
     -not -path '*/renv/*' -not -path '*/.git/*' -print -quit 2>/dev/null | grep -q . || exit 0
file="$data_dir/status.log"
mkdir -p "$data_dir" 2>/dev/null
touch "$file" 2>/dev/null || exit 0
# Only the events that change what Claude should do next are forwarded:
# the index becoming ready (a lookup can be trusted or retried now), a second
# repository joining the workspace, a too-old or missing languageserver, and
# the failures that make answers wrong (renv not activated, a patch off, a
# loader crash, too many files changed on disk to re-read). The rest
# ("server started", "loaded N files", "indexing") stays in the log for
# r-lsp-status and the post-LSP note. The server's pid tag is dropped with
# the timestamp and project tag.
tail -n 0 -F "$file" 2>/dev/null \
  | grep --line-buffered -F "[$PWD]" \
  | grep --line-buffered -E "index ready|added workspace|older than|not installed|renv activation|added no library|patch disabled|failed|not re-read" \
  | sed -u -e 's/^[0-9:]* \[[^]]*\] //' -e 's/^\[pid [0-9]*\] //'
