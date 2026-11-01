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
here="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}" 2>/dev/null || echo "${BASH_SOURCE[0]}")")" && pwd)"
. "$here/hook-lib.sh"
hooks_off && exit 0
# the path as json_field reads it, escapes resolved, so a quote in a file's
# name is no obstacle (a pattern over the raw JSON stopped at the \" and
# left such a file unchecked)
file="$(json_field "$input" file_path)"
case "$file" in
  *.R|*.r|*.Rmd|*.rmd|*.Rmarkdown|*.qmd|*.Rnw|*.rnw) ;;
  *) exit 0 ;;
esac
# Quarto is not R's alone: a .qmd counts only when it has an R chunk
case "$file" in
  *.qmd) grep -q -m1 -E '^[[:space:]]*(```+|~~~+)[[:space:]]*\{[[:space:]]*[rR]([ ,}]|$)' "$file" 2>/dev/null || exit 0 ;;
esac
# An R file under the project root was just edited: the project has R code,
# whatever a walk at session start found, so the Bash and Stop hooks look
# again from now on. A file elsewhere (another repository, ~/.Rprofile)
# says nothing about this project.
if sd="$(session_dir "$input")"; then
  root="$(physical_root "$input")" || root=""
  fdir="$(cd "$(dirname "$file")" 2>/dev/null && pwd -P)" || fdir=""
  if [ -n "$root" ] && [ -n "$fdir" ]; then
    case "$fdir/" in "$root"/*) remember_r_project "$root" "$sd" yes ;; esac
  fi
fi
command -v Rscript >/dev/null 2>&1 || exit 0
out="$(printf '%s' "$input" | R_LSP_HOOK_SESSION="${sd:-}" Rscript --vanilla "$here/../scripts/hook-post-edit.R" 2>/dev/null)"
rc=$?
if [ "$rc" -ne 0 ]; then
  # R itself failed (a broken library, a missing package, a bug in the
  # checker) and its stderr goes nowhere: say so, since silence means
  # checked and clean. The file path is the one string put into the JSON.
  shown="$(json_text "$file")"
  printf '{"hookSpecificOutput":{"hookEventName":"PostToolUse","additionalContext":"r-lsp plugin: the post-edit check did not run on %s (Rscript exited with status %s before it could report), so the file is unchecked. Run `r-colcheck %s` yourself; it prints the error."}}\n' "$shown" "$rc" "$shown"
elif [ -n "$out" ]; then
  printf '%s\n' "$out"
fi
exit 0
