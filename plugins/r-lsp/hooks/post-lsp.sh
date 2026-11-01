#!/bin/bash
# PostToolUse hook for the LSP tool: add what the language server's answer
# does not cover (index still building; mentions in files it does not
# index). Only for R files: another language's server answers for its own
# files, and this hook has nothing to add there, so R is not started.
input="$(cat)"
here="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}" 2>/dev/null || echo "${BASH_SOURCE[0]}")")" && pwd)"
. "$here/hook-lib.sh"
hooks_off && exit 0
file="$(json_field "$input" filePath)"
if [ -n "$file" ]; then
  case "$file" in
    *.R|*.r|*.Rmd|*.rmd|*.Rmarkdown|*.qmd|*.Rnw|*.rnw) ;;
    *) exit 0 ;;
  esac
else
  # no file in the request (a workspace symbol search): an R project's, or
  # not ours; a root that cannot be found (a cwd that is gone) is not ours
  # either, and leaves no memory behind
  sd="$(session_dir "$input")" || sd=""
  root="$(physical_root "$input")" || exit 0
  r_project "$root" "$sd" || exit 0
fi
command -v Rscript >/dev/null 2>&1 || exit 0
printf '%s' "$input" | Rscript --vanilla "$here/../scripts/hook-post-lsp.R" 2>/dev/null
exit 0
