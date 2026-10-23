#!/bin/bash
# PostToolUse hook for Bash: an R file the command changed (a heredoc,
# sed -i, styler, roxygenise, git checkout, a code generator) gets the same
# check as an Edit, because the Edit|Write hook and the language server never
# see it. Cheap when nothing changed: a find for files newer than the
# session's stamp that the ledger of checked files does not list.
input="$(cat)"
case "$input" in
  *'"tool_name"'*) ;;
  *) exit 0 ;;
esac
here="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}" 2>/dev/null || echo "${BASH_SOURCE[0]}")")" && pwd)"
. "$here/hook-lib.sh"
check_unchecked "$input" bash
exit 0
