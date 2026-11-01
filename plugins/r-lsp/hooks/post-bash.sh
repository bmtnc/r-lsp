#!/bin/bash
# PostToolUse hook for Bash: an R file the command changed (a heredoc,
# sed -i, styler, roxygenise, git checkout, a code generator) gets the same
# check as an Edit, because the Edit|Write hook and the language server never
# see it. Cheap when nothing changed: a find for files newer than the
# session's stamp that the ledger of checked files does not list. In a
# project remembered as having no R code nothing is walked at all, unless
# the command itself looks like one that could have brought R code in.
input="$(cat)"
here="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}" 2>/dev/null || echo "${BASH_SOURCE[0]}")")" && pwd)"
. "$here/hook-lib.sh"
hooks_off && exit 0
case "$input" in
  *'"tool_name"'*) ;;
  *) exit 0 ;;
esac
mode=""
rish_command "$(json_field "$input" command)" && mode=recheck
check_unchecked "$input" bash "$mode" "$here"
exit 0
