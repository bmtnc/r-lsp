#!/bin/bash
# SubagentStart hook: the project context the main conversation got at
# session start, for a subagent that starts from nothing, plus what differs
# for it (the server's diagnostics stay in the main conversation; the hooks'
# notes are its check). Only in R projects, like session-start.sh, and from
# the text that hook saved for this session when it is there, so a subagent
# costs no walk and no R start; R runs only when nothing was saved.
input="$(cat)"
here="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}" 2>/dev/null || echo "${BASH_SOURCE[0]}")")" && pwd)"
. "$here/hook-lib.sh"
hooks_off && exit 0
root="$(physical_root "$input")" || root="${CLAUDE_PROJECT_DIR:-$PWD}"
sd="$(session_dir "$input")" || sd=""
r_project "$root" "$sd" 6 start || exit 0
if [ -n "$sd" ] && [ -s "$sd/context-subagent.json" ]; then
  cat "$sd/context-subagent.json"
  exit 0
fi
command -v Rscript >/dev/null 2>&1 || exit 0
Rscript --vanilla "$here/../scripts/session-context.R" "$root" subagent 2>/dev/null
exit 0
