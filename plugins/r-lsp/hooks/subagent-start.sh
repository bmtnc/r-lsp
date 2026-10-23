#!/bin/bash
# SubagentStart hook: the project context the main conversation got at
# session start, for a subagent that starts from nothing, plus what differs
# for it (the server's diagnostics stay in the main conversation; the hooks'
# notes are its check). Only in R projects, like session-start.sh.
input="$(cat)"
root="${CLAUDE_PROJECT_DIR:-$PWD}"
here="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}" 2>/dev/null || echo "${BASH_SOURCE[0]}")")" && pwd)"
command -v Rscript >/dev/null 2>&1 || exit 0
. "$here/hook-lib.sh"
sd="$(session_dir "$input")" || sd=""
r_project "$root" "$sd" 6 || exit 0
Rscript --vanilla "$here/../scripts/session-context.R" "$root" subagent 2>/dev/null
exit 0
