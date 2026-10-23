#!/bin/bash
# SessionStart hook (startup, resume, clear, compact). Prints project context
# for Claude (stdout becomes context) and warms the language server's index
# cache in the background. Also stamps the session, so the Bash and Stop
# hooks know which R files changed from here on. Does nothing in projects
# without R files, or without R installed.
input="$(cat)"
root="${CLAUDE_PROJECT_DIR:-$PWD}"
here="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}" 2>/dev/null || echo "${BASH_SOURCE[0]}")")" && pwd)"
command -v Rscript >/dev/null 2>&1 || exit 0
. "$here/hook-lib.sh"
# Stamp the session first, R files or not: a project that gets its first R
# file later in the session is still covered. Files changed before a start
# or a resume are not this session's work; a compaction mid-work keeps the
# stamp, so a file changed before it and not yet checked is still found.
if sd="$(session_dir "$input")"; then
  case "$(json_field "$input" source)" in
    compact) [ -f "$sd/start.stamp" ] || touch "$sd/start.stamp" 2>/dev/null ;;
    *) touch "$sd/start.stamp" 2>/dev/null ;;
  esac
  # the directory's own time says "in use", so the sweep below spares it;
  # ledgers of sessions a week idle are of no use to anyone
  touch "$sd" 2>/dev/null
  find "$(dirname "$sd")" -mindepth 1 -maxdepth 1 -type d -mtime +7 -exec rm -rf {} + 2>/dev/null
fi
r_project "$root" "${sd:-}" 6 || exit 0
Rscript --vanilla "$here/../scripts/session-context.R" "$root" 2>/dev/null
# Background, with stdout/stderr closed so the hook returns at once.
( Rscript --vanilla "$here/../scripts/prewarm-index.R" "$root" >/dev/null 2>&1 & )
exit 0
