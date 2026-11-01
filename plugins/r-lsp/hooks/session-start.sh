#!/bin/bash
# SessionStart hook (startup, resume, clear, compact). Prints project context
# for Claude (stdout becomes context) and, on a cold cache, warms the
# language server's index in the background. Also stamps the session, so
# the Bash and Stop hooks know which R files changed from here on, and saves
# the context for the subagents of this session. Does nothing in projects
# without R files (a new session looks once; a resumed one, a compaction and
# the other hooks trust what it found), or with R_LSP_HOOKS=off. A project
# with R code but no Rscript on PATH gets one line saying so, since silence
# would read as "checked and clean".
input="$(cat)"
here="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}" 2>/dev/null || echo "${BASH_SOURCE[0]}")")" && pwd)"
. "$here/hook-lib.sh"
hooks_off && exit 0
root="$(physical_root "$input")" || root="${CLAUDE_PROJECT_DIR:-$PWD}"
source="$(json_field "$input" source)"
# Stamp the session first, R files or not: a project that gets its first R
# file later in the session is still covered. Files changed before a start
# or a resume are not this session's work; a compaction mid-work keeps the
# stamp, so a file changed before it and not yet checked is still found.
if sd="$(session_dir "$input")"; then
  case "$source" in
    compact) [ -f "$sd/start.stamp" ] || touch "$sd/start.stamp" 2>/dev/null ;;
    *) touch "$sd/start.stamp" 2>/dev/null ;;
  esac
  # the directory's own time says "in use", so the sweep below spares it
  # (remember_r_project and check_unchecked move it too, when the memory is
  # refreshed or the ledger written mid-session); ledgers of sessions a week
  # idle, and what was remembered about a directory a week ago, are of no
  # use to anyone
  touch "$sd" 2>/dev/null
  find "$(dirname "$sd")" -mindepth 1 -maxdepth 1 -type d -mtime +7 -exec rm -rf {} + 2>/dev/null
  find "$(data_base)/projects" -mindepth 1 -maxdepth 1 -type d -mtime +7 -exec rm -rf {} + 2>/dev/null
fi
# a new session, a fork and /clear take a fresh look; a resume or a compaction trusts the memory
case "$source" in startup|fork|clear|"") mode=fresh ;; *) mode=start ;; esac
r_project "$root" "${sd:-}" 6 "$mode" || exit 0
if ! command -v Rscript >/dev/null 2>&1; then
  printf 'R LSP plugin: this project has R code but Rscript is not on the PATH Claude Code was started with, so the plugin'"'"'s checks, context and language server are off. Start Claude Code from a shell where `Rscript --version` works.\n'
  exit 0
fi
# the context for Claude, and the same text for this session's subagents
# (subagent-start.sh prints the saved file instead of running R again)
if [ -n "${sd:-}" ]; then
  R_LSP_HOOK_SESSION="$sd" Rscript --vanilla "$here/../scripts/session-context.R" "$root" 2>/dev/null
else
  Rscript --vanilla "$here/../scripts/session-context.R" "$root" 2>/dev/null
fi
# Background, with stdout/stderr closed so the hook returns at once. Not
# after a compaction: the server is running and holds the index already.
if [ "$source" != compact ]; then
  ( Rscript --vanilla "$here/../scripts/prewarm-index.R" "$root" >/dev/null 2>&1 & )
fi
exit 0
