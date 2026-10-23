#!/bin/bash
# Sourced by the hooks: the session directory, modification times and the R
# files changed in this session that no check has seen. Plain POSIX tools so
# it runs on Linux and macOS alike.
r_lsp_tab="$(printf '\t')"

# The first string value of "FIELD": "..." in the hook JSON (top level or not;
# the hook payloads carry session_id and cwd once).
json_field() {
  printf '%s' "$1" | sed -n "s/.*\"$2\" *: *\"\([^\"]*\)\".*/\1/p" | head -1
}

# $CLAUDE_PLUGIN_DATA/sessions/<session id>, created on demand; empty when
# there is no session id (scripts/hook-check-lib.R computes the same path).
# The id is a path component, so anything but [A-Za-z0-9._-] becomes '_'.
session_dir() {
  local id base d
  id="$(json_field "$1" session_id)"
  [ -n "$id" ] || id="${CLAUDE_CODE_SESSION_ID:-}"
  id="$(printf '%s' "$id" | tr -c 'A-Za-z0-9._-' '_' | cut -c1-120)"
  case "$id" in ""|.|..|*[!A-Za-z0-9._-]*) return 1 ;; esac
  base="${CLAUDE_PLUGIN_DATA:-${TMPDIR:-/tmp}/r-lsp-hooks}"
  d="$base/sessions/$id"
  mkdir -p "$d" 2>/dev/null || return 1
  printf '%s' "$d"
}

# Is there R code under $1? Session start looks six levels deep once and
# leaves a marker in the session directory ($2); the hooks that run on every
# Bash call test the marker, and fall back to a shallow look for a project
# that gained its first R file later in the session.
r_project() {
  local root="$1" sd="$2" depth="${3:-3}"
  [ -n "$sd" ] && [ -f "$sd/r-project" ] && return 0
  find "$root" -maxdepth "$depth" \( -name renv -o -name .git -o -name node_modules \) -prune -o \
       -type f \( -name '*.R' -o -name '*.r' -o -name '*.Rmd' -o -name '*.qmd' -o -name '*.Rmarkdown' -o -name '*.Rnw' \) \
       -print -quit 2>/dev/null | grep -q . || return 1
  [ -n "$sd" ] && touch "$sd/r-project" 2>/dev/null
  return 0
}

mtime_of() { stat -c %Y "$1" 2>/dev/null || stat -f %m "$1" 2>/dev/null; }
# "mtime:size", the key the ledger stores for a checked file.
stamp_of() { stat -c %Y:%s "$1" 2>/dev/null || stat -f %m:%z "$1" 2>/dev/null; }

# The project root as a physical path (the ledger stores resolved paths).
physical_root() {
  local r="${CLAUDE_PROJECT_DIR:-}"
  [ -n "$r" ] || r="$(json_field "$1" cwd)"
  [ -n "$r" ] || r="$PWD"
  (cd "$r" 2>/dev/null && pwd -P)
}

# R files under $1 that arrived or changed since the session's stamp in $2
# (by inode change time, so a file copied or moved in with an old
# modification time counts too) whose current mtime:size is not in the
# ledger: one path per line. Outside an R project nothing is walked, so a
# Bash call there costs one shallow find.
unchecked_r_files() {
  local root="$1" sd="$2" stamp ledger f m
  stamp="$sd/start.stamp"
  ledger="$sd/checked.tsv"
  [ -f "$stamp" ] || return 0
  r_project "$root" "$sd" || return 0
  find "$root" \( -name renv -o -name packrat -o -name .git -o -name .svn -o -name .hg -o -name node_modules \
       -o -name .Rproj.user -o -name .cache -o -name .colcheck \) -prune -o \
       -type f -cnewer "$stamp" \( -name '*.R' -o -name '*.r' -o -name '*.Rmd' -o -name '*.rmd' -o -name '*.Rmarkdown' \
       -o -name '*.qmd' -o -name '*.Rnw' -o -name '*.rnw' \) -print 2>/dev/null |
  while IFS= read -r f; do
    m="$(stamp_of "$f")"
    [ -n "$m" ] || continue
    if [ -f "$ledger" ] && grep -qxF -- "$m$r_lsp_tab$f" "$ledger" 2>/dev/null; then continue; fi
    printf '%s\n' "$f"
  done
}

# Run the multi-file check on the unchecked files, if any. $1 = hook JSON,
# $2 = via (bash|stop). Prints the R script's output (JSON) or nothing.
check_unchecked() {
  local input="$1" via="$2" here root sd files n
  here="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}" 2>/dev/null || echo "${BASH_SOURCE[0]}")")" && pwd)"
  sd="$(session_dir "$input")" || return 0
  [ -f "$sd/start.stamp" ] || return 0
  root="$(physical_root "$input")" || return 0
  files="$(unchecked_r_files "$root" "$sd")"
  [ -n "$files" ] || return 0
  command -v Rscript >/dev/null 2>&1 || return 0
  n="$(printf '%s\n' "$files" | grep -c .)"
  # Every path goes to R through a file (paths may hold spaces; a branch
  # switch may touch hundreds): R checks the newest ten and records the rest
  # as seen, so nothing comes back at the next command.
  local list out rc
  list="$(mktemp "${TMPDIR:-/tmp}/r-lsp-hook.XXXXXX")" || return 0
  printf '%s\n' "$files" > "$list"
  out="$(printf '%s' "$input" | R_LSP_HOOK_VIA="$via" R_LSP_HOOK_ROOT="$root" R_LSP_HOOK_SESSION="$sd" R_LSP_HOOK_LIST="$list" \
    Rscript --vanilla "$here/../scripts/hook-post-bash.R" 2>/dev/null)"
  rc=$?
  rm -f "$list"
  if [ "$rc" -ne 0 ]; then
    # R itself failed and its stderr goes nowhere: say so (the ledger was
    # not written, so the files come back at the next hook). Paths are the
    # one string put into the JSON; quotes and backslashes are escaped.
    local shown
    shown="$(printf '%s\n' "$files" | head -5 | sed 's/[\\"]/\\&/g' | paste -sd ',' - | sed 's/,/, /g')"
    if [ "$via" = stop ]; then
      printf '{"systemMessage":"r-lsp plugin: %s R file(s) changed in this session could not be checked (Rscript exited with status %s): %s. Run `r-colcheck FILE` on them yourself; it prints the error."}\n' "$n" "$rc" "$shown"
    else
      printf '{"hookSpecificOutput":{"hookEventName":"PostToolUse","additionalContext":"r-lsp plugin: this command changed %s R file(s) (%s) and the check did not run on them (Rscript exited with status %s before it could report). Run `r-colcheck FILE` on them yourself; it prints the error."}}\n' "$n" "$shown" "$rc"
    fi
  elif [ -n "$out" ]; then
    printf '%s\n' "$out"
  fi
}
