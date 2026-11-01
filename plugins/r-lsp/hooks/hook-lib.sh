#!/bin/bash
# Sourced by the hooks: the session directory, modification times, whether
# the project has R code at all (remembered, so a session in a directory
# without R code costs one directory walk and nothing after), and the R
# files changed in this session that no check has seen. Plain POSIX tools so
# it runs on Linux and macOS alike (bash 3.2, BSD find, stat, grep and sed).
r_lsp_tab="$(printf '\t')"

# R_LSP_HOOKS=off (also 0, false, no) turns every hook into a no-op: the
# plugin then adds nothing to a session but its shell tools and the server.
hooks_off() {
  case "${R_LSP_HOOKS:-}" in off|OFF|Off|0|false|FALSE|False|no|NO|No) return 0 ;; esac
  return 1
}

# The first string value of "FIELD": "..." in the hook JSON, escapes
# resolved. Claude Code writes the top-level fields (session_id, cwd, ...)
# before tool_input and tool_response, and a Bash command's output can carry
# the same text (a cat of a JSON file), so the first match is the field and a
# later one is data.
json_field() {
  local rest
  # everything up to and including the first "key" goes (the shortest
  # prefix), then the value is read from the start of what is left, so no
  # regex engine has to pick the leftmost of several matches
  rest="${1#*\"$2\"}"
  [ "$rest" != "$1" ] || return 0
  printf '%s' "$rest" | head -1 \
    | sed -E -n 's/^[[:space:]]*:[[:space:]]*"((\\.|[^"\\])*)".*$/\1/p' \
    | sed -e 's/\\"/"/g' -e 's/\\\\/\\/g'
}

# $1 as the inside of a JSON string: backslashes, quotes and tabs escaped,
# any other control character dropped, so a note that names a file is valid
# JSON whatever the file is called.
json_text() {
  printf '%s' "$1" | sed -e 's/[\\"]/\\&/g' -e "s/$r_lsp_tab/\\\\t/g" | tr -d '\000-\037'
}

data_base() { printf '%s' "${CLAUDE_PLUGIN_DATA:-${TMPDIR:-/tmp}/r-lsp-hooks}"; }

# $CLAUDE_PLUGIN_DATA/sessions/<session id>, created on demand; empty when
# there is no session id (scripts/hook-check-lib.R computes the same path).
# The id is a path component, so anything but [A-Za-z0-9._-] becomes '_'.
session_dir() {
  local id d
  id="$(json_field "$1" session_id)"
  [ -n "$id" ] || id="${CLAUDE_CODE_SESSION_ID:-}"
  id="$(printf '%s' "$id" | tr -c 'A-Za-z0-9._-' '_' | cut -c1-120)"
  case "$id" in ""|.|..|*[!A-Za-z0-9._-]*) return 1 ;; esac
  d="$(data_base)/sessions/$id"
  [ -d "$d" ] || mkdir -p "$d" 2>/dev/null || return 1
  printf '%s' "$d"
}

# $CLAUDE_PLUGIN_DATA/projects/<checksum of the physical root>: what the
# plugin remembers about a directory across sessions (that it has no R code).
project_dir() {
  local id d
  [ -n "$1" ] || return 1
  id="$(printf '%s' "$1" | cksum 2>/dev/null | cut -d' ' -f1)"
  case "$id" in ""|*[!0-9]*) return 1 ;; esac
  d="$(data_base)/projects/$id"
  [ -d "$d" ] || mkdir -p "$d" 2>/dev/null || return 1
  printf '%s' "$d"
}

mtime_of() { stat -c %Y "$1" 2>/dev/null || stat -f %m "$1" 2>/dev/null; }
# "mtime:size", the key the ledger stores for a checked file.
stamp_of() { stat -c %Y:%s "$1" 2>/dev/null || stat -f %m:%z "$1" 2>/dev/null; }

# Is the file $1 there and younger than $2 minutes?
younger_than() {
  local m now
  m="$(mtime_of "$1")" || return 1
  case "$m" in ""|*[!0-9]*) return 1 ;; esac
  now="$(date +%s)"
  [ $(( now - m )) -lt $(( $2 * 60 )) ]
}

# The project root as a physical path (the ledger and the project memory key
# on it, so a symlinked path and its target are one project).
physical_root() {
  local r="${CLAUDE_PROJECT_DIR:-}"
  [ -n "$r" ] || r="$(json_field "$1" cwd)"
  [ -n "$r" ] || r="$PWD"
  (cd "$r" 2>/dev/null && pwd -P)
}

# Directories never worth walking for R code or changed files: package
# libraries (R's own too: site-library, a user library such as
# x86_64-pc-linux-gnu-library, macOS's Library, and an R installation's
# lib/R, lib64/R, share/R or R.framework, so a session started in $HOME or
# /usr is not an R project), dependency caches, build output, R CMD check and
# pkgdown/quarto output (*.Rcheck, inst/doc, _site, _freeze), the targets
# store, editor and VCS internals, the plugin's own cache. One list for
# every walk in this file. The R side (scripts/colcheck-lib.R
# project_skip_dirs, scripts/hook-post-lsp.R) skips the R-specific part of
# it: libraries, check and build output, VCS internals. An R scripts
# project whose only R code sits under build/, dist/, target/ or vendor/ is
# not seen: a DESCRIPTION, renv.lock or .Rproj at the root, or an edit of an
# R file, still makes it an R project.
r_lsp_skip_dirs="renv packrat .git .svn .hg node_modules bower_components .venv venv .tox __pycache__ site-packages
.mypy_cache .pytest_cache .ruff_cache target build dist .next .nuxt .cache .gradle .idea .vscode .terraform vendor
Pods .yarn .pnpm-store coverage .Rproj.user .colcheck site-library *-gnu-library *-mingw32-library Library .Trash Trash
*.Rcheck _targets _freeze _site revdep R.framework"
r_lsp_skip_paths="*/inst/doc */lib/R */lib64/R */share/R"
# The find expression that prunes them, to expand unquoted with globbing off
# (set -f: the patterns are find's, not the shell's):
# -mindepth 1 ( -name a -o -name b ... -o -path */inst/doc ... -o -name .?* -type d ) -prune -o
# The root itself is never pruned (-mindepth 1 keeps every test off it), so
# a project checked out as build/, dist/ or vendor/ is walked like any
# other, and the paths printed are the root's as given plus what lies
# under it, the form the ledger keys on. Every hidden directory is pruned,
# in the list or not: the editor servers and package managers under $HOME
# (.vscode-server, .emacs.d, .local, .cargo) hold R files that are nobody's
# project, and a session started there must stay dormant; an R file under
# .github/ or the like is still reached by the edit hook, which walks
# nothing.
prune_expr() {
  local d sep='('
  printf '%s' '-mindepth 1 '
  for d in $r_lsp_skip_dirs; do
    printf '%s -name %s' "$sep" "$d"
    sep=' -o'
  done
  for d in $r_lsp_skip_paths; do printf ' -o -path %s' "$d"; done
  printf ' -o -name .?* -type d ) -prune -o'
}

# Is there R code under $1 within $2 levels? An R, R Markdown or Sweave file,
# or a package or renv marker, settles it at the first hit. Quarto is not
# R's alone: a .qmd counts only when one of them (the first twenty found)
# has an R chunk. A directory without any costs the whole walk, which is
# why r_project() remembers the answer.
r_files_under() {
  local root="$1" depth="$2" q
  set -f
  # shellcheck disable=SC2046
  if find "$root" -maxdepth "$depth" $(prune_expr) \
       -type f \( -name '*.R' -o -name '*.r' -o -name '*.Rmd' -o -name '*.rmd' -o -name '*.Rmarkdown' -o -name '*.Rnw' \
       -o -name '*.rnw' -o -name DESCRIPTION -o -name renv.lock -o -name '*.Rproj' \) -print -quit 2>/dev/null | grep -q .; then
    set +f
    return 0
  fi
  # shellcheck disable=SC2046
  q="$(find "$root" -maxdepth "$depth" $(prune_expr) -type f -name '*.qmd' -print 2>/dev/null | head -20)"
  set +f
  [ -n "$q" ] || return 1
  printf '%s\n' "$q" | tr '\n' '\0' | xargs -0 grep -l -m1 -E '^[[:space:]]*(```+|~~~+)[[:space:]]*\{[[:space:]]*[rR]([ ,}]|$)' 2>/dev/null | grep -q .
}

# Remember whether the project at $1 has R code: in the session directory
# ($2) as the marker r-project or no-r-project, holding the root it is
# about (a session's directory can change: a worktree, a resume from
# elsewhere), and across sessions under projects/ (a day's memory that a
# directory has none). $3 is yes or no.
remember_r_project() {
  local root="$1" sd="$2" pd
  # nothing is known about no directory, and an empty marker would count
  # for every root (marker_for)
  [ -n "$root" ] || return 1
  pd="$(project_dir "$root")" || pd=""
  if [ "$3" = yes ]; then
    [ -n "$sd" ] && { printf '%s' "$root" > "$sd/r-project"; rm -f "$sd/no-r-project"; } 2>/dev/null
    [ -n "$pd" ] && rm -f "$pd/no-r-project" 2>/dev/null
  else
    [ -n "$sd" ] && printf '%s' "$root" > "$sd/no-r-project" 2>/dev/null
    # the day's memory names its directory too: its key is a checksum of
    # the path, which two paths can share
    [ -n "$pd" ] && printf '%s' "$root" > "$pd/no-r-project" 2>/dev/null
    # the weekly sweep in session-start.sh reads each directory's own time,
    # which rewriting a file inside does not move: a memory refreshed
    # today is in use
    [ -n "$pd" ] && touch "$pd" 2>/dev/null
  fi
  [ -n "$sd" ] && touch "$sd" 2>/dev/null
  return 0
}

# Is the marker $1 there, about the root $2 (an older marker without a root counts)?
marker_for() {
  local content
  [ -f "$1" ] || return 1
  content="$(cat "$1" 2>/dev/null)"
  [ -z "$content" ] || [ "$content" = "$2" ]
}

# Does the project at $1 (a physical path) have R code? The answer is
# remembered: in the session directory ($2) as r-project (final for the
# session; an edit of an R file under the root sets it too) or no-r-project,
# and for a day under projects/. $4 names the occasion:
#   fresh    a new session, or /clear: walk, whatever is remembered, so a
#            directory that gained R code outside Claude is seen;
#   start    a resume, a compaction, a subagent, the monitor: the session's
#            memory, else the day's;
#   recheck  a Bash command that could have brought R code in: walk, unless
#            a recheck for the same command walked within the last minute
#            (the throttle is for a repeated command in a tree too big to
#            walk, not for the second command of a `curl` then `tar`);
#   (none)   the Bash, Stop and LSP hooks: the session's memory for ten
#            minutes, then one walk, so R code that arrived by a route no
#            hook sees is found within ten minutes.
# A walk that a hook's timeout cut short left no answer: the walk's own
# marker then counts as "no R code" for half an hour, so the hooks do not
# repeat a walk the tree is too big for. $3 is how deep to look (six levels:
# an R package nested in a monorepo, a script under analysis/2024/q3/).
r_project() {
  local root="$1" sd="$2" depth="${3:-6}" mode="${4:-}" pd
  # no root (a cwd that is gone and no CLAUDE_PROJECT_DIR): no answer, and
  # no marker, since an empty one would count for every root
  [ -n "$root" ] || return 1
  if [ -n "$sd" ]; then
    marker_for "$sd/r-project" "$root" && return 0
    if marker_for "$sd/no-r-project" "$root"; then
      case "$mode" in
        fresh) ;;
        recheck) younger_than "$sd/last-recheck" 1 && [ "$(cat "$sd/last-recheck" 2>/dev/null)" = "$r_lsp_recheck_key" ] && return 1 ;;
        start) return 1 ;;
        *) younger_than "$sd/no-r-project" 10 && return 1 ;;
      esac
    fi
    if [ "$mode" != fresh ] && younger_than "$sd/walk-started" 30; then return 1; fi
  fi
  if [ "$mode" = start ]; then
    pd="$(project_dir "$root")" || pd=""
    if [ -n "$pd" ] && marker_for "$pd/no-r-project" "$root" && younger_than "$pd/no-r-project" 1440; then
      [ -n "$sd" ] && printf '%s' "$root" > "$sd/no-r-project" 2>/dev/null
      return 1
    fi
  fi
  [ -n "$sd" ] && touch "$sd/walk-started" 2>/dev/null
  [ -n "$sd" ] && [ "$mode" = recheck ] && printf '%s' "$r_lsp_recheck_key" > "$sd/last-recheck" 2>/dev/null
  if r_files_under "$root" "$depth"; then
    [ -n "$sd" ] && rm -f "$sd/walk-started" 2>/dev/null
    remember_r_project "$root" "$sd" yes
    return 0
  fi
  [ -n "$sd" ] && rm -f "$sd/walk-started" 2>/dev/null
  remember_r_project "$root" "$sd" no
  return 1
}

# The command a recheck is for, as its checksum (cksum of
# tool_input.command), set by check_unchecked() before the walk and kept by
# r_project() in last-recheck with the time of that walk: the one-minute
# throttle then holds for the same command again (a loop of `git pull`s in
# a tree too big to walk in a hook's time) and never for a second command
# that could have brought R code in (`curl`, then `tar`). A variable, not an
# argument, because the walk functions between the two take no command.
r_lsp_recheck_key=""
recheck_key() { printf '%s' "$1" | cksum 2>/dev/null | cut -d' ' -f1; }

# A Bash command that could bring R code into a project that had none: it
# names an R file (a glob too) or one of the marker files the walk looks
# for (DESCRIPTION, renv.lock, an .Rproj), runs an R tool (by name or by
# path; R itself with an option or as `R CMD`), or fetches, unpacks, copies,
# links or switches files (git with options before the subcommand, gh, svn,
# hg, scp, tar, 7z, ln). Matched against tool_input.command alone (never the
# output), the tool names as whole words. A generator the list does not
# know (`python3 scaffold.py`) is caught by the Stop hook's ten-minute look,
# by an Edit of an R file, or by the next new session.
r_lsp_rish_command='[A-Za-z0-9_./*-][.](R|r|Rmd|rmd|Rmarkdown|qmd|Rnw|rnw)([^A-Za-z0-9_]|$)|[.]Rproj([^A-Za-z0-9_]|$)|(^|[^A-Za-z0-9_])(DESCRIPTION|renv[.]lock)([^A-Za-z0-9_]|$)|(^|[^A-Za-z0-9_])R +(-|CMD)|(^|[^A-Za-z0-9_])(Rscript|renv|rmarkdown|quarto|devtools|usethis|pak|remotes|cp|mv|ln|tar|bsdtar|7z|rsync|scp|curl|wget|unzip|gunzip)([^A-Za-z0-9_]|$)|(^|[^A-Za-z0-9_])git +([^|;&]* +)?(checkout|switch|pull|merge|stash|clone|rebase|cherry-pick|reset|worktree|restore|apply|am|submodule)([^A-Za-z0-9_]|$)|(^|[^A-Za-z0-9_])(gh +(repo +clone|pr +checkout)|svn +(checkout|co|update|up|switch)|hg +(clone|pull|update|checkout))([^A-Za-z0-9_]|$)'
# rish_command COMMAND: true when the command matches the pattern above.
# json_field leaves the JSON escapes \n, \t and \r between the lines of a
# multi-line command as they are; they become spaces first, so a tool named
# at the start of a later line is still a whole word.
rish_command() { printf '%s' "$1" | sed 's/\\[ntr]/ /g' | grep -qE "$r_lsp_rish_command"; }

# R files under $1 that arrived or changed since the session's stamp in $2
# (by inode change time, so a file copied or moved in with an old
# modification time counts too) whose current mtime:size is not in the
# ledger: one "mtime:size<TAB>path" per line, the key that the ledger will
# store for the file as it was found (not as it may be after the check).
# Nothing is walked once the project is remembered as having no R code.
unchecked_r_files() {
  local root="$1" sd="$2" mode="${3:-}" stamp ledger f m
  stamp="$sd/start.stamp"
  ledger="$sd/checked.tsv"
  [ -f "$stamp" ] || return 0
  r_project "$root" "$sd" 6 "$mode" || return 0
  set -f
  # shellcheck disable=SC2046
  find "$root" $(prune_expr) \
       -type f -cnewer "$stamp" \( -name '*.R' -o -name '*.r' -o -name '*.Rmd' -o -name '*.rmd' -o -name '*.Rmarkdown' \
       -o -name '*.qmd' -o -name '*.Rnw' -o -name '*.rnw' \) -print 2>/dev/null |
  while IFS= read -r f; do
    # Quarto is not R's alone: a .qmd counts only with an R chunk (the test
    # post-edit.sh and r_files_under apply)
    case "$f" in
      *.qmd) grep -q -m1 -E '^[[:space:]]*(```+|~~~+)[[:space:]]*\{[[:space:]]*[rR]([ ,}]|$)' "$f" 2>/dev/null || continue ;;
    esac
    m="$(stamp_of "$f")"
    [ -n "$m" ] || continue
    if [ -f "$ledger" ] && grep -qxF -- "$m$r_lsp_tab$f" "$ledger" 2>/dev/null; then continue; fi
    printf '%s\n' "$m$r_lsp_tab$f"
  done
  set +f
}

# Run the multi-file check on the unchecked files, if any. $1 = hook JSON,
# $2 = via (bash|stop), $3 = recheck to look for R code again although the
# project was remembered as having none, $4 = this hook's directory. Prints
# the R script's output (JSON) or nothing. The cheap tests come first: no
# R, no session, no stamp, no R code, nothing changed.
check_unchecked() {
  local input="$1" via="$2" mode="${3:-}" here="${4:-}" root sd files n
  command -v Rscript >/dev/null 2>&1 || return 0
  [ -n "$here" ] || here="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}" 2>/dev/null || echo "${BASH_SOURCE[0]}")")" && pwd)"
  sd="$(session_dir "$input")" || return 0
  [ -f "$sd/start.stamp" ] || return 0
  root="$(physical_root "$input")" || return 0
  # a recheck is throttled for its own command alone
  r_lsp_recheck_key=""
  [ "$mode" = recheck ] && r_lsp_recheck_key="$(recheck_key "$(json_field "$input" command)")"
  files="$(unchecked_r_files "$root" "$sd" "$mode")"
  [ -n "$files" ] || return 0
  n="$(printf '%s\n' "$files" | grep -c .)"
  # Every path goes to R through a file (paths may hold spaces; a branch
  # switch may touch hundreds): R checks the newest ten and records the rest
  # as seen, so nothing comes back at the next command.
  local list out rc
  list="$(mktemp "${TMPDIR:-/tmp}/r-lsp-hook.XXXXXX")" || return 0
  printf '%s\n' "$files" > "$list"
  # the ledger R appends to is memory in use: the directory's own time, which
  # the weekly sweep in session-start.sh reads, says so
  touch "$sd" 2>/dev/null
  out="$(printf '%s' "$input" | R_LSP_HOOK_VIA="$via" R_LSP_HOOK_ROOT="$root" R_LSP_HOOK_SESSION="$sd" R_LSP_HOOK_LIST="$list" \
    Rscript --vanilla "$here/../scripts/hook-post-bash.R" 2>/dev/null)"
  rc=$?
  rm -f "$list"
  if [ "$rc" -ne 0 ]; then
    # R itself failed and its stderr goes nowhere: say so (the ledger was
    # not written, so the files come back at the next hook). Paths are the
    # one string put into the JSON: each is escaped on its own (json_text)
    # and they are joined here, so a comma in a name stays as it is.
    local shown="" f
    while IFS= read -r f; do
      shown="$shown${shown:+, }$(json_text "$f")"
    done <<EOF
$(printf '%s\n' "$files" | cut -f2- | head -5)
EOF
    if [ "$via" = stop ]; then
      printf '{"systemMessage":"r-lsp plugin: %s R file(s) changed in this session could not be checked (Rscript exited with status %s): %s. Run `r-colcheck FILE` on them yourself; it prints the error."}\n' "$n" "$rc" "$shown"
    else
      printf '{"hookSpecificOutput":{"hookEventName":"PostToolUse","additionalContext":"r-lsp plugin: this command changed %s R file(s) (%s) and the check did not run on them (Rscript exited with status %s before it could report). Run `r-colcheck FILE` on them yourself; it prints the error."}}\n' "$n" "$shown" "$rc"
    fi
  elif [ -n "$out" ]; then
    printf '%s\n' "$out"
  fi
}
