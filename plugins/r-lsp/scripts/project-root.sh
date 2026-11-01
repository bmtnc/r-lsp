#!/bin/bash
# Sourced by the shell tools (bin/r-colcheck, bin/r-colmap, bin/r-callmap):
# the project root for a command typed in DIR, as the hooks and the server
# find it. The nearest directory at or above DIR with a project marker
# (renv.lock, DESCRIPTION, .git, .here, _targets.R, *.Rproj), the walk
# stopping below the home directory, so a dotfiles repository or a .here in
# $HOME never counts as the project; else $CLAUDE_PROJECT_DIR when Claude
# Code set it; else DIR itself. Prints the root.
project_root() {
  local d="${1:-$PWD}" home
  home="$(cd ~ 2>/dev/null && pwd -P)"
  d="$(cd "$d" 2>/dev/null && pwd -P)" || d="$PWD"
  local start="$d"
  while [ -n "$d" ] && [ "$d" != "/" ] && [ "$d" != "$home" ]; do
    if [ -e "$d/renv.lock" ] || [ -e "$d/DESCRIPTION" ] || [ -e "$d/.git" ] || [ -e "$d/.here" ] \
       || [ -e "$d/_targets.R" ] || ls "$d"/*.Rproj >/dev/null 2>&1; then printf '%s' "$d"; return 0; fi
    d="$(dirname "$d")"
  done
  printf '%s' "${CLAUDE_PROJECT_DIR:-$start}"
}
