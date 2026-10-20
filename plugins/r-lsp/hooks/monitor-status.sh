#!/bin/bash
# Plugin monitor: forward the language server's index events for this
# project to Claude as notifications. The server appends to
# $CLAUDE_PLUGIN_DATA/status.log (passed as the first argument, because
# monitors do not receive the plugin variables in their environment); each
# line carries the project directory in brackets, so sessions in other
# projects are filtered out.
data_dir="$1"
[ -n "$data_dir" ] || exit 0
file="$data_dir/status.log"
mkdir -p "$data_dir" 2>/dev/null
touch "$file" 2>/dev/null || exit 0
tail -n 0 -F "$file" 2>/dev/null \
  | grep --line-buffered -F "[$PWD]" \
  | sed -u -e 's/^[0-9:]* \[[^]]*\] //'
