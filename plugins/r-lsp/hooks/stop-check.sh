#!/bin/bash
# Stop and SubagentStop hook: the safety net under the post-edit and post-bash
# hooks. An R file changed in this session that no check has seen (a hook
# that timed out, a file written by a background process) is checked now; if
# there are findings in the code, the turn is held once with them as the
# reason, so the code is not left for the user with a known error in it.
# Never holds twice (stop_hook_active), never speaks when the check is clean,
# walks nothing in a project remembered as having no R code.
input="$(cat)"
here="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}" 2>/dev/null || echo "${BASH_SOURCE[0]}")")" && pwd)"
. "$here/hook-lib.sh"
hooks_off && exit 0
printf '%s' "$input" | grep -qE '"stop_hook_active" *: *true' && exit 0
check_unchecked "$input" stop "" "$here"
exit 0
