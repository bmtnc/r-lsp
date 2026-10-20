#!/bin/bash
# PostToolUse hook for the LSP tool: add what the language server's answer
# does not cover (index still building; mentions in files it does not index).
input="$(cat)"
command -v Rscript >/dev/null 2>&1 || exit 0
here="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}" 2>/dev/null || echo "${BASH_SOURCE[0]}")")" && pwd)"
printf '%s' "$input" | Rscript --vanilla "$here/../scripts/hook-post-lsp.R" 2>/dev/null
exit 0
