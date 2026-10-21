# r-lsp

R language server for Claude Code, with renv support, tidyverse-aware diagnostics and a column-name check.

Gives Claude diagnostics after each edit, hover documentation, go-to-definition, find-references, workspace symbol search and call hierarchy for `.R`, `.Rmd` and `.qmd` files, plus `r-colcheck`, which checks the column names in tidyverse code against the data the code reads.

## Prerequisites

- R installed, with `R` and `Rscript` on the `PATH` of the shell you start Claude Code from.
- The [`languageserver`](https://github.com/REditorSupport/languageserver) R package installed globally:

```bash
Rscript --vanilla -e 'install.packages("languageserver", repos = "https://cloud.r-project.org")'
```

This also installs [`lintr`](https://github.com/r-lib/lintr), which produces the diagnostics. If diagnostics report `.onLoad failed in loadNamespace() for 'lintr'` naming a missing package (e.g. `there is no package called 'lazyeval'`), a transitive dependency is absent from your global library — install the named package the same way.

Plugin language servers run in terminal and desktop sessions, not in Claude Code cloud sessions.

## Installation

```
/plugin install r-lsp --marketplace bmtnc/r-lsp
```

Or in two steps on Claude Code versions before 2.1.275:

```
/plugin marketplace add bmtnc/r-lsp
/plugin install r-lsp
```

Then restart Claude Code or run `/reload-plugins`.

## Verify

Start a session in an R project. The first context lines should read `R LSP plugin: R package '...'` (or `R scripts project`) with file counts and the index cache state. Ask Claude to introduce and fix a small error in an `.R` file: the server starts on that first edit and the conversation shows `Found N new diagnostic issues`. `r-lsp-status` prints the server's index events. If something is missing, run `/plugin` and open the **Errors** tab.

## What Claude gets

- **Diagnostics after each edit**: syntax errors, undefined functions and variables, non-exported `pkg::fun`, missing packages, deprecated or superseded tidyverse functions, functions missing from the version renv pins. In projects without a `.lintr` file the plugin's profile (`lintr/agent_linters.R`) is used: likely bugs only, no style, and bare column names inside dplyr/tidyr/ggplot2 calls are not reported. Set `R_LSP_AGENT_LINTERS=false` to use lintr's defaults.
- **Navigation that is complete when it arrives**: references, definitions, workspace symbols and call hierarchy wait for the index (up to 30 s on first use, `R_LSP_INDEX_WAIT`). After each result a note lists the mentions `grep` finds in files the server does not index (`tests/`, scripts, Rmd) and says if the index is still building.
- **Column-name checks**: after each `.R` edit, `r-colcheck` reads only the headers of the data files the code reads and reports column names that cannot exist at that point of the pipeline. Also runnable by name: `r-colcheck path/to/file.R`.
- **Context at session start**: the project's shape and whether the index cache is warm; the cache is built in the background.
- **Index events**: "indexing" / "index ready" reach Claude through a plugin monitor; `r-lsp-status` prints them on demand.
- **A skill** that tells Claude when to use the LSP tool, what each answer covers, and how to get real column names for data the code does not read from a file.

## Working across multiple repositories

Claude Code runs one R language server per session, rooted at the directory you launched the session in. When Claude opens a file from another repository (or a directory added with `/add-dir`), the plugin adds that repository's root as a second workspace, indexes it and attaches its renv library, so definition, references and call hierarchy work there too.

Subagents share the one server and can use the LSP tool; pushed diagnostics reach only the main conversation, so the skill tells subagents to run `r-colcheck` and a parse check themselves after editing.

A large package costs memory in the server (about 1 GB for 100 R files). If you work in several large packages at once, one session per repository keeps that in check.

## Environment variables

Set in the shell that starts Claude Code: `R_LSP_AGENT_LINTERS=false` (lintr defaults instead of the profile), `R_LSP_INDEX_WAIT=<seconds>` (how long cross-file answers wait for indexing, default 30), `R_LSP_STATUS_FILE=<path>` (where index events are written; default `<plugin data dir>/status.log`).

See the repository README for how the pieces work, known limitations and troubleshooting.
