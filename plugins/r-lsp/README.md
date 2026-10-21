# r-lsp

R language server for Claude Code, with renv support, tidyverse-aware diagnostics and a column-name check.

Gives Claude diagnostics after each edit, hover documentation, go-to-definition, find-references, workspace symbol search and call hierarchy for `.R`, `.Rmd` and `.qmd` files, across a package's `R/`, its tests and its scripts (all loaded at startup), plus `r-colcheck`, which checks the column names in tidyverse code against the data the code reads, and `r-callmap`, which prints the project's whole call map at once.

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

Start a session in an R project. The first context lines should read `R LSP plugin: R package '...'` (or `R scripts project`) with file counts and the index cache state. Ask Claude to introduce and fix a small error in an `.R` file: the server starts on that first edit (or on the first LSP call) and the conversation shows `Found N new diagnostic issues`. `r-lsp-status` prints the current server's index events. If something is missing, run `/plugin` and open the **Errors** tab.

## What Claude gets

- **Diagnostics after each edit**: syntax errors, undefined functions (anywhere) and undefined variables (inside function bodies), non-exported `pkg::fun`, missing packages, deprecated, superseded or experimental tidyverse functions, functions missing from the version renv pins. In projects without a `.lintr` file the plugin's profile (`lintr/agent_linters.R`) is used: likely bugs only, no style, and bare column names inside dplyr/tidyr/ggplot2 calls are not reported. In projects with one, that file's rules are the server's diagnostics, and the plugin's bug linters run from the post-edit hook instead (reported in its note, in subagents too); the session-start context says which case applies. Set `R_LSP_AGENT_LINTERS=false` to use lintr's defaults.
- **A monitor only where it matters**: the index monitor (the "1 monitor" line) starts only in a directory with R code; elsewhere it exits at once.
- **Navigation that is complete when it arrives**: references, definitions, hover, document and workspace symbols and call hierarchy wait for the index (up to 30 s on first use, `R_LSP_INDEX_WAIT`) and cover every `.R` file in the project. After each result a note lists the mentions `grep` finds that the answer did not (roxygen links, Rmd, calls through `map()`/`do.call()`, the other branches of a script variable assigned inside `if`/`else`), points out names that are data-frame columns rather than variables, and says if the index is still building.
- **Column-name and argument checks**: after each `.R` edit, `r-colcheck` reads only the headers of the data files the code reads and reports column names that cannot exist at that point of the pipeline, following frames through the project's own functions (same file, `source()`d files, a package's `R/`) and through installed package functions whose output can be read from their code (a team's own cleaning functions over a database or API), and both pipes. It also reports calls to the project's own functions with an argument name the definition does not have, one R would partial-match silently, or too many arguments. Also runnable by name: `r-colcheck path/to/file.R` (exit 1 on findings; a summary of steps checked and skipped on stderr). Data it cannot open (S3 objects, databases, project loaders) is checked through a schema cache, `.colcheck/schemas.json`, keyed by the string the code passes to its loader: `r-colcheck learn KEY 'EXPR' [--source FILE.R]` evaluates the loader once in the project and records the column names; the post-edit note says which frames still need it. A cache hit is taken before a function's code is read, for piped and direct calls; `# cols: KEY` names the expression the line assigns, an entry keyed on a function name (`learn "pkg::fn"`) covers every call to a helper with a fixed output, and one keyed on a call (`learn 'fn(type = "monthly")'`) covers the calls carrying that argument. `mutate(across(cols, f))` keeps the column set; a step that loses it is named in a note, also when it sits inside a project helper the checker followed. `--steps` lists the column set after every pipeline step. A file that does not parse is reported with the parser's line and message. A failure of the checker itself is reported on the statement it dropped, never swallowed. The cache is gitignored by default (its own `.gitignore`) and committable by deleting that file.
- **A call map in one listing**: `r-callmap` prints every function in the project with its file:line and arguments, the project functions it calls and what calls it (functions, tests, scripts), then the entry points, the functions nobody calls and any recursion. `r-callmap NAME` shows one function's callers and callees two levels each way with each call site. Functions passed as values (`map(x, f)`) and by name (`do.call("f")`) are edges, tagged as such. The LSP tool gives the call hierarchy one function at a time; this lets Claude plan a refactor from one listing and open only the files it names. It parses the code without running it, so calls built from strings at runtime, S4 dispatch, R6 calls through an object and Rmd/qmd code are not in it. `--json` for the same data as JSON.
- **Context at session start**: the project's shape and whether the index cache is warm; the cache is built in the background.
- **Index events**: "index ready" and "added workspace" reach Claude through a plugin monitor, two or three lines in a typical session; `r-lsp-status` prints the full log on demand.
- **A skill** that tells Claude when to use the LSP tool, what each answer covers, and how to get real column names for data the code does not read from a file.

## Working across multiple repositories

Claude Code runs one R language server per session, rooted at the directory you launched the session in. When Claude opens a file from another repository (or a directory added with `/add-dir`), the plugin adds that repository's root as a second workspace, indexes it and attaches its renv library, so definition, references and call hierarchy work there too.

Subagents share the one server and can use the LSP tool; pushed diagnostics reach only the main conversation, so the skill tells subagents to run `r-colcheck` and a parse check themselves after editing.

A large package costs memory in the server (about 1 GB for 100 R files). If you work in several large packages at once, one session per repository keeps that in check.

## Environment variables

Set in the shell that starts Claude Code: `R_LSP_AGENT_LINTERS=false` (lintr defaults instead of the profile), `R_LSP_INDEX_WAIT=<seconds>` (how long cross-file answers wait for indexing, default 30), `R_LSP_STATUS_FILE=<path>` (where index events are written; default `<plugin data dir>/status.log`).

See the repository README for how the pieces work, known limitations and troubleshooting.
