---
name: r-lsp
description: Working in an R repository (.R, .Rmd, .qmd files, R packages, tidyverse/dplyr scripts, renv projects, Shiny, targets). How to use the R language server through the LSP tool, what each answer does and does not cover, how to check data-frame column names, and the plugin's shell tools (r-colcheck, r-lsp-status). Use whenever reading, navigating, refactoring or editing R code.
---

# R code intelligence in this repository

The R language server runs for `.R`, `.Rmd` and `.qmd` files. It starts on the first LSP call or edit that touches an R file. Load the LSP tool once with `ToolSearch "select:LSP"`, then prefer it over text search for these questions:

| Question | Operation | Covers |
|---|---|---|
| Where is `f` defined? | `goToDefinition` | package `R/`, open files, `source()`d files, installed packages (jumps to a temp copy of the package source); works from scripts and tests into `R/` |
| Who calls `f`? | `findReferences`, `incomingCalls` | same scope plus every `.R` file in the project (tests, scripts), loaded at startup; not Rmd/Quarto, roxygen `[f()]` links or calls through `map()`/`do.call()`, so read the plugin note that follows: it lists every mention `grep` finds that the server did not return, line by line, in returned files too |
| What does `f` call? | `outgoingCalls` | direct calls only; functions passed as values (`map(x, f)`, `do.call`) are not edges |
| What does `f` take? | `hover` | signature + roxygen for project functions; the full help page for package functions |
| What is in this file? | `documentSymbol` | top-level functions and variables |
| Find by name | `workspaceSymbol` | every R file in the project, including tests and scripts |

Positions are 1-based; point at the symbol itself, not the bracket or comma before it.

## What to trust

- Cross-file answers wait for indexing (up to ~30 s on first use), then they are complete for the scope above. After each LSP result a plugin note lists the mentions `grep` finds in files the server does not index. Treat the server's list plus that note as the full set of call sites before a rename or signature change.
- "No references found" is reliable only after the note confirms nothing else was found; `r-lsp-status` shows whether indexing is still running (it shows the current server's events only, and says when the server it last heard from has exited).
- A script-level variable assigned inside `if`/`else` or a loop is scoped by the server to that block: references from one branch's assignment miss the other branch, and `goToDefinition` names one branch. The note recognizes the case and lists the other mentions in the file; take the union before a rename. A name that only appears as `name = ...` inside `mutate()`, `summarise()` or `tibble()` is a data-frame column, which the server does not track at all; the note says where it is created, and "No references found" means nothing for it.
- The server only sees files changed through Edit/Write. After `git checkout`, `styler`, `roxygen2::roxygenise()` or `sed`, results for those files are stale until you Read or edit them.
- Diagnostics after an edit report syntax errors, undefined functions (anywhere) and undefined variables (inside function bodies only), non-exported `pkg::fun`, missing packages, deprecated or superseded tidyverse functions, and functions missing from the version renv pins. Wrong argument names in calls to the project's own functions come from `r-colcheck` after the edit (see below); wrong argument names in calls to package functions are not reported by anything. Diagnostics reach only the main conversation, not subagents: in a subagent, run `Rscript -e 'parse("file.R")'` and `r-colcheck file.R` yourself after editing.
- Subagents share the one server (one per session) and can use the LSP tool. Files in another repository are indexed on first open, so navigation works there too.

## Data-frame column names

Column names inside `mutate()`, `filter()`, `summarise()`, `select()`, `aes()` live in the data, not the code. Before writing tidyverse code against a dataset, get the real names cheaply:

- a CSV/TSV: `Rscript -e 'names(readr::read_csv("data/x.csv", n_max = 0))'`
- parquet: `Rscript -e 'names(arrow::open_dataset("data/x.parquet")$schema)'`
- a targets object: `Rscript -e 'names(targets::tar_read(x))'`
- an object in a running session: `names(df)` or `str(df)`

After editing a pipeline, `r-colcheck path/to/file.R` checks every bare column name against the data files the code reads (headers only, nothing loaded) and tracks names through the verbs, reporting at the line of the step that uses the name. It is also run for you after each edit of an `.R` file; it stays silent when a step makes the column set unknowable (`across()`, `pivot_wider()`, joins to unknown frames, a frame passed through a package function). Run by hand, it ends with a summary on stderr (`N pipeline step(s) checked ... M skipped (columns unknowable)`), so silence with 0 steps checked means nothing was checkable, and it exits 1 when there are findings. The same run checks every call to a function defined in the project against its formals: an argument name the function does not have, one R would partial-match silently (`flow_metric =` for `flow_metrics`), or too many positional arguments. Functions with `...` and names a base package also defines are not checked. Frames passed to functions defined in the project (same file, `source()`d files, the package's `R/`) are followed into the body and back out, with `|>` and `%>%` alike. A cheap runtime check for anything it cannot see: run the pipeline on `head(df, 5)`.

### Data the checker cannot open: the schema cache

Data read from S3, a database or a project loader (`returns <- s3_read("returns/monthly")`) has no local file to open. For those, the checker uses `.colcheck/schemas.json`, a cache of observed column names keyed by the string the code passes to its loader (the S3 key, URI or dataset name). It matches any string literal in the loader call, or a `# cols: KEY` comment on the assignment line when the call carries no key.

When a frame comes from a source that is not cached and the code then pipes it into verbs, the post-edit note says so and nothing downstream of it is checked:

```
R/report.R:17: 'holdings' comes from a source the checker cannot open (s3_read("holdings/latest")); no cached schema for key "holdings/latest" ... Learn it once: r-colcheck learn "holdings/latest" 's3_read("holdings/latest")'
```

Act on it when you are about to write or change code against that frame:

```
r-colcheck learn "holdings/latest" 's3_read("holdings/latest")' --source R/loaders.R
```

`learn` runs the expression once in the project (renv active, the shell's AWS credentials, no `--vanilla`) and records the names and types; arrow datasets give their schema without downloading rows, anything else is materialized once. `--source FILE.R` loads the file that defines the loader; pass files of function definitions only, never a script that runs a pipeline or writes data. Run it from anywhere in the project. `r-colcheck schemas` lists what is cached and how old each entry is; `r-colcheck forget KEY` drops one.

Refresh an entry only on evidence: the check reports a column you are sure exists (the cache is stale, re-run `learn` for that key; when the entry is more than 90 days old the note says so next to the findings), or the user says the data changed. Never run `learn` from a hook or in a loop; it reaches the network.

The cache is local by default: `.colcheck/` is created with its own `.gitignore`. Deleting `.colcheck/.gitignore` commits it so teammates share the entries.
