---
name: r-lsp
description: Working in an R repository (.R, .Rmd, .qmd files, R packages, tidyverse/dplyr scripts, renv projects, Shiny, targets). How to use the R language server through the LSP tool, what each answer does and does not cover, how to check data-frame column names, and the plugin's shell tools (r-colcheck, r-lsp-status). Use whenever reading, navigating, refactoring or editing R code.
---

# R code intelligence in this repository

The R language server runs for `.R`, `.Rmd` and `.qmd` files. It starts on the first edit of an R file. Load the LSP tool once with `ToolSearch "select:LSP"`, then prefer it over text search for these questions:

| Question | Operation | Covers |
|---|---|---|
| Where is `f` defined? | `goToDefinition` | package `R/`, open files, `source()`d files, installed packages (jumps to a temp copy of the package source) |
| Who calls `f`? | `findReferences`, `incomingCalls` | same scope; **not** `tests/`, scripts or Rmd |
| What does `f` call? | `outgoingCalls` | direct calls only; functions passed as values (`map(x, f)`, `do.call`) are not edges |
| What does `f` take? | `hover` | signature + roxygen for project functions; the full help page for package functions |
| What is in this file? | `documentSymbol` | top-level functions and variables |
| Find by name | `workspaceSymbol` | every R file in the project, including tests and scripts |

Positions are 1-based; point at the symbol itself, not the bracket or comma before it.

## What to trust

- Cross-file answers wait for indexing (up to ~30 s on first use), then they are complete for the scope above. After each LSP result a plugin note lists the mentions `grep` finds in files the server does not index. Treat the server's list plus that note as the full set of call sites before a rename or signature change.
- "No references found" is reliable only after the note confirms nothing else was found; `r-lsp-status` shows whether indexing is still running.
- The server only sees files changed through Edit/Write. After `git checkout`, `styler`, `roxygen2::roxygenise()` or `sed`, results for those files are stale until you Read or edit them.
- Diagnostics after an edit report syntax errors, undefined functions and variables, non-exported `pkg::fun`, missing packages, deprecated or superseded tidyverse functions, and functions missing from the version renv pins. Wrong argument names are not reported. Diagnostics reach only the main conversation, not subagents: in a subagent, run `Rscript -e 'parse("file.R")'` and `r-colcheck file.R` yourself after editing.
- Subagents share the one server (one per session) and can use the LSP tool. Files in another repository are indexed on first open, so navigation works there too.

## Data-frame column names

Column names inside `mutate()`, `filter()`, `summarise()`, `select()`, `aes()` live in the data, not the code. Before writing tidyverse code against a dataset, get the real names cheaply:

- a CSV/TSV: `Rscript -e 'names(readr::read_csv("data/x.csv", n_max = 0))'`
- parquet: `Rscript -e 'names(arrow::open_dataset("data/x.parquet")$schema)'`
- a targets object: `Rscript -e 'names(targets::tar_read(x))'`
- an object in a running session: `names(df)` or `str(df)`

After editing a pipeline, `r-colcheck path/to/file.R` checks every bare column name against the data files the code reads (headers only, nothing loaded) and tracks names through the verbs. It is also run for you after each edit of an `.R` file; it stays silent when a source cannot be resolved or a step makes the column set unknowable (`across()`, `pivot_wider()`, joins to unknown frames). A cheap runtime check for anything it cannot see: run the pipeline on `head(df, 5)`.
