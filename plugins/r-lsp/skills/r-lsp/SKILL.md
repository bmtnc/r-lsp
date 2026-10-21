---
name: r-lsp
description: Working in an R repository (.R, .Rmd, .qmd files, R packages, tidyverse/dplyr scripts, renv projects, Shiny, targets). How to use the R language server through the LSP tool, what each answer does and does not cover, how to check data-frame column names, and the plugin's shell tools (r-callmap, r-colcheck, r-lsp-status). Use whenever reading, navigating, refactoring or editing R code.
---

# R code intelligence in this repository

Three tools, three kinds of question. Pick by the shape of the question, not by habit:

| Question | Use | Why |
|---|---|---|
| How is this code put together? Where does execution start? What is dead? | `r-callmap` (shell) | one listing of every function with its callers and callees, entry points, unreferenced functions, cycles; about 100 tokens per function, read it instead of reading `R/` |
| What breaks if I change `f`? What must change to add an argument to `f`? Who ends up calling `f`, through which functions? | `r-callmap f` | callers up and callees down two levels (`--depth N`) with every call site as file:line, including `map(x, f)` `(as value)` and `do.call("f")` `(by string)`, which the server and `grep "f("` both miss |
| How does a value get from the loader to the output? | `r-callmap entry_point` | the callee chain in call order |
| Where does new code belong? Which helper already does the adjacent thing? | `r-callmap` | see who uses the candidates before extending one |
| Which callers of the functions in this diff need checking? | `r-callmap f g h` | several names in one call |
| Where is `f` used, when `f` is a common word (`fit`, `filter`, `summary`)? | LSP `findReferences` | the server returns the uses in scope where `grep` returns every line with the word |
| What does `pkg::f` take, what does it do? | LSP `hover` | signature and help page of an installed package's function in one call, without opening the package source |
| Where is `pkg::f` defined? | LSP `goToDefinition` | jumps into installed package source, which is not in the repository, so `grep` cannot find it |
| Where is `f` used, plain name? What is in this file? Which file mentions this string or comment? | `grep` / `Read` | one step, returns the code |

The LSP tool is deferred: load it once with `ToolSearch "select:LSP"`. Positions are 1-based; point at the symbol itself, not the bracket or comma before it. Its other operations: `incomingCalls`/`outgoingCalls` (direct calls only; functions passed as values are not edges, so prefer the map), `documentSymbol`, `workspaceSymbol` (every R file in the project, including tests and scripts).

Before any edit, read the file. The map is for the plan, not the edit: it does not show whether a call passes arguments by position or by name, and inserting an argument into a positional call silently shifts the others.

## Before a refactor

Run `r-callmap NAME` for the functions you will change, or `r-callmap` for the whole project when the change is wide or the codebase is new to you, before reading source files. Plan from the map, open only the files it names, then read each file you edit. After the edits, the post-edit diagnostics report a call to a function that no longer exists and the argument check reports calls whose argument names no longer match the definition. `--json` gives the same data for filtering (functions with no caller under `tests/`, for example).

The map reads the code without running it: calls built from strings at runtime (`eval(parse())`, pasted names), S4 dispatch, R6 calls through an object (`obj$m()`) and code in Rmd/qmd are not in it; only functions defined in the project appear, so calls into packages are absent by design. Git-ignored R files are included. A file that does not parse is named in the header as skipped.

## What to trust

- Answers that need the index (references, definition, hover, document and workspace symbols, call hierarchy) wait for it (up to ~30 s on first use), then they are complete for every `.R` file in the project (a package's `R/`, tests, scripts) and for files `source()`d from them. After each LSP result a plugin note lists the mentions `grep` finds in files the server does not index. Treat the server's list plus that note as the full set of call sites before a rename or signature change.
- "No references found" is reliable only after the note confirms nothing else was found; `r-lsp-status` shows whether indexing is still running (it shows the current server's events only, and says when the server it last heard from has exited).
- A script-level variable assigned inside `if`/`else` or a loop is scoped by the server to that block: references from one branch's assignment miss the other branch, and `goToDefinition` names one branch. The note recognizes the case and lists the other mentions in the file; take the union before a rename. A name that only appears as `name = ...` inside `mutate()`, `summarise()` or `tibble()` is a data-frame column, which the server does not track at all; the note says where it is created, and "No references found" means nothing for it.
- The server only sees files changed through Edit/Write. After `git checkout`, `styler`, `roxygen2::roxygenise()` or `sed`, results for those files are stale until you Read or edit them.
- Diagnostics after an edit come from two places. The language server reports syntax errors and, when the project has no `.lintr` of its own, the plugin's bug profile: undefined functions (anywhere) and undefined variables (inside function bodies only), non-exported `pkg::fun`, missing packages, deprecated, superseded or experimental tidyverse functions, and functions missing from the version renv pins. When the project has a `.lintr`, the server reports that file's rules instead (the session-start context says `Lint profile inactive`), and the post-edit hook runs the plugin's bug linters on the edited file and lists them in its note under `Diagnostics from the plugin's bug linters`. Wrong argument names in calls to the project's own functions come from `r-colcheck` in the same note; wrong argument names in calls to package functions are not reported by anything. Server diagnostics reach only the main conversation, not subagents; the hook's note reaches both. In a subagent without that note, run `Rscript -e 'parse("file.R")'` and `r-colcheck file.R` yourself after editing.
- Subagents share the one server (one per session) and can use the LSP tool after loading it with ToolSearch. `r-callmap` needs no loading: a subagent runs it through Bash, which it already has, so for callers and impact it is the first tool, not `grep`. Files in another repository are indexed on first open, so navigation works there too.

## Data-frame column names

Column names inside `mutate()`, `filter()`, `summarise()`, `select()`, `aes()` live in the data, not the code. Before writing tidyverse code against a dataset, get the real names cheaply:

- a CSV/TSV: `Rscript -e 'names(readr::read_csv("data/x.csv", n_max = 0))'`
- parquet: `Rscript -e 'names(arrow::open_dataset("data/x.parquet")$schema)'`
- a targets object: `Rscript -e 'names(targets::tar_read(x))'`
- an object in a running session: `names(df)` or `str(df)`

After editing a pipeline, `r-colcheck path/to/file.R` checks every bare column name against the data files the code reads (headers only, nothing loaded) and tracks names through the verbs, reporting at the line of the step that uses the name. Frames are followed through the project's own functions and through installed package functions whose output can be read from their code (a team's data package's cleaning functions, for instance); a note names a package function whose output could not be read. It is also run for you after each edit of an `.R` file; it stays silent when a step makes the column set unknowable (`across()`, `pivot_wider()`, joins to unknown frames, a frame passed through a package function). Run by hand, it ends with a summary on stderr (`N pipeline step(s) checked ... M skipped (columns unknowable)`), so silence with 0 steps checked means nothing was checkable, and it exits 1 when there are findings. The same run checks every call to a function defined in the project against its formals: an argument name the function does not have, one R would partial-match silently (`flow_metric =` for `flow_metrics`), or too many positional arguments. Functions with `...` and names a base package also defines are not checked. Frames passed to functions defined in the project (same file, `source()`d files, the package's `R/`) are followed into the body and back out, with `|>` and `%>%` alike. A cheap runtime check for anything it cannot see: run the pipeline on `head(df, 5)`.

### Data the checker cannot open: the schema cache

Data read from S3, a database or a project loader (`returns <- s3_read("returns/monthly")`) has no local file to open. For those, the checker uses `.colcheck/schemas.json`, a cache of observed column names keyed by the string the code passes to its loader (the S3 key, URI or dataset name). It matches any string literal in the loader call, or a `# cols: KEY` comment on the assignment line when the call carries no key. A cache hit is taken before the function's code is read (a loader whose columns depend on an argument can never be read from its body), also for a loader call inside a package function the checker is following; `# cols:` names the expression the line assigns, not a sub-expression. A helper whose output is fixed but whose calls carry no literal is cached by its name, `r-colcheck learn "pkg::fn" '<expr>'`, and every call then resolves; the note for such a function gives the command.

When a frame comes from a source that is not cached and the code then pipes it into verbs, the post-edit note says so and nothing downstream of it is checked:

```
R/report.R:17: 'holdings' comes from a source the checker cannot open (s3_read("holdings/latest")); no cached schema for key "holdings/latest" ... Learn it once: r-colcheck learn "holdings/latest" 's3_read("holdings/latest")'
```

Act on it when you are about to write or change code against that frame:

```
r-colcheck learn "holdings/latest" 's3_read("holdings/latest")' --source R/loaders.R
```

`learn` runs the expression once in the project (renv active, the shell's AWS credentials, no `--vanilla`) and records the names and types; arrow datasets give their schema without downloading rows, anything else is materialized once. The code is matched on the key, not on the expression, so when the project's loader materializes a whole table, learn from a cheaper expression that reads the same source (`r-colcheck learn "orders" 'arrow::open_dataset("s3://bucket/orders/")'`); the entry still matches every `fetch_table(date, "orders")` in the code. `--source FILE.R` loads the file that defines the loader; pass files of function definitions only, never a script that runs a pipeline or writes data. Run it from anywhere in the project. `r-colcheck schemas` lists what is cached and how old each entry is; `r-colcheck forget KEY` drops one.

Refresh an entry only on evidence: the check reports a column you are sure exists (the cache is stale, re-run `learn` for that key; when the entry is more than 90 days old the note says so next to the findings), or the user says the data changed. Never run `learn` from a hook or in a loop; it reaches the network.

A post-edit line saying `this file does not parse (LINE:COL: unexpected ...)` means the edit broke the file: fix the syntax first, nothing else in the file was checked. A line saying `r-colcheck hit an error ... and gave up on this statement` is a bug in the checker, not in the code: nothing downstream of that statement was checked, so fall back to reading the data source for that frame, and tell the user, with the statement, so it can be reported.

The cache is local by default: `.colcheck/` is created with its own `.gitignore`. Deleting `.colcheck/.gitignore` commits it so teammates share the entries.
