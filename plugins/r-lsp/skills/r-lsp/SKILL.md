---
name: r-lsp
description: Working in an R repository (.R, .Rmd, .qmd files, R packages, tidyverse/dplyr scripts, renv projects, Shiny, targets). How to use the R language server through the LSP tool, what each answer does and does not cover, how to check data-frame column names, and the plugin's shell tools (r-callmap, r-colmap, r-colcheck, r-lsp-status). Use whenever reading, navigating, refactoring or editing R code. The r-refactor, r-review and r-data skills give the order of operations for those tasks.
---

# R code intelligence in this repository

Quick reference (every tool is on PATH; details follow):

- `r-callmap` (whole project), `r-callmap NAME` (callers and callees), `r-callmap --diff [REF]` (what a change touches, removed functions still mentioned, the tests to run), `r-callmap --tests NAME`, `r-callmap --file PATH`.
- `r-colmap` (every data source, its columns, where each is used, dropped, created or joined in; every function parameter used as a frame and the columns its body reads; a column no followed step reads says where its frame went), `r-colmap COLUMN`.
- `r-colcheck FILE` (column and argument names against the data), `--steps`, `discover` (uncached sources with their learn commands), `learn KEY 'EXPR'`, `learn --all`, `refresh`.
- `r-lint FILE` (the bug profile), `r-lsp-status` (index events), the LSP tool after `ToolSearch "select:LSP"`.
- After every Edit, Write or Bash change of an R file a note reports what the checks found; silence means checked and clean. Skills `r-refactor`, `r-review` and `r-data` give the order of operations for those tasks.

Three tools, three kinds of question. Pick by the shape of the question, not by habit:

| Question | Use | Why |
|---|---|---|
| How is this code put together? Where does execution start? What is dead? | `r-callmap` (shell) | one listing of every function with its callers and callees, entry points, unreferenced functions, cycles; about 100 tokens per function, read it instead of reading `R/` |
| What breaks if I change `f`? What must change to add an argument to `f`? Who ends up calling `f`, through which functions? | `r-callmap f` | callers up and callees down two levels (`--depth N`) with every call site as file:line, including `map(x, f)` `(as value)` and `do.call("f")` `(by string)`, which the server and `grep "f("` both miss |
| How does a value get from the loader to the output? | `r-callmap entry_point` | the callee chain in call order |
| Where does new code belong? Which helper already does the adjacent thing? | `r-callmap` | see who uses the candidates before extending one |
| Which callers of the functions in this diff need checking? Which tests cover them? | `r-callmap --diff` | the definitions the working tree changes against HEAD (or `--diff main`), with the kind of change, their callers, the removed functions still mentioned, and the test files that reach them with the command to run each; no names to supply |
| Which tests exercise `f`? | `r-callmap --tests f` | the test files that reach `f` through callers, with the chain and the command |
| Which columns does this dataset have, and where does the code use, drop, create or join each? Who else reads column `x`? | `r-colmap`, `r-colmap x` | every data source the code reads with its columns and the sites per column, then where the frame went; the data-side counterpart of the map |
| Which columns does the code create that nothing reads? What does `f` expect of the frame it is given? | `r-colmap`, then `r-colcheck --steps FILE` and `r-callmap f` | every unused verdict says where the frame was last seen (returned by `f()`, written by a sink, tracking ended, no later step carries it); the parameter section lists the columns each body reads; the header's step ratio says how much of the code the map saw |
| Where is `f` used, when `f` is a common word (`fit`, `filter`, `summary`)? | LSP `findReferences` | the server returns the uses in scope where `grep` returns every line with the word |
| What does `pkg::f` take, what does it do? | LSP `hover` | signature and help page of an installed package's function in one call, without opening the package source |
| Where is `pkg::f` defined? | LSP `goToDefinition` | jumps into installed package source, which is not in the repository, so `grep` cannot find it |
| Where is `f` used, plain name? What is in this file? Which file mentions this string or comment? | `grep` / `Read` | one step, returns the code |

The LSP tool is deferred: load it once with `ToolSearch "select:LSP"`. Positions are 1-based; point at the symbol itself, not the bracket or comma before it. Its other operations: `incomingCalls`/`outgoingCalls` (direct calls only; functions passed as values are not edges, so prefer the map), `documentSymbol`, `workspaceSymbol` (every R file in the project, including tests and scripts).

Before any edit, read the file. The map is for the plan, not the edit: it does not show whether a call passes arguments by position or by name, and inserting an argument into a positional call silently shifts the others.

## Before a refactor

Run `r-callmap NAME` for the functions you will change, or `r-callmap` for the whole project when the change is wide or the codebase is new to you, before reading source files. Plan from the map, open only the files it names, then read each file you edit. After the edits, the post-edit diagnostics report a call to a function that no longer exists and the argument check reports calls whose argument names no longer match the definition; `r-callmap --diff` then lists everything the change touches, the removed definitions still mentioned anywhere, and the tests to run (`--tests NAME` for one function). `--json` gives the same data for filtering (functions with no caller under `tests/`, for example). The `r-refactor` skill is this procedure in full; `r-review` is the one for reviewing a change.

The map reads the code without running it: calls built from strings at runtime (`eval(parse())`, names computed at run time), S4 dispatch, R6 calls on an object not made by `Class$new()` in the same function, functions held in a list other than by name, and inline `r` code in Rmd/qmd prose are not in it (the chunks are, each read as knitr runs it, and a vignette counts as a script); aliases (`f2 <- f`, `memoise(f)`, `helpers$f()`), `Class$new()` and `obj$m()` on an object made in the same function are edges. Only functions defined in the project appear, so calls into packages are absent by design; a function defined in a script or test file is private to it (tagged `[private to this file]`). Git-ignored R files are included. A file, or a chunk, that does not parse is named in the header as skipped. Exit 2 means the root was wrong (no R file under it): re-run with `--root DIR`.

## What to trust

- Answers that need the index (references, definition, hover, document and workspace symbols, call hierarchy) wait for it (up to ~30 s on first use), then they are complete for every `.R`, `.Rmd` and `.qmd` file in the project (a package's `R/`, tests, scripts, vignettes) and for files `source()`d from them. After a findReferences, incomingCalls or goToDefinition result a plugin note lists the mentions `grep` finds that the answer did not (roxygen links, comments, Rmd/qmd prose, files the server did not load); for outgoingCalls, `r-callmap NAME` is the complete callee list. Treat the server's list plus that note as the full set of call sites before a rename or signature change.
- "No references found" is reliable only after the note confirms nothing else was found; `r-lsp-status` shows whether indexing is still running (it shows the current server's events only, and says when the server it last heard from has exited).
- A script-level variable assigned inside `if`/`else` or a loop is scoped by the server to that block: references from one branch's assignment miss the other branch, and `goToDefinition` names one branch. The note recognizes the case and lists the other mentions in the file; take the union before a rename. A name that only appears as `name = ...` inside `mutate()`, `summarise()` or `tibble()` is a data-frame column, which the server does not track at all; the note says where it is created, and "No references found" means nothing for it.
- Before a references, definition, hover, call-hierarchy or symbol request the server re-reads every loaded file that changed on disk since it was read (a `git checkout`, `styler`, `roxygen2::roxygenise()`, `sed`, a heredoc), so those answers are current; a new file is found once something opens it, and the post-LSP note's grep lists callers in it meanwhile. The changes themselves are checked too: after a Bash command the plugin's note checks the R files it changed (column and argument names, parse errors, the bug linters), up to ten per command, and names the rest as unchecked.
- Diagnostics after an edit come from two places. The language server reports syntax errors and, when the project has no `.lintr` of its own, the plugin's bug profile: undefined functions (anywhere) and undefined variables (inside function bodies only), non-exported `pkg::fun`, missing packages, deprecated, superseded or experimental tidyverse functions, and functions missing from the version renv pins. When the project has a `.lintr`, the server reports that file's rules instead (the session-start context says `Lint profile inactive`), and the post-edit hook runs the plugin's bug linters on the edited file and lists them in its note under `Diagnostics from the plugin's bug linters`. Wrong argument names in calls to the project's own functions come from `r-colcheck` in the same note; wrong argument names in calls to package functions are not reported by anything. Server diagnostics reach only the main conversation, not subagents; the hook's note reaches both, and inside a subagent it carries the bug linters whatever the lint config. Silence after an edit means checked and clean. To run the same linters by hand, on any file, `r-lint FILE...` (exit 1 with findings); `r-colcheck FILE...` for the column and argument checks. At the end of a turn, an R file changed in the session that no check has seen is checked and the turn held once with the findings; address them or say why they do not apply, then stop again.
- Subagents share the one server (one per session) and can use the LSP tool after loading it with ToolSearch. `r-callmap` needs no loading: a subagent runs it through Bash, which it already has, so for callers and impact it is the first tool, not `grep`. Files in another repository are indexed on first open, so navigation works there too.

## Data-frame column names

Column names inside `mutate()`, `filter()`, `summarise()`, `select()`, `aes()` live in the data, not the code. Before writing tidyverse code against a dataset, get the real names cheaply:

- a CSV/TSV: `Rscript -e 'names(readr::read_csv("data/x.csv", n_max = 0))'`
- parquet: `Rscript -e 'names(arrow::open_dataset("data/x.parquet")$schema)'`
- a targets object: `Rscript -e 'names(targets::tar_read(x))'`
- an object in a running session: `names(df)` or `str(df)`

### The check after an edit

After editing a pipeline, `r-colcheck path/to/file.R` checks every bare column name against the data files the code reads (headers only, nothing loaded) and tracks names through the verbs, reporting at the line of the step that uses the name. It is also run for you after each edit of an `.R`, `.Rmd` or `.qmd` file (the chunks are checked, each parsed as knitr runs it); it stays silent when a step makes the column set unknowable. Run by hand, it ends with a summary on stderr that leads with coverage (`N of M pipeline step(s) checked ...; K not checked (a on partly known columns, b on unknown)`), so 0 of 40 means nothing was checkable, and it exits 1 when there are findings; `--help` prints usage, and an unknown option, subcommand or file exits 2 with usage. The same run checks every call to a function defined in the project against its formals: an argument name the function does not have, one R would partial-match silently (`flow_metric =` for `flow_metrics`), or too many positional arguments. Functions with `...` and names a base package also defines are not checked. A cheap runtime check for anything it cannot see: run the pipeline on `head(df, 5)`.

### Three states of a frame

Every frame the checker meets is in one of three states, and `r-colcheck --steps FILE` prints the state after every pipeline step (`--json --steps` for `{findings, steps}`), the lineage a review can read:

- **known** (`region, year, amount`): the exact column set. Names are checked against it; what a step adds and drops is recorded.
- **partly known** (`at least region, year`): these columns and maybe more. A parameter of a named function that the body pipes into a verb, a frame bound with an unknown one, a frame an assertion described. Nothing is reported against it; the names the code reads are recorded for the map; it closes to a known set at the first `select()`, `transmute()`, `summarise()`, `distinct()` or `count()`, or at an exact assertion.
- **unknown** (`(unknown)`): nothing downstream is checked. A note names the step where a known frame became unknown, also when that step is inside a project helper the checker followed (`inside to_utc() at its mutate(across(...))`), for piped and direct calls alike, with the `learn` command that covers it; the map calls it "tracking ended".

`--steps` also shows where a frame went: `returned by f(): ...` after the last expression of a named function, `written by saveRDS(): ...` at a sink. A list of frames (after `split()`) shows as `each: a, b`.

### How a frame becomes known

- **A loader with a file it can open** (`read_csv("data/x.csv")`, parquet, excel, `datasets::mtcars`, a `tibble()` literal). The reader's arguments count (`col_select`, `col_names = FALSE`, `skip`, `sheet`, `read.csv()` name repair). Anything else goes through the schema cache below.
- **Through the project's own functions.** Frames passed to functions defined in the project (same file, `source()`d files, the package's `R/`) are followed into the body and back out, with `|>` and `%>%` alike, and the result's columns continue in the caller. A named function's body is also walked on its own, with each parameter the body uses as a frame partly known, so the steps inside every function are followed even when no caller is.
- **Through installed package functions** whose output can be read from their code (a team's data package's cleaning functions, for instance); a note names a package function whose output could not be read.
- **Split, map, bind.** A frame split into a list (`split(df, df$g)`, `df %>% split(.$g)`, `group_split()`) and mapped over (`map()`, `imap()`, `lapply()`, `map_dfr()`) is followed: the lambda (`~ .x %>% ...`, `function(d) ...`, or a named function) is checked once with its parameter bound to the frame's columns, findings land at the lambda's own lines, `.y` is a name, and `bind_rows()`/`list_rbind()`/`map_dfr()` give the lambda's result columns; a lambda whose result cannot be worked out ends tracking with a note naming the step inside it. Row binding is followed: `bind_rows(a, b)`, `a %>% bind_rows(b)`, `list(a, b) %>% bind_rows()`, `rbind()`, `list_rbind()` give the union of the inputs' columns (first input's grouping and source, `.id` adds its column); an unknown input among known ones leaves the result partly known, a binding whose inputs are all unknown stays unknown without a note, and a list held in a variable is not followed. `bind_cols()` stays unknown.
- **An assertion.** A call that stops unless a frame has certain columns states what the frame has from that line on: `stopifnot(all(cols %in% names(df)))`, `if (!all(cols %in% names(df))) stop()`, checkmate's `assert_names(names(df), must.include = cols)`, or a team's own validator named in `.colcheck.json` at the project root (`{"assertions": [{"fn": "teamdata::require_columns", "frame": 1, "columns": 2, "exact": "strict"}]}`, committed with the code). On a known frame the required names are checked. On an unknown or partly known one they become its columns; an exact assertion closes it at once; inside `if (all(cols %in% names(df))) { ... }` the frame has those columns for the branch only. The column vector may be a literal, a variable bound to one in the same scope, a `switch()` on a literal, or a project or package function returning one.

### What counts as a use, and what adds or drops a column

- Tidyselect helpers are expanded against the known columns in every selection (`select(starts_with("amt"))`, `select(-contains("tmp"))`, `across(ends_with("_usd"), mean)`, `matches()`, `any_of()`, `everything()`, `last_col()`; `where()` stays unresolved). `all_of(cols)` resolves when `cols` is a vector the code states (a literal bound earlier, a `switch()`, a helper returning one), in every verb. `select(-x)` reads `x` and drops it; `select(everything())` reads every column.
- `mutate(across(cols, f))` keeps the column set (a single function and no `.names` rewrites the selected columns in place, whatever the selection); with `.names` or a `list()` of functions the new names are expanded from the template, and `transmute()`/`summarise()` keep the selected columns when the selection is readable.
- A bare name passed as an argument inside an expression that names a function in scope (`slide_dbl(x, sd)`, `sapply(x, nchar)`, a project helper) is a function value, not a column; as an operand (`filter(sd > 0)`) it is checked.
- Base-R access on a known frame is checked (`df$x`, `df[["x"]]`, `.data$x`) and `df$x <- v` adds the column. Joins carry the `.x`/`.y` suffixes and drop the right-hand key as dplyr does, and the other frame's columns are joined in, not created. `summarise()` keeps all but the last grouping column, and a frame reassigned inside `if`/`for` has afterwards the columns of either outcome.
- `nest(data = -id)`, `nest(.by = id)` and `nest()` on a grouped frame are followed: the nested columns are read and leave the frame, the list-column arrives. `deframe()` reads its first two columns. `pull()` reads its column.
- The superseded scoped verbs (`mutate_if/at/all`, `filter_at`, `arrange_all`, ...) and the tibble and tidyr helpers (`replace_na`, `na.omit`, `rownames_to_column`, `add_column`, `unite`, `setNames`, `rename_with(toupper)`, `complete`, `separate_rows`, `add_row`) are followed.
- A sink reads nothing by name but is where the frame went: `saveRDS()`, `write_csv()`, `write_parquet()`, `write_dataset()`, `fwrite()`, `dbWriteTable()`, `s3write_using()` and the rest, direct or piped. The frame keeps its columns past a readr writer.

### What ends tracking

`pivot_wider()` with names from the data, a join whose `by`, `suffix` or `keep` the checker cannot read or to an unknown frame, `nest()` with both `.by` and a selection or on an ungrouped frame without groups, `bind_cols()`, `where()` and `all_of(variable)` in a selection on a partly known frame, a `list()` of functions in `mutate_*`, `select_if`/`select_at`, `group_modify`, `nest_by`, `rename_with` with a function other than `toupper`/`tolower`, an `unnest()` of a list-column whose inner columns are not recorded, a package function whose code could not be read, a lambda whose result cannot be worked out. The note names the step and the line; `# cols: KEY` on the statement covers what follows (see the cache below).

### The data map

`r-colmap` lists every data source the code reads with its columns and, per column, where the code uses, drops, creates or joins it in (file:line and verb); then every function parameter a body uses as a frame, with the columns the body reads of it and adds to it, which is what the function expects of the frame it is given. `r-colmap COLUMN` lists every site touching one column, in any source, and where the frame then went. `--file PATH` limits the files, `--json` gives the data.

Read the header first: `N of M pipeline steps followed`. Every verdict is about the steps followed, not the codebase. A low ratio means sources the checker cannot open: `r-colcheck discover`, learn them, read the map again.

A column marked `no use seen in the steps followed` is not dead until the rest of its line is read; it says where the frame carrying it was last seen:

- `returned by f() at R/a.R:12`: the callers of `f` have it; `r-callmap f` lists them. When the checker followed a caller from a known frame, the line says `used N times` instead.
- `written by saveRDS() at R/a.R:14`: the column reached a file, a database or a bucket; that was its purpose.
- `tracking ended at R/a.R:9 (pivot_wider)`: the checker lost the frame there; `r-colcheck --steps R/a.R` shows the step, and `# cols: KEY` on that statement covers what follows.
- `no later step carries it`: the frame's last followed step did not keep it, and nothing returned or wrote it. The one candidate for removal, still to be confirmed by `grep` for the name as a string (`df[["x"]]`, `all_of(var)`, a dashboard outside the code).

An annotated listing:

```
sales.csv   first used at R/load.R:4   4 columns: region, year, amount, customer    (a source: where it is read, what it has)
  region               used 3 times: R/a.R:10 (filter), R/a.R:23 (summarise), ...; dropped at R/a.R:23 (summarise)
  customer             never named by the code; written by write_csv() at R/a.R:30  (carried along and written out, never read by name)
  created by the code:
  total                at R/a.R:10 (summarise); used 1 time
  big                  at R/a.R:12 (mutate); no use seen in the steps followed; returned by flag_big() at R/a.R:13   (the callers decide)
  joined from users.csv at R/a.R:16 (left_join):
  last_name            no use seen in the steps followed; written by saveRDS() at R/a.R:18   (join payload that reached a sink)

users.csv   first used at R/load.R:7   2 columns: id, last_name
  id                   used 1 time: R/a.R:16 (left_join)
  last_name            carried into sales.csv at R/a.R:16 (left_join)

Function parameters used as frames (...):

parameter df of flag_big()   at R/a.R:12   reads 1 column: amount    (what the body expects of the frame it is given)
  amount               used 1 time: R/a.R:12 (mutate)
  created by the code:
  big                  at R/a.R:12 (mutate); no use seen in the steps followed; returned by flag_big() at R/a.R:13
```

The order of operations for "which columns does nothing read?": `r-colmap`; check the header ratio; take the created columns whose line ends in `no later step carries it`; `r-colcheck --steps FILE` for each, to see the step; `r-callmap f` for any `returned by f()`; then `grep` for the name as a string.

### Data the checker cannot open: the schema cache

Data read from S3, a database or a project loader (`returns <- s3_read("returns/monthly")`) has no local file to open. For those, the checker uses `.colcheck/schemas.json`, a cache of observed column names keyed by the string the code passes to its loader (the S3 key, URI or dataset name). It matches any string literal in the loader call, or a `# cols: KEY` comment on the assignment line when the call carries no key. A cache hit is taken before the function's code is read (a loader whose columns depend on an argument can never be read from its body), also for a loader call inside a package function the checker is following; `# cols:` names the expression the line assigns, not a sub-expression, and overrides what the checker inferred for that line when no source consumed it. A helper whose output is fixed but whose calls carry no literal is cached by its name, `r-colcheck learn "pkg::fn" '<expr>'`, and every call then resolves, piped (`df %>% clean()`) or direct; the note for such a function gives the command. A helper whose output depends on an argument gets one entry per value, keyed on the call: `r-colcheck learn 'prepare(type = "monthly")' '<expr>'` matches every call that carries `type = "monthly"` (a positional literal in the key matches it anywhere among the call's arguments), and beats the bare function key. No comment in the code is needed for either. A whole statement whose steps cannot be followed (`split()` into groups, a `map()` over them, `bind_rows()`) is covered by `# cols: KEY` on its first line, learned from the same pipeline; in a package, `learn ... --load-all` loads the package with pkgload and evaluates the expression inside its namespace, so its functions and imports resolve with no `--source` files and no `library()` calls. A literal inside a lambda (`~ .x %>% filter(status == "open")`) is never a cache key. A list-column of data frames is followed through `unnest()`/`hoist()` when its inner columns are recorded, which `learn` does from the first non-empty element; a note saying the inner columns are not recorded means re-run `learn` for that source.

When a frame comes from a source that is not cached and the code then pipes it into verbs, the post-edit note says so and nothing downstream of it is checked:

```
R/report.R:17: 'holdings' comes from a source the checker cannot open (s3_read("holdings/latest")); no cached schema for key "holdings/latest" ... Learn it once: r-colcheck learn "holdings/latest" 's3_read("holdings/latest")'
```

Act on it when you are about to write or change code against that frame:

```
r-colcheck learn "holdings/latest" 's3_read("holdings/latest")' --source R/loaders.R
```

`learn` runs the expression once in the project (renv active, the shell's AWS credentials, no `--vanilla`) and records the names and types; arrow datasets give their schema without downloading rows, anything else is materialized once. The code is matched on the key, not on the expression, so when the project's loader materializes a whole table, learn from a cheaper expression that reads the same source (`r-colcheck learn "orders" 'arrow::open_dataset("s3://bucket/orders/")'`); the entry still matches every `fetch_table(date, "orders")` in the code. `--source FILE.R` loads the file that defines the loader; pass files of function definitions only, never a script that runs a pipeline or writes data. Run it from anywhere in the project. `r-colcheck schemas` lists what is cached and how old each entry is; `r-colcheck forget KEY` drops one. `r-colcheck refresh [--stale] [KEY ...]` re-runs the stored expressions with the `--load-all` and `--source` flags they were learned with, all of them, the stale ones, or the keys given, reporting each as changed, unchanged or failed and going on past a failure.

Refresh on evidence: the check reports a column you are sure exists (the cache is stale: `r-colcheck refresh KEY`, or `learn` again; when the entry is older than `schema_ttl_days` in `.colcheck.json`, 90 by default, the note says so next to the findings), or the user says the data changed (`r-colcheck refresh`, or `--stale`). Never run `learn` or `refresh` from a hook or in a loop; they reach the network.

For the whole project at once: `r-colcheck discover` lists every source the code uses that the checker cannot open, where it is used and the command to learn it, plus the cached entries with their age and where their key is mentioned; `r-colcheck learn --all` runs those commands in one go (`--dry-run` prints them first). A parquet or arrow dataset on a bucket is learned from `arrow::open_dataset(uri)`, which reads the footer and no rows; the note already suggests it. A team loader gets a `loaders` entry in `.colcheck.json` (`{"loaders": [{"fn": "teamdata::fetch_table", "key": "name", "learn": "arrow::open_dataset(sprintf('s3://bucket/%s/', KEY))", "load_all": true}]}`): the key argument is then the cache key, the note gives the derived command, and `discover` derives one per key. `r-colmap` shows what the project already knows: each source's columns and where the code uses, drops or creates them. The `r-data` skill is this procedure in full.

A post-edit line saying `this file does not parse (LINE:COL: unexpected ...)` means the edit broke the file: fix the syntax first, nothing else in the file was checked. A line saying `r-colcheck hit an error ... and gave up on this statement` is a bug in the checker, not in the code: nothing downstream of that statement was checked, so fall back to reading the data source for that frame, and tell the user, with the statement, so it can be reported.

The cache is local by default: `.colcheck/` is created with its own `.gitignore`. Deleting `.colcheck/.gitignore` commits it so teammates share the entries.
