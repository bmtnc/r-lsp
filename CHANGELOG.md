# Changelog

One entry per release, newest first. The version is the one in `plugins/r-lsp/.claude-plugin/plugin.json` and `.claude-plugin/marketplace.json`; each release is a commit whose subject starts with the version, tagged `vX.Y.Z` by the maintainer (a cloud session cannot push tags). The commit message carries the full account; this file carries the headline.

## 1.9.0 (2026-11-01)

Dormant outside R projects, a data map that knows lineage, what the code writes and what a diff touches, data followed across scripts and targets, logic findings, and the sweep's fixes.
- Hooks: a directory without R code costs one walk at the first session start and nothing after (the answer is remembered for the session and, for a day, for the directory; a resumed session, a compaction, a subagent and the monitor trust it, the Bash, Stop and LSP hooks for ten minutes at a time; a new session, a fork, `/clear`, an edit of an R file under the root, or a command that could have brought R code in look again; a marker says which directory it is about). The LSP hook runs only for R files. `R_LSP_HOOKS=off` turns every hook off. Quarto files count only with an R chunk; R libraries, build and check output are never walked. The session id is read from the payload's own field, never from a command's output; the ledger records a file as it was checked; the stop hook holds the turn only for a defect in the code, not for a source it cannot open or a linter that could not run; the subagent context is saved per session instead of re-run; the index is pre-warmed only when its cache is cold; a project with R code but no Rscript on PATH gets one line saying so.
- `r-colmap`: column lineage (what each created column derives from, what is computed from it), overwrites as sites, the files and tables the code writes with who reads them, `--source NAME`, `--at FILE:LINE` (the column set before and after each step on a line), `--diff [REF]` (the columns the changed lines use, create, overwrite or drop, where else each is used, the columns the changed files no longer create and who still reads them, the checker's findings on the changed lines). Sources are keyed by what they are, so two files with one name are two sources and a literal keeps its identity when lines move; a long created section is folded; assertions are listed apart from data.
- `r-colcheck`: a reader of a path another script writes (`write_csv`, `saveRDS`, `write_parquet`, `dbWriteTable`, ...) resolves from the writer's columns when the file is absent; `tar_target()` binds a target, `tar_read()`/`tar_load()` resolve it from `_targets.R`; `janitor::clean_names()` and `rename_with(make_clean_names)` are computed; every finding names the nearest known column (`did you mean region?`); a wide frame is counted, not cut. New findings: a name assigned twice in one call, a column compared with itself, `mutate(x = NULL)` removes the column; a note when an aggregate runs per group that `summarise()`'s default left behind.
- Checker fixes: a pipeline into `ggplot()` is checked, computed aesthetics are not columns; `select()` keeps the grouping columns and applies its arguments left to right; `rename()` renames the groups; a TSV read with `fread()`/`vroom()` is read with its own separator; `header = T`; a helper calling its sibling in the same file is followed; a stale cached schema in an Rmd no longer aborts the file; a registered loader resolves through its key argument alone; `count(name = variable)`, `unnest_longer(values_to, indices_to)`, `mutate(.keep = )`; a brace-less helper in another file reports its own line; a column name with regex characters; steps counted once; the walker's scope restored after a checker error.
- CLI: one root-detection rule for the shell tools (markers up to the home directory, then `CLAUDE_PROJECT_DIR`); `--root` anywhere; `help` as the first argument everywhere; unknown options exit 2 in every tool; `learn` refuses to overwrite a cache that does not parse and keeps fields it does not know; `--source` relative to the shell's directory; `r-lsp-status -n N` and `--help`; `r-lint` judges each file against its own project; `r-colcheck --json` paths relative to the root.
- `r-callmap --diff`: a deletion above the first definition changes nothing, a prose edit in an Rmd is not top-level code, a target's span ends where the next target begins.
- Server and profile: every patch is wrapped; a failed worker refresh is said; `R_LSP_LOAD_MAX` below zero is the default; the status log is trimmed at start; the profile's undefined-function check does not depend on the process running it; a line-leading named argument is not a definition; a pre-set `R_USER_CONFIG_DIR` is reported as the profile being off; `R_LSP_AGENT_LINTERS` takes `false`, `0`, `no`, `off`.
- Tests run without the language server: the scenarios that need it skip; new scenarios for the dormant hooks, data flow between scripts and targets, and the tools' argument conventions.

## 1.8.1 (2026-10-23)

A changelog with one entry per release, the release steps written down, quick references at the top of the plugin README and the r-lsp skill, and a session-start line that names every tool and task skill, so an agent knows what it has.

## 1.8.0 (2026-10-23)

Every change to an R file is checked (Bash, subagents, stop), the server re-reads changed files, the call map knows what a diff touches and which tests reach it, a data map, a schema registry for S3 loaders, and the sweep's fixes.
- Every change to an R file is checked: after a Bash command (heredoc, `sed -i`, `styler`, `roxygenise()`, `git checkout`), inside subagents (which also get the project context at their start), after `/compact`, and once more at the end of the turn for files no check has seen.
- The language server re-reads files that changed on disk before answering, so navigation after git or styler is current.
- `r-callmap --diff [REF]` (what a change touches, removed functions still mentioned, callers, and the tests that reach them), `--tests NAME`, `--file PATH`.
- `r-colmap`: the data map of every source, its columns, and where each is used, dropped or created.
- `r-colcheck discover`, `learn --all`, and a loader registry in `.colcheck.json` so S3 loaders are learned from parquet footers.
- `r-lint FILE`: the bug profile from the shell. Skills `r-refactor`, `r-review`, `r-data`.
- Checker: reader arguments, base-R column access, joins with suffixes, `a:b` ranges, grouping after `summarise()`, `pivot_longer`, branch reassignments, twenty more verbs, sounder cache matching. Map: shared definition files outside `R/`, R6 `$new()`/`super$`, aliases, no false S3 tags, exit 2 on a wrong root. Server: survives the next languageserver's asynchronous index, status log tells sessions apart and records failures, defunct-function lints reach the hook note.

## 1.7.19 (2026-10-23)

Rmd/qmd chunks are read: the call map, the column check, the post-edit hook and the server's startup loader cover vignettes.

## 1.7.18 (2026-10-21)

Tidyselect helpers expand against known columns; a function passed as a value is not a column; partly known row binding; refresh.

## 1.7.17 (2026-10-21)

Split() and map() lambdas are followed; all_of(cols) reads the vector the code states.

## 1.7.16 (2026-10-21)

Row binding gives the union of its inputs' columns.

## 1.7.15 (2026-10-21)

Column assertions state what a frame has; learn --load-all; a literal inside a lambda is not a key.

## 1.7.14 (2026-10-21)

Across() keeps the column set, a stop inside a helper is named, piped calls hit the cache, keys per argument, --steps.

## 1.7.13 (2026-10-21)

List-columns of data frames are followed through unnest() and hoist().

## 1.7.12 (2026-10-21)

The note names the step where a known frame became unknown.

## 1.7.11 (2026-10-21)

A pipeline ending in pull() or nrow() is checked, and its target is not an unresolved frame.

## 1.7.10 (2026-10-21)

No note for public-package functions; the docs stop blaming find-references for map() sites.

## 1.7.9 (2026-10-21)

No monitor outside an R project.

## 1.7.8 (2026-10-21)

Cache entries keyed on a function name.

## 1.7.7 (2026-10-21)

Tidy-select islands inside data-masked code are checked; an unresolved grouping is unknown, not empty.

## 1.7.6 (2026-10-21)

All_of("name") is checked in .by and in select().

## 1.7.5 (2026-10-21)

The .by argument is checked like group_by().

## 1.7.4 (2026-10-21)

Hover and document symbols wait for the index; an empty answer while indexing is called what it is.

## 1.7.3 (2026-10-21)

A file that does not parse is reported, not passed over in silence.

## 1.7.2 (2026-10-21)

No checker crash on factory calls; the cache beats following; reassignment reads the old frame.

## 1.7.1 (2026-10-21)

Session-start context states the real index coverage.

## 1.7.0 (2026-10-21)

Follow data frames into installed package functions; flag experimental functions.

## 1.6.2 (2026-10-21)

Bug linters run from the hook when a project .lintr wins; profile loads on lintr 3.1.

## 1.6.1 (2026-10-21)

Forward only actionable index events; stop logging no-op rescans.

## 1.6.0 (2026-10-21)

R-callmap, a static call map of the project in one listing.

## 1.5.1 (2026-10-20)

The note greps the file's own repository.

## Before 1.5.1 (2026-04-22 to 2026-10-20)

Unversioned commits: the renv-aware wrapper and the server patches (shared scope, complete answers, loading tests and scripts at startup), the diagnostics profile, `r-colcheck` with frames followed through project functions and the schema cache, the post-LSP and post-edit notes, and the smoke suite.
