# Changelog

One entry per release, newest first. The version is the one in `plugins/r-lsp/.claude-plugin/plugin.json` and `.claude-plugin/marketplace.json`; each release is a commit whose subject starts with the version, and a tag `vX.Y.Z` on it. The commit message carries the full account; this file carries the headline.

## 1.8.1 (2026-10-23)

A changelog with one entry per release, tags on every release, quick references at the top of the plugin README and the r-lsp skill, and a session-start line that names every tool and task skill, so an agent knows what it has.

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
