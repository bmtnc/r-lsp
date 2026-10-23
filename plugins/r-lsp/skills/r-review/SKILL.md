---
name: r-review
description: Review R changes in the working tree (or a branch against main) with the plugin's tools before a commit or pull request. Use when asked to review, check, sanity-check or summarise R code changes, or before committing R code. Covers impact (callers of changed functions, removed functions still mentioned, changed signatures), column names against the data, argument names, the bug linters and the tests to run.
---

# Review R changes with the map and the checks

A review of R changes has four questions the tools answer directly; run them before reading the diff line by line.

1. **What changed and who depends on it.** `r-callmap --diff` (or `r-callmap --diff REF` against a branch) lists the definitions the diff touches with the kind of change (`[new]`, `[arguments were (x)]`, `[body changed]`), their callers with call sites, the definitions removed with where they are still mentioned, and the top-level code that changed. A changed argument list means every call site listed needs reading: positional calls shift silently. A removed definition with mentions left is a bug.
2. **Is the pipeline code right against the data.** `r-colcheck FILE...` on each changed `.R`/`.Rmd` file reports bare column names that cannot exist at that point, checked against the data the code reads (local files or the schema cache), and calls to project functions with argument names the definition does not have; it exits 1 with findings. The stderr summary leads with coverage: `0 of N steps checked` means nothing was checkable, not that it was clean; a note names each source to learn (`r-colcheck discover` lists them all). `r-colmap COLUMN` shows where a column the diff creates or renames is used elsewhere.
3. **Bugs the linters know.** In the main conversation the language server reported the plugin's bug profile after each Edit/Write (undefined functions, non-exported `pkg::fun`, deprecated or superseded calls, missing packages); for files changed otherwise, the post-bash note did. To run them on the changed files by hand: `r-lint FILE...` (exit 1 with findings, parse errors included).
4. **Which tests prove it.** The `--diff` output ends with the test files that reach the changed definitions and the command for each; run those. "No test reaches" names the functions the suite does not cover.

Report in that order: impact, data, bugs, tests, then what the tools cannot see (dynamic calls, S4, R6 through an object, inline Rmd code).
