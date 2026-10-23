---
name: r-refactor
description: Change or extend R functions safely with the plugin's call map. Use when asked to rename, split, add an argument to, delete or otherwise refactor an R function, or to make a change whose callers you do not know yet. Gives the order of operations - map, plan, edit, diff, test - using r-callmap, r-colcheck and the post-edit notes.
---

# Refactor an R function with the map

The call map is static and complete for calls by name, by value (`map(x, f)`) and by string (`do.call("f")`), so it replaces reading `R/` to find callers. Work in this order; each step names the tool that answers it.

1. **Map before reading.** `r-callmap NAME` (several names at once: `r-callmap f g h`) prints the callers two levels up and the callees two levels down, each call site as `file:line`. For a wide change, `r-callmap` alone prints the whole project (about 100 tokens per function). Read the map, then open only the files it names.
2. **Decide what each call site needs.** The map shows who calls, not how arguments are passed. Before changing a signature, read each call site: a positional call shifts silently when an argument is inserted. Adding an argument with a default at the end is the only change that needs no call-site edits.
3. **Edit with Edit/Write.** After each edit the plugin's note reports column names that cannot exist, calls to project functions whose argument names no longer match the definition, parse errors, and (when the language server does not report on the file) the bug linters. Silence means checked and clean. A change made through Bash (`sed`, a heredoc, `styler`) is checked after the command, so it is not unchecked, but the language server does not see it until the file is read again.
4. **Check the diff as a whole.** `r-callmap --diff` lists every definition the working tree changes against `HEAD` (new, arguments changed, body changed), the definitions that are gone and where they are still mentioned, the top-level code that changed, the callers of everything changed, and the test files that reach them with the command to run each. A function listed under "Definitions no longer in the project" with mentions left is a broken call. `r-callmap --diff main` compares against a branch instead.
5. **Run the tests the map names.** `r-callmap --tests NAME` lists the test files that reach NAME through callers, with the `devtools::test(filter = ...)` or `testthat::test_file()` command. Run those before the whole suite; "No test reaches" means the change has no test, which is worth saying in the summary.
6. **Say what the map cannot see.** Calls built from strings at run time, S4 dispatch, R6 calls through an object (`obj$method()`) and inline `r` code in Rmd prose are not edges. For a function with a common name, confirm with `LSP findReferences` (load it with `ToolSearch "select:LSP"`) and the note that follows it.

For a data pipeline, add `r-colmap COLUMN` (where the column is used, dropped or created, per source) before renaming or removing a column, and `r-colcheck FILE` after; both are the data-side counterparts of the map.
