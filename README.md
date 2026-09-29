# R Language Server for Claude Code

## Why this matters

Without an LSP, Claude Code navigates R codebases using grep and file reads — text search that has no understanding of R itself. It can't distinguish a function definition from a comment that mentions the same name, doesn't know what arguments a function accepts, and can't trace how functions call each other across files.

An LSP (Language Server Protocol) gives Claude Code a direct line to R's own parser and package system. Instead of searching for text patterns, Claude can ask "what does this function do?" and get the actual documentation, or "where is this defined?" and jump straight to the source. After every edit, the LSP re-analyzes the file and reports errors immediately — Claude sees them in the same turn and can fix them without a separate test cycle.

The practical difference: Claude makes fewer mistakes, finds things faster, and catches errors earlier. It understands your code structurally — which packages you use, what functions they export, how your functions relate to each other — rather than just pattern-matching against text.

Gives Claude Code live diagnostics, hover documentation, go-to-definition, find-references, and completions for R code. Works with renv projects — the LSP sees your pinned package versions, not just globally installed packages. Diagnostics report likely bugs only, and understand tidyverse code (see [Diagnostics](#diagnostics)).

## Setup

### Prerequisites

- macOS or Linux
- R installed and on your `PATH`
- Claude Code (CLI, desktop app, or IDE extension)

### Step 1: Install the `languageserver` R package globally

Run this in your terminal (not inside R):

```bash
Rscript --vanilla -e 'install.packages("languageserver", repos = "https://cloud.r-project.org")'
```

`--vanilla` is important — without it, running this inside an renv project would install into the project library instead of the global library.

This also installs `lintr`, which produces the diagnostics.

### Step 2: Install the plugin

In Claude Code, run:

```
/plugin marketplace add bmtnc/r-lsp
/plugin install r-lsp
```

Then restart Claude Code or run `/reload-plugins`.

### Step 3: Verify

Open any R project and ask Claude Code to hover over a function. If you get documentation back, it's working. For renv projects, test with a function from a package that's only in the renv library (not installed globally) to confirm renv integration.

---

## Repository structure

```
r-lsp/
├── .claude-plugin/
│   └── marketplace.json        # Marketplace manifest
├── plugins/
│   └── r-lsp/
│       ├── .claude-plugin/
│       │   └── plugin.json     # Plugin metadata
│       ├── .lsp.json           # LSP server config
│       ├── bin/
│       │   └── r-lsp-wrapper   # Wrapper script (renv-aware)
│       ├── lintr/
│       │   ├── agent_linters.R # Diagnostics profile (bugs only, tidyverse-aware)
│       │   └── user-config/    # Points lintr at the profile
│       └── README.md
├── tests/
│   ├── smoke_test.py           # End-to-end test through the LSP
│   ├── lsp_client.py           # Minimal LSP client used by the test
│   └── fixtures/
├── .github/workflows/
│   └── smoke-test.yml          # Runs the test on push and weekly
└── README.md                   # This file
```

## How it works

The R language server (`REditorSupport/languageserver`) communicates with Claude Code over stdin/stdout using the Language Server Protocol.

The wrapper script solves a problem with renv: the LSP process starts before it knows which project you're working on, so renv can't activate at startup. The wrapper patches the server's initialization handler to activate renv once the project root is known, giving the LSP visibility into both the project's renv library (pinned versions) and the global library (where `languageserver` is installed).

For projects without renv, the renv part has no effect — the LSP uses the global library as normal.

The wrapper also points lintr at the plugin's diagnostics profile (see [Diagnostics](#diagnostics)).

## What this gives Claude Code

- **Diagnostics**: After every `.R` file edit, reports syntax errors and likely bugs (see [Diagnostics](#diagnostics)).
- **Hover**: Documentation for any function, including from renv-pinned packages.
- **Go-to-definition**: Jump to where a function is defined within the package's `R/` directory.
- **Find references**: Find all usages of a function within `R/`.
- **Completions**: Function and symbol name completions based on what's in scope.
- **Outgoing calls**: Trace which functions a given function calls, including namespaced calls like `dplyr::mutate()`.

## Diagnostics

When a project has no `.lintr` file (and you have no `~/.lintr`), diagnostics come from the plugin's profile in `plugins/r-lsp/lintr/agent_linters.R` instead of lintr's defaults. lintr's defaults are mostly style rules. For an agent those are noise: they push Claude to reformat code it wasn't asked to touch, and they bury real errors. On a 44-line tidyverse script, the defaults gave 49 diagnostics, and 48 were false or style-only. The profile gives 10, all real.

The profile reports:

- **Undefined functions**, in top-level script code as well as inside functions: `could not find function "filterr"`. lintr's own check only looks inside function bodies. Names defined anywhere in the project count as defined, so helpers from another script don't trigger it.
- **Undefined variables** inside functions, and **non-exported** `pkg::fun` calls.
- **Deprecated, defunct and superseded functions**, judged against the version installed for the project. The profile reads each function's `lifecycle` signal (or its help page badge), e.g. `cur_data() is deprecated as of dplyr 1.1.0; use pick() instead`. Superseded functions show as information (they still work), deprecated as warnings, defunct as errors.
- **Functions missing from the project's pinned version**, e.g. `list_rbind() is not in purrr 0.3.5, the version installed for this project`. This catches new APIs written into a project that renv pins to an older release.
- lintr's `correctness` and `common_mistakes` rules: `x == NA`, missing packages, duplicate arguments, and similar.

It understands tidyverse code:

- `library(tidyverse)` (and other meta-packages built the same way, like `tidymodels`) attaches its core packages, so `mutate()`, `ggplot()` and `%>%` are known.
- Bare column names inside dplyr, tidyr, ggplot2 and similar calls are not reported. Undefined *functions* inside those calls still are.
- Variables used only inside glue or cli strings (`"{n} rows"`) are not reported as unused.
- cli-style message bullets (`c(i = "...", i = "...")`) are not reported as duplicate arguments.

To turn the profile off and get lintr's defaults, set `R_LSP_AGENT_LINTERS=false` in the environment Claude Code runs in. A project `.lintr` always wins, and so does `~/.lintr`. The profile is found through `R_USER_CONFIG_DIR`, so while it is active lintr does not read a user config at `~/.config/R/lintr/config`; move that to `~/.lintr` to keep it. If you set `R_USER_CONFIG_DIR` yourself, the wrapper leaves it alone and the profile is not used.

## Known limitations

- **References cover `R/`, open files and `source()`d files**: Since `languageserver` 0.3.19, workspace symbol search covers every `.R` file in the project, packages or not. Find-references and go-to-definition still only see a package's `R/` directory, files that are open, and files linked by static `source()` calls. Calls in unopened `tests/` or `scripts/` files are missed.
- **No argument checks**: A wrong argument name or too many arguments (`f(a = 1, bb = 2)`) is not reported.
- **Dynamic scope**: Files that use `attach()`, `list2env()`, `sys.source()` or `box::use()`, or that load a package that isn't installed, skip the undefined-function check. The check can't know what those put in scope.
- **Single workspace**: Each LSP instance serves one project. It can't cross-reference between two of your packages simultaneously.
- **Hover gaps**: Some namespaced calls may not return hover docs depending on how the package structures its help pages.

## Troubleshooting

- **LSP not starting**: Check that `r-lsp-wrapper` is executable and on `PATH`. Check that `R` is on `PATH`.
- **`languageserver` not found**: Run `Rscript --vanilla -e 'find.package("languageserver")'` — should return a path under the global R library, not an renv path.
- **renv packages not showing hover**: The LSP may not have finished initializing. Wait a few seconds after opening a project and try again.
- **JSON-RPC errors**: Check that `renv/activate.R` in your project doesn't print to stdout. The wrapper suppresses messages, but custom `.Rprofile` hooks could interfere.

---

## Appendix: Technical context for Claude Code

This section documents the architecture, gotchas, and debugging knowledge discovered during development. Claude Code should reference this when setting up, verifying, or troubleshooting the R LSP.

### Architecture — the full chain

There are five layers between Claude Code and R code intelligence:

1. **Claude Code harness** — The LSP client. When you edit or open an `.R` file, the harness sends LSP events (`textDocument/didOpen`, `textDocument/didChange`) to the server and receives diagnostics back. It also makes LSP requests (hover, go-to-definition, find-references) and feeds results into conversation context.

2. **Plugin config (`.lsp.json`)** — A static JSON declaration that tells Claude Code: "for files ending in `.R`, `.Rmd`, or `.qmd`, spawn the command `r-lsp-wrapper` and talk to it using the Language Server Protocol over stdio." No code — equivalent to a VS Code `settings.json` LSP entry.

3. **Wrapper script (`r-lsp-wrapper`)** — A bash script that launches R with `--vanilla` and applies a monkey-patch before starting the language server. The `--vanilla` flag prevents `.Rprofile` from running, which is necessary because renv's `.Rprofile` hook would hijack `.libPaths()` before the LSP knows the project root.

4. **R session** — A long-running R process using the global library. One process per Claude Code session. Persists for the session lifetime — not re-launched per file or per request.

5. **`languageserver` R package** — The open-source `REditorSupport/languageserver` package from CRAN. Implements the Language Server Protocol: diagnostics, go-to-definition, hover, find-references, completions, outgoing calls. Communicates with Claude Code over stdin/stdout using JSON-RPC.

### The renv problem and how the wrapper solves it

**The problem:** When R starts in a directory with an renv project, `.Rprofile` calls `renv::activate()`, which replaces `.libPaths()` with the project-scoped library. `languageserver` is installed globally (not in any project's renv lockfile), so the LSP process can't find it and fails silently.

**Why `--vanilla` alone isn't enough:** Using `--vanilla` skips `.Rprofile`, so renv never activates and `languageserver` loads fine. But then the LSP can only see packages in the global library. Any package installed only in the renv project library (e.g., dplyr, tidyr, arrow) is invisible — hover returns nothing, completions are missing, and diagnostics can't resolve imports.

**Why activating renv at startup doesn't work:** The wrapper script runs before the LSP receives the `initialize` request from Claude Code. At startup, the R process doesn't know which project it's serving. The working directory is the user's home directory (confirmed: `lsof -p <pid>` shows `cwd` as `~`), not the project root. Calling `source('renv/activate.R')` with a relative path either fails (no `renv/activate.R` in `~`) or activates the wrong project.

**The solution — monkey-patch `on_initialized`:** The wrapper overrides `languageserver:::on_initialized` before calling `languageserver::run()`. This function is called after the LSP receives the `initialized` notification from Claude Code, at which point `self$rootPath` contains the project root. The patch:

1. `setwd(self$rootPath)` — changes to the project directory (renv determines its project from the working directory)
2. `source(renv/activate.R)` — activates renv, which sets `.libPaths()` to the project library
3. Appends the global library back to `.libPaths()` — so `languageserver` (and its dependencies) remain findable
4. Puts a private library first on `.libPaths()`, holding symlinks to `languageserver` and everything it imports, so a project that pins an old `lintr`, `xml2`, etc. in renv cannot break the server
5. Restarts the idle helper processes (see below)
6. Restores the original working directory
7. Calls the original `on_initialized` handler

**Helper processes:** `languageserver` parses files and runs lintr in separate R processes (callr sessions). They start with the server, before renv is activated, and keep the library paths they started with. Without a restart, diagnostics never see the renv library. Every function from a renv-only package is reported as "no visible global function definition", and hover fails on functions attached with `library()`. After changing `.libPaths()`, the wrapper retires the helpers that aren't running a task, and their replacements inherit the new paths.

**Versions the server loads itself:** A helper process loads `languageserver` and its imports, such as purrr, stringr, rlang and cli, from the global library, and R can't load two versions of a package in one process. So the diagnostics profile reads package facts (exports, lifecycle stage, version) from the project's installed copy on disk, not from what is loaded.

**Critical detail — stdout protection:** renv's `activate.R` can produce output. The LSP communicates over stdout using JSON-RPC, so any stray output corrupts the protocol (symptoms: `Header must provide a Content-Length property` errors, concatenated responses). The wrapper uses `sink(stderr())` to redirect all output during activation.

### What the LSP indexes

Since `languageserver` 0.3.19 (setting `index_mode`, default `"auto"`), indexing has two levels:

- **Full parse**: a package's `R/` files, open files, and files linked by static `source()` calls. These feed find-references, go-to-definition, incoming calls and completion.
- **Shallow summary**: every other `.R` file in the workspace, packages or not. The summary stores definitions and `source()` edges but not call sites. So those files appear in workspace symbol search, but calls inside them are not found by find-references.

This means:
- `workspaceSymbol` covers the whole project
- `findReferences` and `incomingCalls` miss calls in unopened `tests/` and `scripts/` files. For example, find-references on dplyr's `compute_by()` returns the 10 uses in `R/` and none of the ~20 in `tests/`
- The only index settings are `index_mode` (`"auto"` or `"off"`) and include/exclude globs; there is no "fully parse everything" mode

### How namespace resolution works

The `languageserver` loads package namespaces lazily. When it encounters `pkg::fun()`:
- `get_namespace(pkgname)` checks if the package is installed via `find.package(pkgname, quiet = TRUE)`
- If found, creates a `PackageNamespace` object that loads the package's exports via `asNamespace()` and `getNamespaceExports()`
- The namespace is cached for subsequent lookups

For hover on `pkg::fun()`:
- The token scanner (C code: `scan_token_c`) extracts `package`, `accessor` (`::`), and `token` (function name)
- Since `accessor != ""`, the hover handler skips local symbol resolution entirely
- Falls through to `workspace$get_help(token, package)`, which calls `utils::help((topic), (pkgname))`
- The help result is rendered as text (or markdown if `rmarkdown` + `pandoc` are available)

### LSP capabilities — what works and what doesn't

**Works well:**
- `documentSymbol` — listing functions in any `.R` file
- `hover` — documentation for local functions and `pkg::fun()` namespaced calls (with renv fix)
- `goToDefinition` — for functions defined in the project's `R/` directory
- `findReferences` — within `R/`, open files and `source()`d files
- `outgoingCalls` — traces calls including through `pkg::fun()` namespaced calls (with renv fix)
- `diagnostics` — syntax errors and lintr warnings after every edit
- `completions` — function names, parameters, symbols in scope

**Does not work:**
- `findReferences` into unopened `scripts/` or `tests/` files — those get only a shallow summary
- `goToDefinition` into external package source — jumps to the installed copy, not a local clone
- Multi-root workspace — `workspace/didChangeWorkspaceFolders` is a no-op in `languageserver`
- `hover` on column names inside tidy evaluation (e.g., `mutate(data, new_col = old_col + 1)`) — `old_col` is data-masked, not a real R symbol, so static analysis can't resolve it

### Verifying renv integration is working

To confirm the LSP can see renv packages, not just global ones:

1. Find a package in the project's `renv.lock` that is NOT installed globally:
   ```r
   Rscript --vanilla -e "
     lock <- jsonlite::fromJSON('renv.lock')
     global_pkgs <- rownames(installed.packages())
     renv_only <- setdiff(names(lock\$Packages), global_pkgs)
     cat(head(renv_only, 5), sep='\n')
   "
   ```

2. Use LSP hover on a `pkg::fun()` call from one of those packages. If it returns full documentation, renv integration is working.

3. If hover returns nothing for renv-only packages but works for globally-installed ones, the renv activation is not running. Check:
   - Is `r-lsp-wrapper` the script being executed? (`ps aux | grep languageserver`)
   - Does the project have `renv/activate.R`?
   - Are there any errors in stderr? (check Claude Code debug logs)

### Process lifecycle

- Claude Code spawns one R LSP process per session
- The process runs for the lifetime of the session
- `maxRestarts: 3` in `.lsp.json` means if the process crashes, Claude Code will restart it up to 3 times
- `startupTimeout: 60000` (60 seconds) gives the server time to load, especially on first run when R packages may need compilation
- When Claude Code exits, the LSP process is terminated

### Dependencies

The only external dependency is the `languageserver` R package, from CRAN; it installs `lintr` and its other dependencies. No fork, no custom builds. The wrapper script is plain bash, the diagnostics profile is plain R, and the plugin config is plain JSON.

### Testing

`tests/smoke_test.py` starts the wrapper on scratch projects and talks to it over LSP, the way Claude Code does. It checks planted bugs, a tidyverse script, renv-only packages, a project pinning an old `lintr`, and a project pinning an old `purrr`. It needs R with `languageserver`, `lintr` and `renv` installed (plus `tidyverse` for the tidyverse scenario):

```bash
python3 tests/smoke_test.py            # all scenarios
python3 tests/smoke_test.py -k renv    # scenarios whose name contains "renv"
```

CI runs it on every push and weekly, because a new `languageserver` release can break the wrapper's patches without any change here. Set `R_LSP_WRAPPER` to test a different wrapper script.
