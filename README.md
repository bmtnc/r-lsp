# R Language Server for Claude Code

## Why this matters

Without an LSP, Claude Code navigates R codebases using grep and file reads — text search that has no understanding of R itself. It can't distinguish a function definition from a comment that mentions the same name, doesn't know what arguments a function accepts, and can't trace how functions call each other across files.

An LSP (Language Server Protocol) gives Claude Code a direct line to R's own parser and package system. Instead of searching for text patterns, Claude can ask "what does this function do?" and get the actual documentation, or "where is this defined?" and jump straight to the source. After every edit, the LSP re-analyzes the file and reports errors immediately — Claude sees them in the same turn and can fix them without a separate test cycle.

The practical difference: Claude makes fewer mistakes, finds things faster, and catches errors earlier. It understands your code structurally — which packages you use, what functions they export, how your functions relate to each other — rather than just pattern-matching against text.

Gives Claude Code live diagnostics, hover documentation, go-to-definition, find-references, and completions for R code. Works with renv projects — the LSP sees your pinned package versions, not just globally installed packages.

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

Optionally, install `lintr` for linting diagnostics:

```bash
Rscript --vanilla -e 'install.packages("lintr", repos = "https://cloud.r-project.org")'
```

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
│   └─��� marketplace.json        # Marketplace manifest
├── plugins/
│   └── r-lsp/
│       ├── .claude-plugin/
│       │   └── plugin.json     # Plugin metadata
│       ├── .lsp.json           # LSP server config
│       ├── bin/
│       │   └── r-lsp-wrapper   # Wrapper script (renv-aware)
│       └── README.md
└── README.md                   # This file
```

## How it works

The R language server (`REditorSupport/languageserver`) communicates with Claude Code over stdin/stdout using the Language Server Protocol.

The wrapper script solves a problem with renv: the LSP process starts before it knows which project you're working on, so renv can't activate at startup. The wrapper patches the server's initialization handler to activate renv once the project root is known, giving the LSP visibility into both the project's renv library (pinned versions) and the global library (where `languageserver` is installed).

For projects without renv, the wrapper has no effect — the LSP uses the global library as normal.

## What this gives Claude Code

- **Diagnostics**: After every `.R` file edit, reports syntax errors, warnings, and linting issues (if `lintr` is installed globally).
- **Hover**: Documentation for any function, including from renv-pinned packages.
- **Go-to-definition**: Jump to where a function is defined within the package's `R/` directory.
- **Find references**: Find all usages of a function within `R/`.
- **Completions**: Function and symbol name completions based on what's in scope.
- **Outgoing calls**: Trace which functions a given function calls, including namespaced calls like `dplyr::mutate()`.

## Known limitations

- **`R/` only**: The language server indexes `R/` in R packages. Functions defined in `scripts/` or `tests/` are not included in workspace-wide queries like find-references.
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
4. Restores the original working directory
5. Calls the original `on_initialized` handler

**Critical detail — stdout protection:** renv's `activate.R` can produce output. The LSP communicates over stdout using JSON-RPC, so any stray output corrupts the protocol (symptoms: `Header must provide a Content-Length property` errors, concatenated responses). The wrapper uses `sink(stderr())` to redirect all output during activation.

### What the LSP indexes

The `load_workspace` function in `languageserver` is hardcoded to scan only `R/`:

```r
source_dir <- file.path(workspace$root, "R")
files <- list.files(source_dir, pattern = "\\.r$", ignore.case = TRUE)
```

And it only runs for R packages:

```r
if (!is_package(workspace$root)) { return(invisible(NULL)) }
```

This means:
- `scripts/`, `tests/`, `inst/` are never indexed
- Non-package projects (no `DESCRIPTION` file) get no workspace indexing at all
- `findReferences`, `incomingCalls`, and `workspaceSymbol` only cover `R/`
- Individual files outside `R/` still get local analysis (diagnostics, document symbols) when opened, but aren't part of cross-file queries
- There is no configuration option to change which directories are indexed — it's baked into the `languageserver` source code

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
- `findReferences` — within `R/` only
- `outgoingCalls` — traces calls including through `pkg::fun()` namespaced calls (with renv fix)
- `diagnostics` — syntax errors and lintr warnings after every edit
- `completions` — function names, parameters, symbols in scope

**Does not work:**
- `findReferences` across `scripts/` or `tests/` — those directories are not indexed
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

The only external dependency is the `languageserver` R package (and its dependency `collections`). Both are on CRAN. No marketplace, no fork, no custom builds. The wrapper script is plain bash. The plugin config is plain JSON.

If `lintr` is installed globally, the language server will use it for linting diagnostics. This is optional — without it, you still get syntax error detection but not style/quality warnings.
