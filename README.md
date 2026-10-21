# R Language Server for Claude Code

## Why this matters

Without an LSP, Claude Code navigates R codebases using grep and file reads — text search that has no understanding of R itself. It can't distinguish a function definition from a comment that mentions the same name, doesn't know what arguments a function accepts, and can't trace how functions call each other across files.

An LSP (Language Server Protocol) gives Claude Code a direct line to R's own parser and package system. Instead of searching for text patterns, Claude can ask "what does this function do?" and get the actual documentation, or "where is this defined?" and jump straight to the source. After every edit, the LSP re-analyzes the file and reports errors, so Claude can fix them without a separate test cycle.

The practical difference: Claude makes fewer mistakes, finds things faster, and catches errors earlier. It understands your code structurally — which packages you use, what functions they export, how your functions relate to each other — rather than just pattern-matching against text.

Gives Claude Code diagnostics after each edit, hover documentation, go-to-definition, find-references, workspace symbol search and call hierarchy for R code, plus a column-name check for tidyverse code. Works with renv projects — the LSP sees your pinned package versions, not just globally installed packages. Diagnostics report likely bugs only, and understand tidyverse code (see [Diagnostics](#diagnostics)).

## Setup

### Prerequisites

- macOS or Linux
- R installed, with `R` and `Rscript` on the `PATH` of the shell you start Claude Code from (the server uses `R`, the hooks and shell tools use `Rscript`)
- Claude Code (CLI, desktop app, or IDE extension) in a terminal session; plugin language servers do not run in cloud sessions

### Step 1: Install the `languageserver` R package globally

Run this in your terminal (not inside R):

```bash
Rscript --vanilla -e 'install.packages("languageserver", repos = "https://cloud.r-project.org")'
```

`--vanilla` is important — without it, running this inside an renv project would install into the project library instead of the global library.

This also installs `lintr`, which produces the diagnostics. The plugin needs `languageserver` 0.3.19 or newer for index status, complete cross-file answers and other-repository support (those arrived with the project-wide index in 0.3.19; 0.3.20 is current). On an older release the server still starts with renv support and the hooks, prints a message to `r-lsp-status` and `claude --debug`, and the rest stays off. The install command above also upgrades an existing copy; to see which version is installed:

```bash
Rscript --vanilla -e 'cat(as.character(packageVersion("languageserver")))'
```

CRAN's prebuilt macOS binaries can lag the source release by several versions (with R 4.4 the binary was still 0.3.17 while the source was 0.3.20). If the version printed is below 0.3.19, install from source, which needs the Xcode command line tools:

```bash
Rscript --vanilla -e 'install.packages("languageserver", repos = "https://cloud.r-project.org", type = "source")'
```

### Step 2: Install the plugin

In Claude Code, run:

```
/plugin install r-lsp --marketplace bmtnc/r-lsp
```

or, on Claude Code versions before 2.1.275, in two steps:

```
/plugin marketplace add bmtnc/r-lsp
/plugin install r-lsp
```

Then restart Claude Code or run `/reload-plugins`.

### Step 3: Verify

1. Start a session in an R project. The first lines of context should read `R LSP plugin: R package '...'` (or `R scripts project`), with file counts and the index cache state. That is the `SessionStart` hook; it confirms `Rscript` was found.
2. Ask Claude to introduce a small error in an `.R` file and fix it. The server starts on that first edit; after the edit, the conversation shows `Found N new diagnostic issues`.
3. Ask Claude to run `r-lsp-status`. It prints the server's "indexing" / "index ready" events for the project.
4. Ask Claude to hover over a function. For renv projects, pick a function from a package that is only in the renv library to confirm renv integration.

If a step fails, see [Troubleshooting](#troubleshooting).

### Using it

Nothing to do: the skill tells Claude when to use the LSP tool and what each answer covers, the hooks add context on their own, and the three shell tools are on Claude's `PATH`. The pieces you can ask for by name:

- `r-colcheck path/to/file.R` — column names in tidyverse code vs the data the file reads (also runs after every `.R` edit). `r-colcheck learn KEY 'EXPR'` caches the column names of data the checker cannot open (S3, databases) so those pipelines are checked too.
- `r-callmap` — the project's call map in one listing: every function with its file:line, what it calls and what calls it, including tests and scripts. `r-callmap NAME` shows one function's callers and callees. Useful before a refactor.
- `r-lsp-status` — recent index events for this project.

Environment variables, set in the shell that starts Claude Code: `R_LSP_AGENT_LINTERS=false` to use lintr's default rules instead of the plugin's profile; `R_LSP_INDEX_WAIT=<seconds>` to change how long cross-file answers wait for indexing (default 30); `R_LSP_LOAD_MAX=<n>` to cap how many test and script files are loaded at startup (default 600); `R_LSP_STATUS_FILE=<path>` to move the status log; `R_LSP_COLCHECK_PACKAGES=false` to stop `r-colcheck` reading installed package functions (which loads their namespaces); `R_LSP_COLCHECK_DEBUG=1` to print the call stack of any checker failure.

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
│       │   ├── r-lsp-wrapper   # Starts the server (environment, lint profile)
│       │   ├── r-lsp-server.R  # The server with the plugin's patches
│       │   ├── r-colcheck      # Column names vs the data a script reads; `learn` fills the schema cache
│       │   ├── r-callmap       # Static call map of the project, or of one function's neighbourhood
│       │   └── r-lsp-status    # Recent index events for this project
│       ├── hooks/              # SessionStart context + pre-warm, post-edit and post-LSP notes
│       ├── monitors/           # Streams index events to Claude
│       ├── scripts/            # R code behind the hooks, r-colcheck and r-callmap
│       ├── skills/r-lsp/       # Tells Claude how to use all of this
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

Beyond the server itself, the plugin uses the other channels Claude Code gives a plugin, because Claude Code's LSP client is narrow: its LSP tool has nine read-only operations (definition, references, hover, document and workspace symbols, implementation, call hierarchy), it pushes diagnostics only after Claude's own edits (10 per file, 30 per edit, each shown once per session), and it never tells the server about files changed by `git` or `Bash`. So:

- **A skill** (`skills/r-lsp/SKILL.md`) tells Claude when to use the LSP tool, what each answer covers, and how to check data-frame column names.
- **Hooks** add what the server cannot say: a `SessionStart` hook prints the project's shape and warms the index cache; after each LSP result a hook appends the mentions `grep` finds that the answer did not (Rmd/qmd, roxygen links, functions passed as values) and says whether the index is still building; after each `.R` edit a hook runs the column-name and argument checks, and the bug linters when a project `.lintr` wins.
- **A monitor** tells Claude when the index is ready and when a second repository joins the workspace; the other status events stay in the log for `r-lsp-status`.
- **Shell tools** on Claude's `PATH`: `r-colcheck`, `r-callmap` and `r-lsp-status`.

## Index, completeness and other repositories

The server indexes in the background after it starts (on the first LSP call or `.R` edit). Three things make that visible and safe:

- **Complete answers.** References, definitions, hover, document and workspace symbols and call hierarchy wait for the index and the parse queue before answering (up to 30 s, `R_LSP_INDEX_WAIT`). Hover and document symbols read parse data from the same queue, so before 1.7.4 a cold hover on a function defined in another file answered "no information", which looks like a real empty answer; the post-LSP note now also says, when the index was still building at reply time, that an empty result is not an answer and to repeat the call. Without this, a references call right after opening a file returned 0 results on dplyr, which Claude reads as "nothing calls this".
- **Status events.** The server writes "server started (pid N)" / "indexing N files" / "index ready (N files, T s)" / "added workspace" to `<plugin data dir>/status.log`. The monitor forwards this project's "index ready" and "added workspace" lines to Claude (the others change nothing Claude should do next); a rescan of an unchanged workspace, which the server runs whenever a batch of documents opens and finishes from its cache in a tenth of a second, is not logged at all unless it runs for half a second, so a session's log holds a few lines, not a dozen; `r-lsp-status` prints the log on demand, only those of the most recently started server, and says when that server has exited (after `/reload-plugins` the old server's "index ready" is not the new server's state); the post-LSP hook warns when an answer was given while indexing.
- **Pre-warm.** At session start, a background `Rscript` builds the server's persistent index cache for the project, so the first index is near-instant. (It speeds up the file index, not the parse of package `R/` files, which still takes a few seconds per hundred files.)

**Other repositories.** Claude Code sends the server one root, the session's directory. When Claude opens a file outside every known workspace (another repository, `/add-dir`), the server adds that file's project root (found by `DESCRIPTION`, `.git`, `*.Rproj`, `renv.lock`, `_targets.R`, `.here`) as a workspace, indexes it, and attaches its renv library. Navigation then works there too.

**Subagents** inherit the LSP tool (background subagents since Claude Code v2.1.280) and share the one server; requests queue. Pushed diagnostics reach only the main conversation, so subagents rely on the hooks and on running `r-colcheck` or `Rscript -e 'parse("file.R")'` themselves; the skill says so.

## Column names in tidyverse code

Column names inside `mutate()`, `filter()`, `summarise()`, `select()` and `aes()` live in the data, not the code, so no language server can check them. `r-colcheck FILE.R` does the next best thing statically: it finds the data each frame comes from (a path given to `read_csv()`, `read.csv()`, `fread()`, `readRDS()`, `read_parquet()`, `read_excel()`, `read_dta()`; a built-in dataset; a `tibble()`/`data.frame()`/`tribble()` literal), reads only the column names (one header line, or file metadata; RDS files are loaded only under 20 MB), then tracks the column set through the verbs: `mutate` adds, `rename` swaps, `select` keeps, `summarise` replaces with the grouping columns plus the new names, `pivot_longer` and joins to known frames are computed. A bare name that cannot exist is reported at the line of the step that uses it, not the first line of the pipeline:

```
R/analysis.R:20: 'regoin' is not a column at this point (columns: region, year, amount, customer; data from sales.csv)
```

It stays silent whenever it cannot be sure: a function argument it cannot trace, or a step that makes the set unknowable (`across()`, tidyselect helpers, `pivot_wider()`, `clean_names()`, joins to unknown frames, functions from packages). So that silence is not mistaken for a clean bill, the command line ends with a summary on stderr, `r-colcheck: 12 pipeline step(s) checked against known columns in 1 file(s); 3 step(s) skipped (columns unknowable); 0 finding(s), 0 note(s).`, and exits 1 when there are findings (0 for notes only), so it can gate a CI step. On a fixture of 9 planted column bugs and 35 correct lines using `{{ }}`, `.data`, `across()`, `glue`, `case_when()`, joins and pivots, it reports the 9 and nothing else; on dplyr's own 191 source files it reports no column findings. The post-edit hook runs it after every `.R` edit (about 1 s).

**Through project functions.** Scripts factor logic into helpers: `cleaned <- clean(sales)`, `sales %>% add_share()`. When a known frame is passed to a function defined in the project, the checker binds the parameter to that column set, checks the body at its own lines, and carries the columns of the body's last expression (or `return()`) back to the caller, so `cleaned |> filter(bigg)` is still checked. Both pipes work, with `.` placeholders. Functions are known from the file itself, from files it `source()`s with a literal path, and, in an R package, from every file under `R/`. Named function bodies are also checked once at definition with unknown parameters, so a loader that reads its own data inside a function is checked even when nothing in the file calls it. A finding inside a body names the call site it came from. Calls inside `expect_error()` and the other negative testthat expectations are not followed, since those pass wrong frames on purpose; a column a function tests for with `names()` before using counts as present inside it. Recursion is cut at four levels; functions from installed packages are not followed. Checked against a 236-file package and four other projects (655 R files) with no findings, after fixing what that sweep turned up: empty arguments like `df[, 1]` and forwarded `...` crashed the walkers (previously hidden by an error guard), and columns created earlier in the same `mutate()` were reported as missing by later arguments.

**Through installed packages.** In most teams raw data goes through the team's own package before any dplyr code: `orders <- teamdata::clean_orders(raw, customers)`, where the package wraps a database or an API and its cleaning functions filter, select and rename. The checker reads an installed function's body the way it reads a project function's: the installed function's code is available without running it, the parameters are bound to the caller's column sets, and the columns of the last expression come back out, with the finding's source stated as `teamdata::clean_orders() (read from its code)`. Internal helpers the function calls resolve in the package's own namespace. `pkg::fn` always resolves; a bare name resolves through the packages the file attaches with `library()`. Base, recommended and tidyverse packages are skipped, since their verbs are handled directly and their bodies are C or dynamic. Nothing inside a package body is reported or counted; only the result crosses out. A function whose output cannot be read statically (it builds the frame at run time) leaves the frame unknown as before, and the note names it: `'b' comes from teamdata::opaque(), whose output columns could not be read from its code`, so a skipped pipeline has a cause and a `# cols: KEY` plus `learn` can cover it. The project's renv library is searched first, so in-house packages are found at the versions the project pins. Reading a function loads the package's namespace (its `.onLoad` runs, as it does for hover and go-to-definition); `R_LSP_COLCHECK_PACKAGES=false` turns this off.

**Argument names.** The same walk checks every call to a function defined in the project against that function's formals: a name the function does not have (`calc(df, flows = 1)`), a name R would partial-match silently (`flow_metric =` for `flow_metrics`), an ambiguous prefix, or more positional arguments than there are formals. Functions with `...` accept anything and are not checked; neither are names a default package also defines, calls inside `expect_error()`, or `pkg::f()` for any package but the project's own. On a 42-file package this found 21 test calls, in two files, passing a short argument name that partially matched a longer parameter, which R accepts today and breaks the day a `k` is added. lintr has no equivalent, and the language server does not look at arguments.

### Data that is not a local file: the schema cache

Most real data is not a CSV next to the script: it comes from S3, a database or a project's own loader (`returns <- s3_read("returns/monthly")`), through functions the checker cannot know. For those it uses a schema cache, `.colcheck/schemas.json`, a snapshot of observed column names keyed by the string the code passes to its loader (an S3 key, a URI, a dataset name). The checker is reader-agnostic: it matches any string literal in the loader call against the cache, whatever the function is called, or a `# cols: KEY` comment on the assignment line when the call carries no key. Matching by variable name is deliberately not done, so a frame called `df` never picks up a cache entry by accident. A cache hit is the user's own statement of what the call returns, so it is taken before the function's code is read: a loader whose columns depend on an argument (`fetch_table(date, "orders")`) can never be read from its body, and the cache is how it gets covered, also when that call sits inside a package function the checker is following. `# cols:` applies to the expression the line assigns (the head of its pipeline), never to a sub-expression met on the way, such as the loader's first argument. A helper whose output is the same every call but whose calls carry no literal (`taxonomy(products, categories)`, `default_rates()`) is cached once by name, `r-colcheck learn "pkg::fn" '<expr>'` or `learn "fn" ...`, and every call to it then resolves, namespaced or bare, with no comment on any line; a literal or a `# cols:` on the line still wins. The note for such a function suggests exactly that command. A function key asserts a fixed output, so a loader whose columns depend on an argument keeps its literal keys.

The cache is filled on demand, never from a hook. When a frame comes from a source that is not cached and the code pipes it into verbs, the post-edit note names it and nothing downstream of it is checked; frames that are never used that way are not mentioned:

```
R/report.R:17: 'holdings' comes from a source the checker cannot open (s3_read("holdings/latest")); no cached schema for key "holdings/latest", so column names downstream are not checked. Learn it once: r-colcheck learn "holdings/latest" 's3_read("holdings/latest")'
```

`r-colcheck learn KEY 'EXPR' [--source FILE.R ...]` evaluates the loader expression once in the project, with renv active and the shell's credentials (it is the one r-colcheck command that does not run with `--vanilla`), and records names and types: arrow datasets give their schema without reading rows, data frames and lazy database tables give their names. The code is matched on the key, not on the expression, so when the project's loader materializes a whole table, learn the entry from a cheaper expression that reads the same source, such as `arrow::open_dataset("s3://bucket/orders/")`: the entry still matches every `fetch_table(date, "orders")` in the code. `--source` loads the files that define the loader. Each entry keeps the expression, an observation timestamp and the method, which is what a later refresh needs. `r-colcheck schemas` lists the entries with their age; when findings rest on an entry older than 90 days, the note says so, since a column the data gained since then would look like a bug; `r-colcheck forget KEY` removes one. The skill tells Claude to learn a schema when it is about to write code against an unresolved frame, and to refresh one only on evidence: a reported column that certainly exists, or the user saying the data changed. Nothing refreshes on a timer.

`learn` creates `.colcheck/` with its own `.gitignore` (`*`), so the cache stays local by default; delete that file to commit the cache and share it with the team.

A file that does not parse is reported on the line the parser names, with the offending line and a caret, and nothing in it is checked; the server's diagnostic says the same once the server is running, but the hook's note reaches subagents and the first edits of a session too. A failure of the checker itself is reported, never swallowed: the statement it gave up on gets a line saying so (`r-colcheck hit an error ... and gave up on this statement`), the summary counts them, and `R_LSP_COLCHECK_DEBUG=1` prints the call stack to stderr. Silence means checked and clean.

## The call map

Claude Code's LSP tool hands out the call graph one function at a time: `incomingCalls` or `outgoingCalls` at a cursor position. Nobody maps a package that way, so before a refactor an agent greps a name and reads whole files to work out what calls what. `r-callmap` prints the whole map at once from R's own parser, so Claude reads one listing and opens only the files it names. The point is fewer files read, not speed: grep is faster for a single name.

```
r-callmap: static call map of /path/to/pkg (11 functions in 5 files; 1 Rmd/qmd not parsed).
Static: misses calls built from strings at runtime (eval(parse()), names pasted together), S4 dispatch, ...

R/apply.R
  R/apply.R:1    run_all(xs, how)    [exported]   calls: helper (as value), by_string (by string)   called by: scripts/run.R
  R/apply.R:6    helper(x)           called by: run_all (as value), describe.myclass
  R/apply.R:8    by_string(parts)    called by: run_all (by string)
```

Each line is one definition: file:line, arguments, tags (`[exported]` from NAMESPACE, `[S3 method of print]`, `[S3 generic]`), the project functions it calls and its callers. Calls from code outside any function (tests, scripts, top-level statements) are attributed to the file, with a count when there are several. Functions passed as values (`lapply(x, f)`, `purrr::map(x, f)`, `do.call(f, ...)`, any bare project function name used as a value) are tagged `(as value)`; names passed as strings to `do.call()`, `match.fun()`, `get()` or the apply family, and `Recall()`, are tagged `(by string)`. Calls into packages are left out; `pkg::f()` counts when `pkg` is the project's own package. Files are listed in `Collate` order (else alphabetically), then `tests/`, then the rest; a script's heading shows the files it `source()`s and the project functions its top-level code calls. After the listing come the entry points (functions no project function calls, with the files that do), the unreferenced functions, and any recursion or mutual recursion. R6 methods are listed as `Class$method`, with `self$m()` and `private$m()` calls inside the class resolved; in `_targets.R`, each `tar_target()` is listed as `target name` with the functions its command calls.

`r-callmap NAME` prints the neighbourhood instead: the definition, its callers expanded upward and its callees downward to `--depth` levels (default 2) with each call site's file:line, cycles cut. An S3 method's callers include its generic's callers, through the generic; a generic lists the methods it dispatches to. An unknown name gets the nearest defined names. `--json` gives the map or the neighbourhood as a list of `{name, file, line, args, exported, kind, calls, called_by}` objects. It always exits 0.

Which definition a call reaches: a function in a script or a test file (a file with top-level code) is visible to that file and to the files that `source()` it; functions in `R/` and in files that only define functions are visible everywhere; among several visible definitions of a name, the one in the same file wins. Parameters and local variables that share a project function's name are not counted as uses of it. A file that does not parse is listed in the header as skipped, with the parse error. Git-ignored R files (a `scripts/scratch/` directory) are included, since a refactor has to update them too; the server's startup loader skips them. On a 90-file package (69 functions) a full run takes 0.4 s; on a 236-file one (177 functions), 1.0 s. The session-start context says how many functions the map would list.

Measured on that 90-file package with a planning task (a function two levels below the scripts gains a required argument; list every call site, every signature that must change to thread it through, and the tests), both with the same model and the same tools: an agent told to run `r-callmap NAME` first answered in 22 s over 2 turns and opened no source file; the same agent with grep and Read took 86 s over 11 turns and opened 4 files. Both answers were complete, including the two call sites where the function is passed as a value to `purrr::map()`; the grep agent, having read the files, also noted which calls pass arguments by position. The map is for the plan; the files still get read before the edit.

## What this gives Claude Code

- **Diagnostics**: after every `.R` file edit, syntax errors and likely bugs (see [Diagnostics](#diagnostics)). They reach only the main conversation, and Claude Code may deliver them a turn late; the hooks below are synchronous.
- **Hover**: signature and roxygen text for project functions; the help page for package functions, including renv-pinned ones.
- **Go-to-definition, find references, call hierarchy**: complete for every `.R` file in the project (a package's `R/`, tests, scripts; loaded at startup) and for other repositories Claude opens files in. The note after each result lists what `grep` finds that the answer did not: roxygen `[f()]` links, Rmd, calls through `map()`/`do.call()`, and the other assignments of a script variable the server scoped to one branch.
- **Workspace symbol search**: every R file in the project.
- **After each edit**: the column-name check (`r-colcheck`) for tidyverse code, and the argument-name check for calls to the project's own functions.
- **A call map**: `r-callmap` lists every function with what it calls and what calls it, for planning a refactor from one listing.
- **At session start**: the project's shape and index state, as context.

Claude Code's LSP tool has no completion, rename or code-action operations, so the server's support for those is unused.

## Diagnostics

When a project has no `.lintr` file (and you have no `~/.lintr`), diagnostics come from the plugin's profile in `plugins/r-lsp/lintr/agent_linters.R` instead of lintr's defaults. When a project does have one, lintr uses that file alone (it takes the first config it finds and merges nothing), so the server's diagnostics are that file's rules; the plugin then runs its bug linters from the post-edit hook instead, on the edited file, and they appear in the hook's note after each edit (see below). lintr's defaults are mostly style rules. For an agent those are noise: they push Claude to reformat code it wasn't asked to touch, and they bury real errors. On a 44-line tidyverse script, the defaults gave 49 diagnostics, and 48 were false or style-only. The profile gives 10, all real.

The profile reports:

- **Undefined functions**, in top-level script code as well as inside functions: `could not find function "filterr"`. lintr's own check only looks inside function bodies. Names defined anywhere in the project count as defined, so helpers from another script don't trigger it.
- **Undefined variables** inside functions, and **non-exported** `pkg::fun` calls.
- **Deprecated, defunct, superseded and experimental functions**, judged against the version installed for the project. An experimental function (`group_modify()` in dplyr 1.1) is reported as a warning: its interface may change. The profile reads each function's `lifecycle` signal (or its help page badge), e.g. `cur_data() is deprecated as of dplyr 1.1.0; use pick() instead`. Superseded functions show as information (they still work), deprecated as warnings, defunct as errors.
- **Functions missing from the project's pinned version**, e.g. `list_rbind() is not in purrr 0.3.5, the version installed for this project`. This catches new APIs written into a project that renv pins to an older release.
- lintr's `correctness` and `common_mistakes` rules: `x == NA`, missing packages, duplicate arguments, and similar.

It understands tidyverse code:

- `library(tidyverse)` (and other meta-packages built the same way, like `tidymodels`) attaches its core packages, so `mutate()`, `ggplot()` and `%>%` are known.
- Bare column names inside dplyr, tidyr, ggplot2 and similar calls are not reported. Undefined *functions* inside those calls still are.
- Variables used only inside glue or cli strings (`"{n} rows"`) are not reported as unused.
- cli-style message bullets (`c(i = "...", i = "...")`) are not reported as duplicate arguments.

To turn the profile off and get lintr's defaults, set `R_LSP_AGENT_LINTERS=false` in the environment Claude Code runs in; that also turns off the hook's pass. A project `.lintr` or `.lintr.R` (also under `.github/linters/`) always wins in the server, and so does `~/.lintr`. In that case the session-start context and `r-lsp-status` say `lint profile inactive: <file> takes precedence`, and the post-edit hook runs the profile's bug linters itself: undefined functions, non-exported `pkg::fun`, missing packages, deprecated, superseded and defunct calls, judged against the project's renv library. The hook reports them under `Diagnostics from the plugin's bug linters` (about 2 s on a 340-line file, since the lifecycle check reads the help page of each called package function), and reports a failure to load the profile rather than staying silent. Linters the project's config also enables (lintr's correctness set, `sprintf_linter` for instance) can appear twice, once from the server and once in the note; the deprecated, namespace and undefined-function checks are profile-only and do not. The profile is written for lintr 3.2.0 or newer; on an older lintr it runs in a compatibility mode and the session start and status log say so, since CRAN's macOS binaries for R 4.3 stop at 3.2.0. The profile is found through `R_USER_CONFIG_DIR`, so while it is active lintr does not read a user config at `~/.config/R/lintr/config`; move that to `~/.lintr` to keep it. If you set `R_USER_CONFIG_DIR` yourself, the wrapper leaves it alone and the profile is not used.

## Known limitations

- **Scope of references**: the server itself fully parses a package's `R/` plus files that are open or `source()`d. The plugin loads every other `.R` file in the project the same way at startup (tests, scripts, `data-raw/`; git-ignored files excepted; up to `R_LSP_LOAD_MAX`, default 600), so callers there are in the server's answer from a cold start. Rmd and Quarto files are not loaded; the post-LSP hook lists every mention `grep` finds that the answer does not, line by line, so read the two together.
- **Script variables assigned in a block**: the server scopes an assignment inside `if`/`else` or a loop body to that block, so a script-level variable assigned in two branches gets two disjoint reference sets and a definition that names one branch. The post-LSP note recognizes the case (the assignment is outside every function) and lists the other mentions in the file instead of calling the server's answer complete. A name that is only created as `name = ...` inside `mutate()`, `summarise()` or `tibble()` is a column, which the server does not track; the note says so and lists where it is created. (languageserver 0.3.19 scoped each open document to its package root or `source()` closure and evicted loaded documents outside that closure, so hover, definition and call hierarchy from a script or test into `R/` returned nothing; the plugin's shared-scope and loader patches restore one scope per workspace.)
- **Call graph gaps**: functions passed as values (`map(x, f)`, `lapply`, `do.call`), S3 dispatch (`print(x)` to `print.myclass`) and R6 methods are not edges in the call hierarchy; `r-callmap` lists functions passed as values, S3 methods with their generic, and R6 calls made inside the class. (Since 0.3.19 the server also answered `outgoingCalls` from a shallow index that dropped calls after `!` or a unary minus, inside subscripts and inside `if` conditions, 4 of 26 on a real function; the plugin declines that shortcut so the full parse answers.)
- **The call map is static**: `r-callmap` reads the parse trees and runs nothing. Calls built from strings at runtime (`eval(parse())`, names pasted together), S4 dispatch, R6 calls through an object (`obj$method()`), `super$` calls and code in Rmd/qmd are not edges. A project function named like a base function (`c`, `filter`) attracts every call to that name, which is what R does too. A local variable sharing a project function's name inside a data-masking verb can still show up as an `(as value)` edge.
- **Argument checks cover project functions only**: `r-colcheck` reports wrong, partially matching or surplus arguments in calls to functions defined in the project; a wrong argument name in a call to a package function is not reported by anything.
- **Files outside the project**: a file edited outside the session's repository (a scratch file under `/tmp`) is linted against its own directory, so the repository's `.lintr` does not apply and its functions read as undefined there.
- **Silent parse errors in some files**: an R file whose roxygen contains inline code such as `` `r lifecycle::badge("deprecated")` `` gets no diagnostics at all when it fails to parse (lintr treats the file as a literate document). Rmd chunks with a syntax error are dropped silently.
- **Position off-by-one**: at the first character of a symbol that directly follows `(`, `,` or `$`, the server answers for the punctuation and returns nothing. The post-LSP hook points this out; one character to the right works.
- **Dynamic scope**: Files that use `attach()`, `list2env()`, `sys.source()` or `box::use()`, or that load a package that isn't installed, skip the undefined-function check. The check can't know what those put in scope.
- **Files changed outside Edit/Write**: Claude Code never tells the server about files changed by `git`, `styler` or `Bash`; their results are stale until Claude reads or edits them.
- **Cost**: a package with ~100 R files takes ~20 s and ~1 GB before the first diagnostic; 400 files, over two minutes. The server idles at ~10% of a core.
- **Hover gaps**: Some namespaced calls may not return hover docs depending on how the package structures its help pages.

## Troubleshooting

- **No `R LSP plugin:` lines at session start**: `Rscript` is not on the `PATH` of the shell that started Claude Code, or the project has no R files within three directory levels.
- **No diagnostics after an edit**: run `/plugin` and open the **Errors** tab. `Executable not found` means `R` is not on `PATH`. To test the server by hand: `echo | ~/.claude/plugins/.../r-lsp/bin/r-lsp-wrapper` should print nothing and wait (Ctrl-C to stop); an R error there is the cause.
- **`languageserver` not found**: Run `Rscript --vanilla -e 'find.package("languageserver")'` — should return a path under the global R library, not an renv path.
- **References seem incomplete**: `r-lsp-status` shows whether indexing finished; the post-LSP note lists what `grep` finds outside the server's scope. On a large package the first cross-file answer can take up to 30 s.
- **renv packages not showing hover**: the server may not have finished initializing. Wait a few seconds after opening a project and try again.
- **JSON-RPC errors / server restarts**: check that `renv/activate.R` in your project doesn't print to stdout. The server redirects output during activation, but custom `.Rprofile` hooks could interfere. `claude --debug` logs the server's stderr.
- **No index events from the monitor**: plugin monitors are marked experimental by Anthropic and run only in interactive sessions; `r-lsp-status` reads the same log.

---

## Appendix: Technical context for Claude Code

This section documents the architecture, gotchas, and debugging knowledge discovered during development. Claude Code should reference this when setting up, verifying, or troubleshooting the R LSP.

### Architecture — the full chain

There are five layers between Claude Code and R code intelligence:

1. **Claude Code harness** — The LSP client. When you edit or open an `.R` file, the harness sends LSP events (`textDocument/didOpen`, `textDocument/didChange`) to the server and receives diagnostics back. It also makes LSP requests (hover, go-to-definition, find-references) and feeds results into conversation context.

2. **Plugin config (`.lsp.json`)** — A static JSON declaration that tells Claude Code: "for files ending in `.R`, `.Rmd`, or `.qmd`, spawn the command `r-lsp-wrapper` and talk to it using the Language Server Protocol over stdio." No code — equivalent to a VS Code `settings.json` LSP entry.

3. **Wrapper script (`r-lsp-wrapper`) and server program (`r-lsp-server.R`)** — The bash wrapper sets the environment (lint profile, plugin root) and launches `R --vanilla --no-echo --file=r-lsp-server.R`. The R file applies the plugin's patches to the `languageserver` package, then calls `languageserver::run()`. The `--vanilla` flag prevents `.Rprofile` from running, which is necessary because renv's `.Rprofile` hook would hijack `.libPaths()` before the LSP knows the project root.

4. **R session** — A long-running R process using the global library. One process per Claude Code session. Persists for the session lifetime — not re-launched per file or per request.

5. **`languageserver` R package** — The open-source `REditorSupport/languageserver` package from CRAN. Implements the Language Server Protocol: diagnostics, go-to-definition, hover, find-references, workspace symbols, call hierarchy (and completion, rename, formatting and more that Claude Code never requests). Communicates with Claude Code over stdin/stdout using JSON-RPC.

Around the server, the plugin's hooks (`hooks/`), monitor (`monitors/`), shell tools (`bin/`) and skill (`skills/`) run as separate short processes; the R code behind them is in `scripts/`.

### The renv problem and how the wrapper solves it

**The problem:** When R starts in a directory with an renv project, `.Rprofile` calls `renv::activate()`, which replaces `.libPaths()` with the project-scoped library. `languageserver` is installed globally (not in any project's renv lockfile), so the LSP process can't find it and fails silently.

**Why `--vanilla` alone isn't enough:** Using `--vanilla` skips `.Rprofile`, so renv never activates and `languageserver` loads fine. But then the LSP can only see packages in the global library. Any package installed only in the renv project library (e.g., dplyr, tidyr, arrow) is invisible — hover returns nothing and diagnostics can't resolve imports.

**Why activating renv at startup doesn't work:** The wrapper script runs before the LSP receives the `initialize` request from Claude Code. At startup, the R process doesn't know which project it's serving. The working directory is the user's home directory (confirmed: `lsof -p <pid>` shows `cwd` as `~`), not the project root. Calling `source('renv/activate.R')` with a relative path either fails (no `renv/activate.R` in `~`) or activates the wrong project.

**The solution — monkey-patch `on_initialized`:** `r-lsp-server.R` overrides `languageserver:::on_initialized` before calling `languageserver::run()`. This function is called after the LSP receives the `initialized` notification from Claude Code, at which point `self$rootPath` contains the project root. The patch:

1. `setwd(self$rootPath)` — changes to the project directory (renv determines its project from the working directory)
2. `source(renv/activate.R)` — activates renv, which sets `.libPaths()` to the project library
3. Appends the global library back to `.libPaths()` — so `languageserver` (and its dependencies) remain findable
4. Puts a private library first on `.libPaths()`, holding symlinks to `languageserver` and everything it imports, so a project that pins an old `lintr`, `xml2`, etc. in renv cannot break the server
5. Restarts the idle helper processes (see below)
6. Restores the original working directory
7. Calls the original `on_initialized` handler

**Helper processes:** `languageserver` parses files and runs lintr in separate R processes (callr sessions). They start with the server, before renv is activated, and keep the library paths they started with. Without a restart, diagnostics never see the renv library. Every function from a renv-only package is reported as "no visible global function definition", and hover fails on functions attached with `library()`. After changing `.libPaths()`, the wrapper retires the helpers that aren't running a task, and their replacements inherit the new paths.

**Versions the server loads itself:** A helper process loads `languageserver` and its imports, such as purrr, stringr, rlang and cli, from the global library, and R can't load two versions of a package in one process. So the diagnostics profile reads package facts (exports, lifecycle stage, version) from the project's installed copy on disk, not from what is loaded.

**Critical detail — stdout protection:** renv's `activate.R` can produce output. The LSP communicates over stdout using JSON-RPC, so any stray output corrupts the protocol (symptoms: `Header must provide a Content-Length property` errors, concatenated responses). The server program uses `sink(stderr())` to redirect all output during activation.

### The other patches in `r-lsp-server.R`

- **Complete answers.** The server is single-threaded and indexes in small slices between messages; a cross-file request that arrives early is answered from a partial index. The patch wraps the handlers for references, definition, workspace symbols and call hierarchy: before answering, they run the parse queue and the index to completion (bounded by `R_LSP_INDEX_WAIT`). Claude Code's own request timeout is 60 s.
- **Status events.** `process_events`, the loop body, is replaced on the `LanguageServer` class to detect busy/idle transitions and append events to the status log; `on_initialized` and the workspace-add path mark the start.
- **Other repositories.** `text_document_did_open` is wrapped: a file outside every known workspace gets its project root added through the server's own `add_workspace()` and `load_workspace()`, the same path its unused `workspace/didChangeWorkspaceFolders` handler uses, and the root's renv library is appended to `.libPaths()` without activating it.
- **Why a file, not `-e`:** an inline `-e` program goes through the shell and R's front-end script, which mangles backslashes and has a size limit; `--file=` has neither problem.

### What the LSP indexes

Since `languageserver` 0.3.19 (setting `index_mode`, default `"auto"`), indexing has two levels:

- **Full parse**: a package's `R/` files, open files, and files linked by static `source()` calls. These feed find-references, go-to-definition and incoming calls.
- **Shallow summary**: every other `.R` file in the workspace, packages or not. The summary stores definitions and `source()` edges but not call sites. So those files appear in workspace symbol search, but calls inside them are not found by find-references.

This means:
- `workspaceSymbol` covers the whole project
- `findReferences` and `incomingCalls` miss calls in `tests/`, `data-raw/` and script files, open or not: a file counts as package code only under `R/`. For example, find-references on dplyr's `compute_by()` returns the 10 uses in `R/` and none of the 15 in `tests/`
- The only index settings are `index_mode` (`"auto"` or `"off"`) and include/exclude globs; there is no "fully parse everything" mode
- The persistent index cache (`~/.cache/R/languageserver/workspace-index/`, or the platform equivalent) stores the shallow summaries keyed by file size and mtime; the `SessionStart` hook pre-builds it

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
- `documentSymbol` — top-level functions and variables in any `.R` file
- `workspaceSymbol` — every R file in the project
- `hover` — documentation for local functions and `pkg::fun()` namespaced calls (with renv fix)
- `goToDefinition` — for functions defined in the project's `R/` directory, open files and `source()`d files
- `findReferences`, `incomingCalls`, `outgoingCalls` — same scope, complete once the index is ready; calls through `|>` and `%>%` are followed
- `diagnostics` — syntax errors and the profile's lints after every edit

**Does not work:**
- `findReferences` and call hierarchy into `tests/`, `data-raw/`, scripts or Rmd — the post-LSP hook covers these with `grep`
- `goToImplementation` — the server answers "unknown request"; S3 methods of a generic are not listed
- `goToDefinition` into external package source — jumps to a temporary copy of the installed function, not a local clone
- `hover` on column names inside tidy evaluation (e.g., `mutate(data, new_col = old_col + 1)`) — `old_col` is data-masked, not a real R symbol, so static analysis can't resolve it; `r-colcheck` covers column names instead

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
- Subagents share the one server; the LSP tool opens a file before each request, so a subagent's or another repository's first request is enough to get the file (and its repository) indexed

### Dependencies

The only external dependency is the `languageserver` R package, from CRAN; it installs `lintr`, `jsonlite`, `digest` and the rest of what the hooks use. No fork, no custom builds. The wrapper and hooks are plain bash, the server patches, the diagnostics profile, the column checker and the call map are plain R, and the plugin config is plain JSON. `r-colcheck` uses `readr`, `arrow`, `readxl`, `haven` or `fst` when they are installed and falls back to base R (or stays silent for formats it cannot read) when they are not.

### Testing

`tests/smoke_test.py` starts the wrapper on scratch projects and talks to it over LSP, the way Claude Code does. It checks planted bugs, a tidyverse script, renv-only packages, a project pinning an old `lintr`, a project pinning an old `purrr`, complete references on an 80-file package asked right after opening (with the status events), navigation into a second repository, the column checker on its fixture and through the post-edit hook, the call map on a fixture package (no server), and the session-start and post-LSP hooks. It needs R with `languageserver`, `lintr` and `renv` installed (plus `tidyverse` for the tidyverse scenario):

```bash
python3 tests/smoke_test.py            # all scenarios
python3 tests/smoke_test.py -k renv    # scenarios whose name contains "renv"
```

CI runs it on every push and weekly, because a new `languageserver` release can break the wrapper's patches without any change here. Set `R_LSP_WRAPPER` to test a different wrapper script.
