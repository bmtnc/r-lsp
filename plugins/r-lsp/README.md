# r-lsp

R language server for Claude Code with renv support.

Provides diagnostics, hover, go-to-definition, find-references, and outgoing calls for `.R`, `.Rmd`, and `.qmd` files.

## Prerequisites

Install the [`languageserver`](https://github.com/REditorSupport/languageserver) R package globally:

```bash
Rscript --vanilla -e 'install.packages("languageserver", repos = "https://cloud.r-project.org")'
```

This also installs [`lintr`](https://github.com/r-lib/lintr), which produces the diagnostics.

If diagnostics report `.onLoad failed in loadNamespace() for 'lintr'` naming a
missing package (e.g. `there is no package called 'lazyeval'`), a transitive
dependency is absent from your global library — install the named package:

```bash
Rscript --vanilla -e 'install.packages("lazyeval", repos = "https://cloud.r-project.org")'
```

## Diagnostics

In projects without a `.lintr` file, diagnostics use the plugin's profile (`lintr/agent_linters.R`):
likely bugs only, no style. It understands tidyverse code: `library(tidyverse)`
attaches its core packages, and bare column names in dplyr/tidyr/ggplot2 calls are
not reported. It flags deprecated and superseded functions, and functions missing
from the package versions renv pins for the project. Set `R_LSP_AGENT_LINTERS=false`
to use lintr's defaults instead. See the repository README for details.

## Installation

```
/plugin install r-lsp --marketplace bmtnc/r-lsp
```

Or in two steps on older Claude Code versions:

```
/plugin marketplace add bmtnc/r-lsp
/plugin install r-lsp
```

## What else the plugin does

- Cross-file answers (references, definitions, call hierarchy, workspace symbols) wait for the index, so they are complete when they arrive; the server's "indexing" / "index ready" events reach Claude through a monitor and `r-lsp-status`.
- A `SessionStart` hook prints the project's shape and warms the index cache in the background.
- After each LSP result, a hook adds the mentions `grep` finds in files the server does not index (tests/, scripts, Rmd) and says if the index is still building.
- After each `.R` edit, `r-colcheck` checks bare column names in tidyverse code against the columns of the data the code reads (headers only), and reports names that cannot exist.
- Files opened from another repository get that repository indexed too (see below).

## Working across multiple repositories

Claude Code runs **one** R language server per session, rooted at the directory
you launched the session in. When Claude opens a file from another repository,
the plugin adds that repository's root as a second workspace (indexed, with its
renv library), so definition, references and call hierarchy work there as well.
Earlier versions did not; the notes below describe what still holds:

- **Cross-file navigation** (go-to-definition, find-references, workspace-symbol)
  works only for the repo you launched the session in. Files from other repos
  get per-file features (document outline, hover) but no cross-file navigation.
- **Subagents share the one server** — spawning N agents to explore N repos does
  *not* give each its own LSP context. They all use the single, single-rooted
  server.

**Recommendation: run one session per repository** (separate terminals, tabs, or
git worktrees), launched from each repo's root. Each session then gets a fully
indexed workspace and its own renv library.

As a convenience for files opened from *other* renv projects within a session,
the wrapper appends that project's renv library to `.libPaths()` so package-aware
diagnostics/hover resolve correctly. This does **not** enable cross-repo
navigation — only the indexed root repo supports that.
