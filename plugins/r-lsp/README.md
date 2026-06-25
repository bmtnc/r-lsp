# r-lsp

R language server for Claude Code with renv support.

Provides diagnostics, hover, go-to-definition, find-references, and outgoing calls for `.R`, `.Rmd`, and `.qmd` files.

## Prerequisites

Install the [`languageserver`](https://github.com/REditorSupport/languageserver) R package globally:

```bash
Rscript --vanilla -e 'install.packages("languageserver", repos = "https://cloud.r-project.org")'
```

Optionally install [`lintr`](https://github.com/r-lib/lintr) globally for linting diagnostics:

```bash
Rscript --vanilla -e 'install.packages("lintr", repos = "https://cloud.r-project.org")'
```

If diagnostics report `.onLoad failed in loadNamespace() for 'lintr'` naming a
missing package (e.g. `there is no package called 'lazyeval'`), a transitive
dependency is absent from your global library — install the named package:

```bash
Rscript --vanilla -e 'install.packages("lazyeval", repos = "https://cloud.r-project.org")'
```

## Installation

```
/plugin marketplace add bmtnc/r-lsp
/plugin install r-lsp
```

## Working across multiple repositories

Claude Code runs **one** R language server per session, rooted at the directory
you launched the session in, and the `languageserver` package only *indexes*
that single root. Consequences:

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
