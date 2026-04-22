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

## Installation

```
/plugin marketplace add bmtnc/r-lsp-setup
/plugin install r-lsp
```
