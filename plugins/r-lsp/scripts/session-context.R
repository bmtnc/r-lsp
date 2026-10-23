# SessionStart hook: a few lines of context about the R project for Claude.
# Printed to stdout, which Claude Code adds to Claude's context.
root <- normalizePath(commandArgs(TRUE)[1], mustWork = FALSE)
# "subagent": the same context for a subagent, which starts from nothing, as
# JSON additionalContext (SubagentStart does not read plain stdout), with the
# lines that differ for it.
subagent <- identical(commandArgs(TRUE)[2], "subagent")
out <- character()
cat <- function(..., sep = " ") out <<- c(out, paste(..., sep = sep))
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
# The same discovery r-callmap uses, so the counts agree. stderr is discarded
# by the hook, so a broken library must not cost the lines below.
have_callmap <- tryCatch({
  source(file.path(script_dir, "colcheck-lib.R"), local = TRUE)
  source(file.path(script_dir, "callmap-lib.R"), local = TRUE)
  TRUE
}, error = function(e) FALSE)
found <- if (have_callmap) callmap_files(root) else {
  r <- list.files(root, pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE)
  m <- list.files(root, pattern = "[.](Rmd|rmd|Rmarkdown|qmd|Rnw|rnw)$", recursive = TRUE, full.names = TRUE)
  skip <- "/(renv|packrat|[.]git|node_modules)/"
  list(r = r[!grepl(skip, r)], rmd = m[!grepl(skip, m)])
}
rfiles <- found$r
tests <- rfiles[grepl("/tests?/", rfiles)]
rmd <- found$rmd
is_pkg <- file.exists(file.path(root, "DESCRIPTION"))
name <- if (is_pkg) tryCatch(read.dcf(file.path(root, "DESCRIPTION"), fields = "Package")[1, 1],
  error = function(e) NA) else NA
kind <- if (is_pkg && !is.na(name)) sprintf("R package '%s'", name) else
  if (file.exists(file.path(root, "_targets.R"))) "targets pipeline" else
  if (file.exists(file.path(root, "app.R"))) "Shiny app" else "R scripts project"
renv <- file.exists(file.path(root, "renv.lock"))
cache <- tryCatch(file.path(tools::R_user_dir("languageserver", "cache"), "workspace-index",
  paste0(digest::digest(root, algo = "xxhash64"), ".rds")), error = function(e) "")
cat(sprintf("R LSP plugin: %s, %d R files (%d under tests/), %d Rmd/qmd%s. Index cache %s.\n",
  kind, length(rfiles), length(tests), length(rmd), if (renv) ", renv project" else "",
  if (nzchar(cache) && file.exists(cache)) "warm" else "cold (building in the background)"))
cat("The R language server starts on the first LSP call or .R/.Rmd edit. Answers that need the index (findReferences,",
    "goToDefinition, hover, documentSymbol, workspaceSymbol, call hierarchy) wait for it, up to ~30 s once, then are complete",
    "for every .R, .Rmd and .qmd file in the project (R/, tests, scripts, vignettes; git-ignored files excepted) and the files they",
    "source(); a note after each LSP result lists any mentions grep finds that the answer did not",
    "(roxygen links, Rmd/qmd prose; for the call hierarchy also functions passed as values). Files from other",
    "repositories are indexed on",
    "first open. Column-name and argument mistakes are checked after each change to an R file, through Edit, Write or a",
    "Bash command (r-colcheck); r-callmap --diff lists what a change touches and the tests to run, r-colmap the data sources",
    "and their columns.\n")
if (subagent) {
  cat("You are a subagent: the language server's diagnostics stay in the main conversation. Your check is the plugin's note",
      "after each Edit, Write or Bash command that changes an R file (r-colcheck column and argument findings, parse errors,",
      "and the plugin's bug linters: undefined functions, non-exported pkg::fun, deprecated or superseded calls). Silence",
      "there means checked and clean. Prefer Edit/Write for R files; a change made through Bash is checked after the",
      "command, up to 10 files, and the server does not see it.\n")
}
# Diagnostics: which lintr config the server will use. A project or home
# .lintr replaces the plugin's profile; say so, or a missing diagnostic reads
# as a clean file.
lint_config_file <- function(path) NULL
lint_profile_off <- function() FALSE
lintr_version_note <- function() NULL
tryCatch(source(file.path(script_dir, "lint-config.R"), local = TRUE), error = function(e) NULL)
cfg <- tryCatch(lint_config_file(root), error = function(e) NULL)
if (lint_profile_off()) {
  cat("Lint profile off (R_LSP_AGENT_LINTERS=false): after-edit diagnostics are lintr's defaults.\n")
} else if (!is.null(cfg)) {
  cat(sprintf(paste("Lint profile inactive: %s takes precedence in the language server, so LSP diagnostics are that",
                    "file's rules. The plugin's bug linters (undefined functions, non-exported pkg::fun, deprecated,",
                    "superseded or experimental calls, missing packages) run from the post-edit hook instead and appear in its note.\n"),
              cfg))
}
note <- lintr_version_note()
if (!is.null(note)) cat(note, "\n")
# Counting definitions parses every file: ~0.3 s for 240 files. Above 500
# files the sentence goes without the count to keep the hook fast.
n_defs <- if (have_callmap && length(rfiles) <= 500L) tryCatch({
  defs <- new_defs()
  for (f in c(rfiles, rmd)) scan_file(f, "", defs)
  sum(vapply(defs$rows, function(d) d$kind != "target", TRUE))
}, error = function(e) NA_integer_) else NA_integer_
cat(sprintf(paste("`r-callmap` prints a static call map of%s (what calls what, tests, scripts and Rmd/qmd chunks included);",
                  "run it, or `r-callmap NAME`, before a refactor and open only the files it names; `r-callmap --diff` lists what",
                  "the working tree's changes touch and the tests that reach them. Also on PATH: `r-colmap` (every data source, its",
                  "columns and where each is used), `r-colcheck` (`discover`, `learn --all` for data that is not a local file),",
                  "`r-lint FILE` (the bug profile), `r-lsp-status`. The r-refactor, r-review and r-data skills give the order of",
                  "operations; the r-lsp skill is the reference.\n"),
            if (is.na(n_defs)) " the project's functions" else sprintf(" all %d functions", n_defs)))
text <- paste(out, collapse = "")
if (subagent) {
  base::cat(jsonlite::toJSON(list(hookSpecificOutput = list(hookEventName = "SubagentStart", additionalContext = sub("\n$", "", text))),
                             auto_unbox = TRUE), "\n")
} else base::cat(text)
