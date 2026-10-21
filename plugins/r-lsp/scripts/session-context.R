# SessionStart hook: a few lines of context about the R project for Claude.
# Printed to stdout, which Claude Code adds to Claude's context.
root <- normalizePath(commandArgs(TRUE)[1], mustWork = FALSE)
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
  m <- list.files(root, pattern = "[.](Rmd|rmd|qmd)$", recursive = TRUE, full.names = TRUE)
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
cat("The R language server starts on the first LSP call or .R edit. Cross-file answers (findReferences,",
    "workspaceSymbol, call hierarchy) wait for indexing, up to ~30 s once, then return complete results",
    "for every .R file in the project (R/, tests, scripts; git-ignored files excepted) and the files they",
    "source(); a note after each LSP result lists any mentions grep finds that the answer did not",
    "(Rmd/qmd, roxygen links, functions passed as values). Files from other repositories are indexed on",
    "first open. Column-name and argument mistakes are checked after each edit (r-colcheck).\n")
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
  for (f in rfiles) scan_file(f, "", defs)
  sum(vapply(defs$rows, function(d) d$kind != "target", TRUE))
}, error = function(e) NA_integer_) else NA_integer_
cat(sprintf(paste("`r-callmap` prints a static call map of%s (what calls what, tests and scripts included);",
                  "run it, or `r-callmap NAME`, before a refactor and open only the files it names.\n"),
            if (is.na(n_defs)) " the project's functions" else sprintf(" all %d functions", n_defs)))
