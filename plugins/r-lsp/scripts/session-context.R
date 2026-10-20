# SessionStart hook: a few lines of context about the R project for Claude.
# Printed to stdout, which Claude Code adds to Claude's context.
root <- normalizePath(commandArgs(TRUE)[1], mustWork = FALSE)
rfiles <- list.files(root, pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE)
rfiles <- rfiles[!grepl("/(renv|packrat|[.]git|node_modules)/", rfiles)]
tests <- rfiles[grepl("/tests?/", rfiles)]
rmd <- list.files(root, pattern = "[.](Rmd|rmd|qmd)$", recursive = TRUE)
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
cat("The R language server starts on the first .R edit. Cross-file answers (findReferences,",
    "workspaceSymbol, call hierarchy) wait for indexing, up to ~30 s once, then return complete",
    "results for R/ files; a note after each LSP result lists any mentions grep finds in files the",
    "server does not index (tests/, scripts, Rmd). Files from other repositories are indexed on",
    "first open. Column-name mistakes in tidyverse code are checked after each edit (r-colcheck).\n")
