# SessionStart hook (background): warm two caches for the project. First
# r-colcheck's results cache (scripts/colcheck-lib.R, "results cache"): every
# project file's check, so r-colmap, the hooks' cross-file checks and the
# next session's Data line find it; up to 90 s, what is not reached is
# checked when first asked for. Then the languageserver persistent index
# cache, so the server's first index is near-instant; it uses the same
# WorkspaceIndex class and cache file the server uses, and the server
# reloads a cached entry only while the file's size and mtime still match.
root <- normalizePath(commandArgs(TRUE)[1], mustWork = FALSE)
quit_quietly <- function() quit(save = "no", status = 0)
if (!dir.exists(root)) quit_quietly()
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
tryCatch({
  source(file.path(script_dir, "colcheck-lib.R"), local = TRUE)
  if (cache_enabled()) {
    found <- project_r_files(root)
    deadline <- proc.time()[[3L]] + 90
    n <- 0L
    for (f in c(found$r, found$rmd)) {
      if (proc.time()[[3L]] > deadline) break
      quiet(checked_file(f, root))
      n <- n + 1L
      if (n %% 25L == 0L) cache_flush()   # what is done so far survives an interruption
    }
    cache_flush()
  }
}, error = function(e) NULL)
if (!requireNamespace("languageserver", quietly = TRUE)) quit_quietly()
# A cache that exists is the server's to keep current (it reloads entries
# whose size and mtime still match and re-parses the rest): re-indexing
# everything here would compete with it for up to two minutes.
cache <- tryCatch(file.path(tools::R_user_dir("languageserver", "cache"), "workspace-index",
                            paste0(digest::digest(root, algo = "xxhash64"), ".rds")), error = function(e) "")
if (nzchar(cache) && file.exists(cache)) quit_quietly()
tryCatch({
  ns <- asNamespace("languageserver")
  idx <- ns$WorkspaceIndex$new(root)
  if (!isTRUE(idx$enabled)) quit_quietly()
  idx$discover()
  deadline <- proc.time()[[3L]] + 120
  while (length(idx$pending) && proc.time()[[3L]] < deadline) idx$process_batch()
  idx$cache_dirty <- TRUE
  idx$save_cache()
}, error = function(e) NULL)
