# SessionStart hook (background): build the languageserver persistent index
# cache for the project, so the server's first index is near-instant. Uses the
# same WorkspaceIndex class and cache file the server uses; the server reloads
# a cached entry only while the file's size and mtime still match.
root <- normalizePath(commandArgs(TRUE)[1], mustWork = FALSE)
quit_quietly <- function() quit(save = "no", status = 0)
if (!dir.exists(root) || !requireNamespace("languageserver", quietly = TRUE)) quit_quietly()
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
