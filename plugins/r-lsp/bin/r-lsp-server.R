# R language server for Claude Code: patches applied before languageserver::run().
#
# Started by bin/r-lsp-wrapper as
#   R --vanilla --no-echo --file=r-lsp-server.R
# (--vanilla keeps renv's .Rprofile from changing .libPaths() before the
# project root is known; a file rather than -e keeps the program readable and
# free of shell escaping).
#
# Patches, in order:
#   1. renv: activate the project's renv once the root is known, keep the
#      server's own packages first on the library path, restart idle helper
#      processes so they see the new paths.
#   2. Index status: write "indexing" / "index ready" events to a status file
#      that the plugin monitor tails and `r-lsp-status` prints.
#   3. Complete answers: references, definition, workspace symbols and call
#      hierarchy wait (bounded) for the index and the parse queue, so Claude
#      never gets a partial list without knowing it.
#   4. Other repositories: a file opened from outside every known workspace
#      gets its project root added as a workspace (indexed like the main one)
#      and its renv library attached.
#
# Every patch is wrapped so that a languageserver release that renames an
# internal disables that patch instead of stopping the server.

`%||%` <- function(a, b) if (is.null(a)) b else a
ns <- asNamespace("languageserver")

# --- status file -----------------------------------------------------------

status_file <- Sys.getenv("R_LSP_STATUS_FILE",
  file.path(Sys.getenv("CLAUDE_PLUGIN_DATA", tempdir()), "status.log"))
status_tag <- Sys.getenv("CLAUDE_PROJECT_DIR", getwd())
status <- function(text) tryCatch({
  dir.create(dirname(status_file), showWarnings = FALSE, recursive = TRUE)
  con <- file(status_file, open = "a")
  on.exit(close(con))
  writeLines(paste0(format(Sys.time(), "%H:%M:%S"), " [", status_tag, "] R LSP: ", text), con)
}, error = function(e) NULL)

status_state <- new.env()
status_state$busy <- FALSE

index_busy <- function(self) {
  if (self$parse_task_manager$has_work()) return(TRUE)
  for (ws in self$workspaces$values()) {
    idx <- ws$index
    if (!is.null(idx) && isTRUE(idx$enabled) && length(idx$pending)) return(TRUE)
  }
  FALSE
}
index_files <- function(self) {
  n <- 0L
  for (ws in self$workspaces$values()) {
    idx <- ws$index
    if (!is.null(idx) && isTRUE(idx$enabled)) n <- n + idx$files$size()
  }
  n
}
note_busy <- function(self, what) {
  if (!status_state$busy) {
    status_state$busy <- TRUE
    status_state$started <- proc.time()[[3L]]
    status(paste0(what, ": indexing ", index_files(self), " files"))
  }
}
note_idle <- function(self) {
  if (status_state$busy && !index_busy(self)) {
    status_state$busy <- FALSE
    status(sprintf("index ready (%d files, %.1f s)", index_files(self),
      proc.time()[[3L]] - status_state$started))
  }
}

# --- 1. renv ---------------------------------------------------------------

# Retire helper sessions that are not running a task, so replacements start
# with the current .libPaths(). A busy session keeps its old paths until the
# task manager's idle timeout replaces it.
refresh_workers <- function(self) {
  for (tm in list(self$parse_task_manager, self$diagnostics_task_manager)) {
    tryCatch({
      p <- tm$.__enclos_env__$private
      for (s in p$sessions) {
        if (!identical(s$get_state(), "busy")) p$retire_session(s)
      }
      p$ensure_min_sessions()
    }, error = function(e) NULL)
  }
}

# Private library holding languageserver and its recursive dependencies, as
# symlinks into the library they are installed in. Put first on .libPaths()
# so a project pinning an old lintr/xml2/... in renv cannot break the server.
server_lib <- function() {
  lib <- file.path(tempdir(), "r-lsp-server-lib")
  if (dir.exists(lib)) return(lib)
  dir.create(lib)
  ip <- installed.packages()
  ip <- ip[!duplicated(rownames(ip)), , drop = FALSE]
  deps <- unique(c("languageserver", unlist(tools::package_dependencies(
    "languageserver", db = ip, recursive = TRUE))))
  for (p in intersect(deps, rownames(ip))) {
    if (!identical(unname(ip[p, "Priority"]), "base")) {
      file.symlink(file.path(ip[p, "LibPath"], p), file.path(lib, p))
    }
  }
  lib
}

activate_renv <- function(self, root) {
  af <- file.path(root, "renv", "activate.R")
  if (!file.exists(af)) return(FALSE)
  gl <- .libPaths()
  srv <- tryCatch(server_lib(), error = function(e) NULL)
  old_wd <- getwd()
  setwd(root)
  sink(stderr())
  tryCatch(
    suppressMessages(source(af, echo = FALSE, print.eval = FALSE)),
    finally = { sink(); setwd(old_wd) }
  )
  # Server packages first, then the project's renv library, then global.
  .libPaths(unique(c(srv, .libPaths(), gl)))
  if (!is.null(srv)) Sys.setenv(R_LSP_SERVER_LIB = srv)
  refresh_workers(self)
  TRUE
}

orig_initialized <- ns$on_initialized
patched_initialized <- function(self, params) {
  if (!is.null(self$rootPath)) {
    tryCatch(activate_renv(self, self$rootPath),
      error = function(e) message("r-lsp: renv activation failed: ", conditionMessage(e)))
  }
  orig_initialized(self, params)
  tryCatch(if (index_busy(self)) note_busy(self, "workspace"), error = function(e) NULL)
}
assignInNamespace("on_initialized", patched_initialized, ns = "languageserver")

# --- 2. index status events --------------------------------------------------

tryCatch({
  orig_pe <- ns$LanguageServer$public_methods$process_events
  ns$LanguageServer$set("public", "process_events", function() {
    f <- orig_pe
    environment(f) <- environment()
    f()
    if (index_busy(self)) note_busy(self, "workspace") else note_idle(self)
  }, overwrite = TRUE)
}, error = function(e) message("r-lsp: status patch disabled: ", conditionMessage(e)))

# --- 3. complete answers -----------------------------------------------------

# Run parse tasks and index batches until nothing is pending, or the time
# bound passes. The server is single-threaded, so this runs inside the
# request handler; the client simply sees a slower, complete answer. The
# client's own request timeout is 60 s.
index_wait_secs <- suppressWarnings(as.numeric(Sys.getenv("R_LSP_INDEX_WAIT", "30")))
if (is.na(index_wait_secs)) index_wait_secs <- 30
drain_index <- function(self) {
  t0 <- proc.time()[[3L]]
  repeat {
    busy <- FALSE
    pm <- self$parse_task_manager
    pm$check_tasks()
    pm$run_tasks()
    if (pm$has_work()) busy <- TRUE
    for (ws in self$workspaces$values()) {
      idx <- ws$index
      if (!is.null(idx) && isTRUE(idx$enabled) && length(idx$pending)) {
        idx$process_batch()
        busy <- TRUE
      }
    }
    if (!busy || (proc.time()[[3L]] - t0) > index_wait_secs) break
    Sys.sleep(0.005)
  }
  note_idle(self)
}

for (h in c("text_document_references", "text_document_definition", "workspace_symbol",
            "text_document_prepare_call_hierarchy", "call_hierarchy_incoming_calls",
            "call_hierarchy_outgoing_calls")) {
  local({
    orig <- get0(h, envir = ns, inherits = FALSE)
    if (is.function(orig)) {
      patched <- function(self, id, params) {
        tryCatch(drain_index(self), error = function(e) NULL)
        orig(self, id, params)
      }
      assignInNamespace(h, patched, ns = "languageserver")
    }
  })
}

# --- 4. other repositories ---------------------------------------------------

find_project_root <- function(path) {
  dir <- dirname(path)
  home <- normalizePath("~", mustWork = FALSE)
  markers <- c("DESCRIPTION", "renv.lock", "_targets.R", ".here", ".git")
  while (nzchar(dir) && dir != dirname(dir) && dir != home) {
    if (any(file.exists(file.path(dir, markers))) ||
        length(list.files(dir, pattern = "[.]Rproj$"))) return(dir)
    dir <- dirname(dir)
  }
  NULL
}

# The renv library of another project, resolved without activating it:
# activation assumes one project per R session and starts a watchdog.
attach_renv_library <- function(root) {
  if (!file.exists(file.path(root, "renv", "activate.R"))) return(invisible())
  if (!requireNamespace("renv", quietly = TRUE)) return(invisible())
  lib <- tryCatch(renv::paths$library(project = root), error = function(e) NULL)
  if (!is.null(lib) && dir.exists(lib) && !(lib %in% .libPaths())) {
    .libPaths(c(.libPaths(), lib))
    TRUE
  } else FALSE
}

tryCatch({
  orig_open <- ns$text_document_did_open
  patched_open <- function(self, params) {
    tryCatch({
      uri <- params$textDocument$uri
      path <- ns$path_from_uri(uri)
      ws <- self$get_workspace(uri)
      inside <- !is.null(ws$root) && ns$path_has_parent(path, ws$root)
      if (!inside) {
        root <- find_project_root(path)
        if (!is.null(root)) {
          root_uri <- ns$path_to_uri(root)
          if (!self$workspaces$has(root_uri)) {
            self$add_workspace(root_uri)
            self$load_workspace(self$get_workspace(root_uri))
            renv_added <- isTRUE(attach_renv_library(root))
            if (renv_added) refresh_workers(self)
            status(paste0("added workspace ", root, if (renv_added) " (with its renv library)" else ""))
            if (index_busy(self)) note_busy(self, basename(root))
          }
        }
      }
    }, error = function(e) message("r-lsp: adding workspace failed: ", conditionMessage(e)))
    orig_open(self, params)
  }
  assignInNamespace("text_document_did_open", patched_open, ns = "languageserver")
}, error = function(e) message("r-lsp: multi-repo patch disabled: ", conditionMessage(e)))

languageserver::run()
