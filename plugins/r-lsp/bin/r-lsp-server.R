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
#   5. Shared scope: scripts and tests see the package's R/ and vice versa
#      (0.3.19 scoped each document to its package root or source() closure).
#   6. Complete outgoing calls: skip the shallow-index shortcut that drops
#      calls inside `!`, subscripts and `if` conditions (0.3.19+).
#   7. Tests and scripts are loaded as documents at startup, like R/, so
#      callers there are found without anything opening the file first.
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

# Patches 2 to 4 use internals that arrived in languageserver 0.3.18 (several
# workspaces) and 0.3.19 (pending-work check, workspace index). On an older
# release they switch themselves off; say so once where r-lsp-status and
# `claude --debug` can show it.
ls_version <- utils::packageVersion("languageserver")
# Marks where this server's events begin: r-lsp-status shows nothing from
# before the last marker, so a previous server's "index ready" is never
# mistaken for the current state after a reload.
status(sprintf("server started (pid %d, languageserver %s)", Sys.getpid(), ls_version))
if (ls_version < "0.3.19") {
  old_note <- paste0("languageserver ", ls_version, " is older than 0.3.19: renv support works, but index status, ",
                     "complete cross-file answers and other-repository support are off. Upgrade with ",
                     "Rscript --vanilla -e 'install.packages(\"languageserver\")'")
  message("r-lsp: ", old_note)
  status(old_note)
}

# Runs on every event-loop turn, so it must survive any languageserver
# version: an internal that is missing (has_work() and the workspace index
# arrived in 0.3.19) reads as "not busy", never as an error.
index_busy <- function(self) {
  tryCatch({
    pm <- self$parse_task_manager
    if (is.function(pm$has_work) && isTRUE(pm$has_work())) return(TRUE)
    for (ws in self$workspaces$values()) {
      idx <- ws$index
      if (!is.null(idx) && isTRUE(idx$enabled) && length(idx$pending)) return(TRUE)
    }
    FALSE
  }, error = function(e) FALSE)
}
index_files <- function(self) {
  n <- 0L
  for (ws in self$workspaces$values()) {
    idx <- ws$index
    if (!is.null(idx) && isTRUE(idx$enabled)) n <- n + idx$files$size()
  }
  n
}
# The first index of a run is logged when it starts. A later rescan (the
# server re-walks the workspace whenever a batch of documents opens) is
# usually a no-op that finishes in a tenth of a second from the cache, so its
# "indexing" line is held back until it has run for half a second; a rescan
# that never gets that far leaves no trace. Both lines or neither: the
# post-LSP note reads an "indexing" line with no "index ready" after it as
# "still building", so a held line is never written on its own.
status_state$announced <- FALSE
status_state$first <- TRUE
note_busy <- function(self, what) {
  if (!status_state$busy) {
    status_state$busy <- TRUE
    status_state$started <- proc.time()[[3L]]
    status_state$what <- what
    status_state$announced <- status_state$first
    status_state$first <- FALSE
    if (status_state$announced) status(paste0(what, ": indexing ", index_files(self), " files"))
  }
}
note_idle <- function(self) {
  if (!status_state$busy) return(invisible())
  elapsed <- proc.time()[[3L]] - status_state$started
  if (!status_state$announced && elapsed >= 0.5) {
    status_state$announced <- TRUE
    status(paste0(status_state$what, ": indexing ", index_files(self), " files"))
  }
  if (!index_busy(self)) {
    status_state$busy <- FALSE
    if (status_state$announced) status(sprintf("index ready (%d files, %.1f s)", index_files(self), elapsed))
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
    # note_idle runs while busy too: it writes a held "indexing" line once
    # a rescan has run long enough to be worth reporting.
    tryCatch({ if (index_busy(self)) note_busy(self, "workspace"); note_idle(self) },
             error = function(e) NULL)
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
    if (!is.function(pm$has_work)) break   # before 0.3.19 there is nothing to wait for
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
  if (!is.function(ns$LanguageServer$public_methods$add_workspace)) stop("several workspaces need languageserver 0.3.18 or newer")
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

# --- 5. scripts and tests see the package ------------------------------------
# Since 0.3.19 a document is scoped to its package root (files under R/) or to
# the files it source()s. A script in scripts/ or a test file is in neither,
# so hover, definition, references and call hierarchy from it into R/ return
# nothing, and callers in scripts and tests are invisible from R/. In this
# plugin's projects scripts and tests always use the package (load_all(),
# library(pkg)), so every open document shares one scope, as before 0.3.19.

tryCatch({
  ws <- ns$Workspace
  if (!is.function(ws$public_methods$document_uris_for_context)) stop("no document scoping in this version (nothing to patch)")
  # the originals also reset the per-scope caches when the document set
  # changes; keep that, or a namespace built early stays stale
  ws$set("public", "document_uris_for_context", function(uri = NULL) {
    all_uris <- self$documents$keys()
    if (is.function(private$refresh_scope_cache)) private$refresh_scope_cache(all_uris)
    all_uris
  }, overwrite = TRUE)
  ws$set("public", "document_uris_for_references", function(definition_uri, context_uri = definition_uri) {
    self$document_uris_for_context(context_uri)
  }, overwrite = TRUE)
}, error = function(e) message("r-lsp: shared-scope patch disabled: ", conditionMessage(e)))

# --- 6. complete outgoing calls ---------------------------------------------
# Since 0.3.19 outgoingCalls is answered from a shallow reference index when
# one exists; it misses calls after `!` or a unary minus, inside subscripts
# and inside `if` conditions (4 of 26 on a real function). Declining the
# shortcut makes the reply walk the full parse, as before.

tryCatch({
  if (!is.function(ns$indexed_outgoing_calls)) stop("no indexed shortcut in this version (nothing to patch)")
  assignInNamespace("indexed_outgoing_calls", function(workspace, item) NULL, ns = "languageserver")
}, error = function(e) message("r-lsp: full outgoing-calls patch disabled: ", conditionMessage(e)))

# --- 7. tests and scripts are loaded, not only R/ ----------------------------
# The server fully parses a package's R/ files at startup and everything else
# only when opened, so a caller in tests/ or scripts/ is invisible to
# references and call hierarchy until something opens that file. Load every
# other R file the index discovered (git-ignored ones excepted) as a document
# too, up to R_LSP_LOAD_MAX files.

load_extra_max <- suppressWarnings(as.integer(Sys.getenv("R_LSP_LOAD_MAX", "600")))
if (is.na(load_extra_max)) load_extra_max <- 600L
extra_loaded <- new.env()   # workspace root -> uris the plugin loaded
git_ignored <- function(root, paths) {
  if (!length(paths)) return(logical())
  out <- tryCatch(suppressWarnings(system2("git", c("-C", shQuote(root), "check-ignore", "--stdin"),
                                           input = paths, stdout = TRUE, stderr = FALSE)),
                  error = function(e) character())
  paths %in% as.character(out)
}

tryCatch({
  orig_lw <- ns$LanguageServer$public_methods$load_workspace
  if (!is.function(orig_lw) || !is.function(ns$LanguageServer$public_methods$load_index_document)) {
    stop("no index-backed loader in this version")
  }
  ns$LanguageServer$set("public", "load_workspace", function(workspace) {
    f <- orig_lw
    environment(f) <- environment()
    f(workspace)
    tryCatch({
      idx <- workspace$index
      if (is.null(idx) || !isTRUE(idx$enabled)) return(invisible(NULL))
      uris <- setdiff(idx$files$keys(), idx$package_source_uris())
      if (!length(uris)) return(invisible(NULL))
      paths <- vapply(uris, ns$path_from_uri, "")
      uris <- uris[!git_ignored(workspace$root, paths)]
      total <- length(uris)
      if (total > load_extra_max) uris <- head(uris, load_extra_max)
      for (uri in uris) {
        if (!idx$summaries$has(uri)) idx$update_path(ns$path_from_uri(uri))
        self$load_index_document(workspace, uri)
      }
      assign(workspace$root %||% "", vapply(uris, ns$index_canonical_uri, ""), envir = extra_loaded)
      status(sprintf("loaded %d test/script files as documents%s", length(uris),
                     if (total > length(uris)) sprintf(" (of %d; raise R_LSP_LOAD_MAX for the rest)", total) else ""))
    }, error = function(e) message("r-lsp: loading tests/scripts failed: ", conditionMessage(e)))
  }, overwrite = TRUE)

  # On every open the server prunes loaded documents that are not package
  # files, open, or in an open file's source() closure. Keep ours.
  orig_prune <- ns$LanguageServer$public_methods$prune_index_documents
  if (is.function(orig_prune)) {
    ns$LanguageServer$set("public", "prune_index_documents", function(workspace) {
      ours <- get0(workspace$root %||% "", envir = extra_loaded, inherits = FALSE)
      if (!length(ours)) {
        f <- orig_prune
        environment(f) <- environment()
        return(f(workspace))
      }
      index <- workspace$index
      if (is.null(index) || !isTRUE(index$enabled)) return(invisible(NULL))
      document_uris <- workspace$documents$keys()
      open_uris <- Filter(function(uri) isTRUE(workspace$documents$get(uri)$is_open), document_uris)
      keep <- union(union(index$package_source_uris(), open_uris), ours)
      for (uri in open_uris) keep <- union(keep, index$source_closure(uri))
      keep <- unique(vapply(keep, ns$index_canonical_uri, character(1L)))
      remove <- document_uris[!vapply(document_uris, function(uri) ns$index_canonical_uri(uri) %in% keep, logical(1L))]
      remove <- Filter(function(uri) index$contains_path(ns$path_from_uri(uri)), remove)
      for (uri in remove) {
        ns$diagnostics_callback(self, uri, NULL, list())
        workspace$documents$remove(uri)
      }
      if (length(remove)) {
        workspace$diagnostics_globals_cache <- NULL
        workspace$type_hierarchy_cache$clear()
        workspace$update_loaded_packages()
      }
      invisible(NULL)
    }, overwrite = TRUE)
  }
}, error = function(e) message("r-lsp: tests/scripts loading patch disabled: ", conditionMessage(e)))

languageserver::run()
