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
#   7. Tests, scripts and R Markdown/Quarto files are loaded as documents at
#      startup, like R/, so callers there are found without anything opening
#      the file first.
#   8. Files changed outside Edit/Write (git checkout, styler, sed, a
#      heredoc) are re-read before a cross-file request answers from them.
#
# Every patch is wrapped so that a languageserver release that renames an
# internal disables that patch instead of stopping the server, and says so
# in the status file.

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- status file -----------------------------------------------------------

status_file <- Sys.getenv("R_LSP_STATUS_FILE",
  file.path(Sys.getenv("CLAUDE_PLUGIN_DATA", tempdir()), "status.log"))
status_tag <- Sys.getenv("CLAUDE_PROJECT_DIR", getwd())
# Every line names the server that wrote it: two sessions in one project (a
# second terminal, `claude -p` from a script) share this file and their
# lines interleave, so r-lsp-status and the post-LSP hook group by pid.
status_pid <- Sys.getpid()
status <- function(text) tryCatch({
  dir.create(dirname(status_file), showWarnings = FALSE, recursive = TRUE)
  con <- file(status_file, open = "a")
  on.exit(close(con))
  writeLines(paste0(format(Sys.time(), "%H:%M:%S"), " [", status_tag, "] [pid ", status_pid, "] R LSP: ", text), con)
}, error = function(e) NULL)
# A failure, said where `claude --debug` shows it and where r-lsp-status and
# the post-LSP hook read it: one that reaches stderr alone looks like health
# from outside.
note <- function(...) {
  text <- paste0(...)
  message("r-lsp: ", text)
  status(text)
}

status_state <- new.env()
status_state$busy <- FALSE

# The process that started this server (Claude Code's, or a shell it went
# through) and the session, when Claude Code says which: a reader running
# under the same process can then tell this server from another session's
# in the same project.
parent_pid <- tryCatch(trimws(system2("ps", c("-o", "ppid=", "-p", status_pid), stdout = TRUE, stderr = FALSE))[[1L]],
                       error = function(e) "")
if (!isTRUE(grepl("^[0-9]+$", parent_pid))) parent_pid <- "?"
session_id <- Sys.getenv("CLAUDE_CODE_SESSION_ID", "")

# Without languageserver there is no server: say so in the status file
# instead of dying before the first line, which reads as "not started yet"
# (the wrapper runs R --vanilla, so a copy that only a renv or user library
# holds is not seen).
ns <- tryCatch(asNamespace("languageserver"), error = function(e) NULL)
if (is.null(ns)) {
  note("languageserver is not installed in ", paste(.libPaths(), collapse = ":"),
       ": install it with Rscript --vanilla -e 'install.packages(\"languageserver\")'")
  quit(save = "no", status = 1)
}

# Patches 2 to 4 use internals that arrived in languageserver 0.3.18 (several
# workspaces) and 0.3.19 (pending-work check, workspace index). On an older
# release they switch themselves off; say so once where r-lsp-status and
# `claude --debug` can show it.
ls_version <- utils::packageVersion("languageserver")
# Marks where this server's events begin: r-lsp-status shows nothing from
# before the last marker, so a previous server's "index ready" is never
# mistaken for the current state after a reload.
status(sprintf("server started (pid %d, ppid %s, languageserver %s%s)", status_pid, parent_pid, ls_version,
               if (nzchar(session_id)) paste0(", session ", session_id) else ""))
if (ls_version < "0.3.19") {
  note("languageserver ", ls_version, " is older than 0.3.19: renv support works, but index status, ",
       "complete cross-file answers and other-repository support are off. Upgrade with ",
       "Rscript --vanilla -e 'install.packages(\"languageserver\")'")
}

# Which lintr config wins (scripts/lint-config.R, shared with the hook and
# the session-start context). Without it, assume the profile applies.
lint_config_file <- function(path) NULL
lint_profile_off <- function() FALSE
lintr_version_note <- function() NULL
tryCatch(source(file.path(Sys.getenv("R_LSP_PLUGIN_ROOT"), "scripts", "lint-config.R"), local = TRUE),
         error = function(e) NULL)
lintr_note <- lintr_version_note()
if (!is.null(lintr_note)) status(lintr_note)
# Said at startup, where r-lsp-status shows it: a project or home .lintr
# replaces the profile, and the post-edit hook runs the bug linters instead.
note_lint_config <- function(root) {
  tryCatch({
    if (lint_profile_off()) {
      status("lint profile off (R_LSP_AGENT_LINTERS=false): diagnostics use lintr's defaults")
    } else {
      cfg <- lint_config_file(root)
      if (!is.null(cfg)) status(sprintf("lint profile inactive: %s takes precedence; the post-edit hook runs the plugin's bug linters instead", cfg))
    }
  }, error = function(e) NULL)
}

# Runs on every event-loop turn, so it must survive any languageserver
# version: an internal that is missing (has_work() and the workspace index
# arrived in 0.3.19) reads as "not busy", never as an error.
index_busy <- function(self) {
  tryCatch({
    pm <- self$parse_task_manager
    if (is.function(pm$has_work) && isTRUE(pm$has_work())) return(TRUE)
    # languageserver's git master (October 2026) discovers and parses in an
    # index worker: its task manager, and the index's discovering and
    # worker_running flags, are NULL in 0.3.20 and read as "not busy".
    im <- self$index_task_manager
    if (!is.null(im) && is.function(im$has_work) && isTRUE(im$has_work())) return(TRUE)
    for (ws in self$workspaces$values()) {
      idx <- ws$index
      if (!is.null(idx) && isTRUE(idx$enabled) &&
          (length(idx$pending) || isTRUE(idx$discovering) || isTRUE(idx$worker_running))) return(TRUE)
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
  # renv reports a failed bootstrap as a warning, which R holds back until
  # the server's main loop returns: caught here, so it is in the status
  # file while the server runs.
  tryCatch(
    withCallingHandlers(
      suppressMessages(source(af, echo = FALSE, print.eval = FALSE)),
      warning = function(w) { note("renv activation warning: ", conditionMessage(w)); invokeRestart("muffleWarning") }),
    finally = { sink(); setwd(old_wd) }
  )
  added <- setdiff(.libPaths(), gl)
  # Server packages first, then the project's renv library, then global.
  .libPaths(unique(c(srv, .libPaths(), gl)))
  if (!is.null(srv)) Sys.setenv(R_LSP_SERVER_LIB = srv)
  refresh_workers(self)
  # The absence of an error is not success: verify that a library arrived.
  if (length(added)) status(paste0("renv library ", added[[1L]]))
  else note("renv activation added no library: packages the project pins read as not installed in diagnostics ",
            "and hover until renv::restore() has run in the project")
  TRUE
}

orig_initialized <- ns$on_initialized
patched_initialized <- function(self, params) {
  if (!is.null(self$rootPath)) {
    tryCatch(activate_renv(self, self$rootPath),
      error = function(e) note("renv activation failed: ", conditionMessage(e)))
  }
  orig_initialized(self, params)
  if (!is.null(self$rootPath)) note_lint_config(self$rootPath)
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
}, error = function(e) note("status patch disabled: ", conditionMessage(e)))

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
    im <- self$index_task_manager          # git master's index worker; NULL in 0.3.20
    if (!is.null(im) && is.function(im$check_tasks)) im$check_tasks()
    pm$check_tasks()
    pm$run_tasks()
    if (pm$has_work()) busy <- TRUE
    for (ws in self$workspaces$values()) {
      idx <- ws$index
      if (!is.null(idx) && isTRUE(idx$enabled) &&
          (length(idx$pending) || isTRUE(idx$discovering) || isTRUE(idx$worker_running))) {
        idx$process_batch()
        busy <- TRUE
      }
    }
    if (!is.null(im) && is.function(im$run_tasks)) {
      im$run_tasks()
      if (isTRUE(im$has_work())) busy <- TRUE
    }
    if (!busy || (proc.time()[[3L]] - t0) > index_wait_secs) break
    Sys.sleep(0.005)
  }
  note_idle(self)
}

# Every request the LSP tool exposes whose answer depends on the parse
# queue or the index: hover and document symbols read the parse data of the
# file (and of the file that defines the symbol), which is produced by the
# same task queue, so a cold hover answered "no information" for a function
# defined in another file.
for (h in c("text_document_references", "text_document_definition", "workspace_symbol",
            "text_document_prepare_call_hierarchy", "call_hierarchy_incoming_calls",
            "call_hierarchy_outgoing_calls", "text_document_hover", "text_document_document_symbol",
            "text_document_implementation")) {
  local({
    orig <- get0(h, envir = ns, inherits = FALSE)
    if (is.function(orig)) {
      patched <- function(self, id, params) {
        # patch 8: files changed on disk are re-read first, so the parses
        # it queues are among those the drain waits for
        tryCatch(refresh_changed_documents(self), error = function(e) NULL)
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
    }, error = function(e) note("adding workspace failed: ", conditionMessage(e)))
    orig_open(self, params)
  }
  assignInNamespace("text_document_did_open", patched_open, ns = "languageserver")
}, error = function(e) note("multi-repo patch disabled: ", conditionMessage(e)))

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
}, error = function(e) note("shared-scope patch disabled: ", conditionMessage(e)))

# --- 6. complete outgoing calls ---------------------------------------------
# Since 0.3.19 outgoingCalls is answered from a shallow reference index when
# one exists; it misses calls after `!` or a unary minus, inside subscripts
# and inside `if` conditions (4 of 26 on a real function). Declining the
# shortcut makes the reply walk the full parse, as before.

tryCatch({
  if (!is.function(ns$indexed_outgoing_calls)) stop("no indexed shortcut in this version (nothing to patch)")
  assignInNamespace("indexed_outgoing_calls", function(workspace, item) NULL, ns = "languageserver")
}, error = function(e) note("full outgoing-calls patch disabled: ", conditionMessage(e)))

# --- 7. tests, scripts and Rmd/qmd are loaded, not only R/ -------------------
# The server fully parses a package's R/ files at startup and everything else
# only when opened, so a caller in tests/ or scripts/ is invisible to
# references and call hierarchy until something opens that file. Load every
# other R file the index discovered (git-ignored ones excepted) as a document
# too, up to R_LSP_LOAD_MAX files. R Markdown and Quarto files are outside
# the index altogether (it discovers **/*.R), so a vignette's calls were
# invisible until something opened it: they are found here and loaded the
# same way. Document reads the R chunks of a literate file itself, so
# references and call hierarchy reach them as they reach a script.

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
literate_files <- function(root) {
  files <- list.files(root, pattern = "[.](Rmd|rmd|Rmarkdown|qmd)$", recursive = TRUE, full.names = TRUE)
  files <- files[!grepl("/(renv|packrat|[.]git|[.]svn|[.]hg|[.]Rproj[.]user|[.]cache|node_modules|build|dist)/", files)]
  files[!is.na(file.size(files)) & file.size(files) <= 2 * 1024^2]
}
# As load_index_document, without the index bookkeeping (the file is not in
# the index); the parse task reads the chunks because the uri says .Rmd/.qmd.
load_literate_document <- function(self, workspace, uri) {
  if (!is.null(workspace$documents$get(uri, NULL))) return(invisible(NULL))
  path <- ns$path_from_uri(uri)
  stat <- file_stat(path)   # before the read, never after (patch 8)
  content <- tryCatch(stringi::stri_read_lines(path), error = function(e) NULL)
  if (is.null(content)) return(invisible(NULL))
  doc <- ns$Document$new(uri, language = "rmd", version = NULL, content = content)
  workspace$documents$set(uri, doc)
  self$text_sync(uri, document = doc, parse = TRUE)
  if (!is.null(stat)) doc_stat[[uri]] <- stat
  invisible(doc)
}

# The index's own walk, for a languageserver whose discover() returns before
# the file list is filled (git master, October 2026, fills it later from a
# helper process): the same rules as WorkspaceIndex$discover (include and
# exclude globs, size and count limits, entries that resolve outside the
# root), minus the package's R/ files, which the server loads itself. On
# 0.3.20 discover() is synchronous and the list is never empty after it, so
# this walk never runs there.
walk_index_files <- function(idx, root) {
  max_files <- idx$max_files()
  max_bytes <- idx$max_file_bytes()
  real_root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  queue <- root
  visited <- character()
  out <- character()
  while (length(queue) && length(out) < max_files) {
    directory <- queue[[1L]]
    queue <- queue[-1L]
    canonical <- normalizePath(directory, winslash = "/", mustWork = FALSE)
    if (canonical %in% visited) next
    visited <- c(visited, canonical)
    entries <- tryCatch(list.files(directory, all.files = TRUE, no.. = TRUE, full.names = TRUE),
                        error = function(e) character())
    if (!length(entries)) next
    entries <- sort(entries, method = "radix")
    info <- file.info(entries)
    for (i in seq_along(entries)) {
      path <- ns$index_normalize_path(entries[[i]])
      real_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
      if (!identical(real_path, real_root) && !ns$path_has_parent(real_path, real_root)) next
      if (isTRUE(info$isdir[[i]])) {
        if (idx$should_index(path, directory = TRUE)) queue <- c(queue, path)
        next
      }
      if (!idx$should_index(path) || is.na(info$size[[i]]) || info$size[[i]] > max_bytes) next
      if (!is.null(ns$index_package_root(path, root))) next
      out <- c(out, path)
      if (length(out) >= max_files) break
    }
  }
  out
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
      if (!idx$files$size() || isTRUE(idx$discovering)) {
        # the list is filled later by a worker (git master): walk it now
        uris <- vapply(walk_index_files(idx, workspace$root), ns$path_to_uri, "", USE.NAMES = FALSE)
        status(sprintf("index has no file list after discovery (filled later by a worker): walked %s for test/script files myself, %d found",
                       workspace$root, length(uris)))
      } else {
        uris <- setdiff(idx$files$keys(), idx$package_source_uris())
      }
      paths <- vapply(uris, ns$path_from_uri, "")
      uris <- uris[!git_ignored(workspace$root, paths)]
      lit <- literate_files(workspace$root)
      lit <- lit[!git_ignored(workspace$root, lit)]
      lit_uris <- vapply(lit, ns$path_to_uri, "", USE.NAMES = FALSE)
      total <- length(uris) + length(lit_uris)
      if (!total) return(invisible(NULL))
      # one budget for both, R files first
      kept <- head(c(uris, lit_uris), load_extra_max)
      uris <- uris[uris %in% kept]
      lit_uris <- lit_uris[lit_uris %in% kept]
      for (uri in uris) {
        # where an index worker exists (git master) it owns the summaries
        if (is.null(idx$task_manager) && !idx$summaries$has(uri)) idx$update_path(ns$path_from_uri(uri))
        self$load_index_document(workspace, uri)
      }
      for (uri in lit_uris) load_literate_document(self, workspace, uri)
      assign(workspace$root %||% "", vapply(c(uris, lit_uris), ns$index_canonical_uri, ""), envir = extra_loaded)
      status(sprintf("loaded %d test/script files as documents%s%s", length(uris),
                     if (length(lit_uris)) sprintf(", plus %d Rmd/qmd", length(lit_uris)) else "",
                     if (total > length(kept)) sprintf(" (of %d; raise R_LSP_LOAD_MAX for the rest)", total) else ""))
    }, error = function(e) note("loading tests/scripts failed: ", conditionMessage(e)))
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
}, error = function(e) note("tests/scripts loading patch disabled: ", conditionMessage(e)))

# --- 8. files changed outside Edit/Write are re-read -------------------------
# A loaded document keeps the content of its last read: the server reads a
# file once (R/ at startup, tests and scripts through patch 7) and reads it
# again only on didOpen or didSave, or on a didChangeWatchedFiles that
# Claude Code never sends (the server registers no watcher). After a git
# checkout, styler, roxygenise, sed or a heredoc, references, definition and
# hover answer from the old text, with no signal. So before every such
# request, each document the server read from disk (not one the client
# opened: its content is the client's, and Claude Code re-opens the file it
# asks about) is compared with its file by size and mtime; the changed ones
# are read and parsed again, the deleted ones dropped, and drain_index waits
# for the parses. Bounded: past R_LSP_REFRESH_MAX changed files nothing is
# re-read and the status file says so. New files are not found this way (a
# tree walk per request costs too much); the post-LSP hook's grep lists
# callers in them.

doc_stat <- new.env()   # uri -> c(size, mtime) of the file when it was read
file_stat <- function(path) {
  info <- tryCatch(file.info(path, extra_cols = FALSE), error = function(e) NULL)
  if (is.null(info) || is.na(info$size[[1L]]) || isTRUE(info$isdir[[1L]])) return(NULL)
  c(as.numeric(info$size[[1L]]), as.numeric(info$mtime[[1L]]))
}
refresh_max <- suppressWarnings(as.integer(Sys.getenv("R_LSP_REFRESH_MAX", "200")))
if (is.na(refresh_max) || refresh_max < 0L) refresh_max <- 200L
refresh_state <- new.env()
refresh_state$last <- -Inf
refresh_state$skipped <- 0L

refresh_changed_documents <- function(self) {
  now <- proc.time()[[3L]]
  if (now - refresh_state$last < 0.5) return(invisible())   # once per burst of requests
  refresh_state$last <- now
  changed <- list()
  gone <- list()
  for (ws in self$workspaces$values()) {
    for (uri in ws$documents$keys()) {
      doc <- ws$documents$get(uri)
      if (isTRUE(doc$is_open)) next
      path <- ns$path_from_uri(uri)
      stat <- file_stat(path)
      if (is.null(stat)) {
        gone[[length(gone) + 1L]] <- list(ws = ws, uri = uri)
      } else if (!identical(doc_stat[[uri]], stat)) {   # unknown counts as changed
        changed[[length(changed) + 1L]] <- list(ws = ws, uri = uri, path = path, stat = stat)
      }
    }
  }
  n <- length(changed) + length(gone)
  if (!n) return(invisible())
  if (n > refresh_max) {
    if (refresh_state$skipped != n) {
      refresh_state$skipped <- n
      note(n, " loaded files changed or vanished on disk, more than R_LSP_REFRESH_MAX (", refresh_max,
           "): not re-read, so cross-file answers about them are stale until the server restarts")
    }
    return(invisible())
  }
  refresh_state$skipped <- 0L
  for (x in changed) {
    content <- tryCatch(stringi::stri_read_lines(x$path), error = function(e) NULL)
    if (is.null(content)) next
    doc <- x$ws$documents$get(x$uri)
    doc$set_content(doc$version, content)
    doc$call_scan_cache <- NULL
    idx <- x$ws$index
    # the shallow summary now, the full parse through the task queue
    if (!is.null(idx) && isTRUE(idx$enabled)) tryCatch(idx$update_content(x$uri, content), error = function(e) NULL)
    self$text_sync(x$uri, document = doc, parse = TRUE)
    doc_stat[[x$uri]] <- x$stat
  }
  for (x in gone) {
    ns$diagnostics_callback(self, x$uri, NULL, list())
    x$ws$documents$remove(x$uri)
    idx <- x$ws$index
    if (!is.null(idx) && isTRUE(idx$enabled)) tryCatch(idx$remove(x$uri), error = function(e) NULL)
    if (exists(x$uri, envir = doc_stat, inherits = FALSE)) rm(list = x$uri, envir = doc_stat)
  }
  for (ws in self$workspaces$values()) {
    ws$diagnostics_globals_cache <- NULL
    ws$type_hierarchy_cache$clear()
    ws$update_loaded_packages()
  }
  status(sprintf("re-read %d file(s) changed on disk outside the session%s", length(changed),
                 if (length(gone)) sprintf(", dropped %d deleted", length(gone)) else ""))
}

# The stat of every document the server reads is taken as it is loaded.
# The original must be a top-level object: R6 encloses a method in the
# object's environment, whose parent chain ends in the namespace and the
# global environment, so a local() capture is invisible to it.
tryCatch({
  orig_lid <- ns$LanguageServer$public_methods$load_index_document
  if (!is.function(orig_lid)) stop("no index-backed loader in this version")
  ns$LanguageServer$set("public", "load_index_document", function(workspace, uri) {
    f <- orig_lid
    environment(f) <- environment()
    stat <- file_stat(ns$path_from_uri(uri))
    doc <- f(workspace, uri)
    if (!is.null(doc) && !is.null(stat)) doc_stat[[uri]] <- stat
    invisible(doc)
  }, overwrite = TRUE)
}, error = function(e) note("re-read patch disabled: ", conditionMessage(e)))

languageserver::run()
# A crash inside the main loop is logged to stderr and run() returns;
# r-lsp-status tells a dead pid apart either way, this line completes the log.
status("server exited")
