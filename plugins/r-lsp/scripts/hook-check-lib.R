# Shared by the post-edit, post-bash and stop hooks: run r-colcheck and, when
# the language server will not report on the file, the plugin's bug linters,
# on one or more R files, and word the note Claude gets. Also the ledger of
# checked files, <session dir>/checked.tsv ("mtime:size<TAB>path" per line),
# which the Bash and Stop hooks read to find files changed in this session
# that no check has seen at their current modification time and size.
#
# Sourced with local = TRUE by the hook entry scripts; expects `script_dir`.
`%||%` <- function(a, b) if (is.null(a)) b else a
source(file.path(script_dir, "colcheck-lib.R"), local = TRUE)
source(file.path(script_dir, "lint-config.R"), local = TRUE)

hook_root <- function(input) Sys.getenv("CLAUDE_PROJECT_DIR", input$cwd %||% getwd())

# The hooks' session directory: $CLAUDE_PLUGIN_DATA/sessions/<session id>
# (hooks/hook-lib.sh computes the same path).
hook_session_dir <- function(input) {
  d <- Sys.getenv("R_LSP_HOOK_SESSION", "")
  if (nzchar(d)) return(d)
  id <- input$session_id %||% Sys.getenv("CLAUDE_CODE_SESSION_ID", "")
  if (!is.character(id) || !nzchar(id)) return(NULL)
  # a path component, as hooks/hook-lib.sh makes it
  id <- substr(gsub("[^A-Za-z0-9._-]", "_", id), 1L, 120L)
  if (id %in% c("", ".", "..")) return(NULL)
  base <- Sys.getenv("CLAUDE_PLUGIN_DATA", "")
  if (!nzchar(base)) base <- file.path(Sys.getenv("TMPDIR", "/tmp"), "r-lsp-hooks")
  d <- file.path(base, "sessions", id)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(d)) d else NULL
}

# "mtime:size<TAB>path": whole seconds, as `stat` prints them, so the shell
# can compare; the size tells a rewrite within the same second apart.
record_checked <- function(files, session_dir) {
  if (is.null(session_dir) || !length(files)) return(invisible())
  files <- normalizePath(files, mustWork = FALSE)
  info <- file.info(files)
  m <- floor(as.numeric(info$mtime))
  ok <- !is.na(m)
  if (!any(ok)) return(invisible())
  tryCatch(cat(sprintf("%.0f:%.0f\t%s", m[ok], info$size[ok], files[ok]), file = file.path(session_dir, "checked.tsv"), sep = "\n", append = TRUE),
           error = function(e) NULL)
  invisible()
}

clip <- function(lines, n = 15) {
  shown <- head(lines, n)
  paste0(paste(shown, collapse = "\n"),
         if (length(lines) > length(shown)) sprintf("\n... and %d more", length(lines) - length(shown)) else "")
}

# The bug linters, loaded once per process: lintr from the global library,
# the packages the code calls from the project's renv library (prepended), so
# lifecycle stages and exports are judged against the versions the project pins.
lint_state <- new.env()
lint_setup <- function(root) {
  if (!is.null(lint_state$linters) || !is.null(lint_state$failure)) return(invisible())
  res <- tryCatch({
    loadNamespace("lintr")
    if (file.exists(file.path(root, "renv", "activate.R")) && requireNamespace("renv", quietly = TRUE)) {
      lib <- tryCatch(renv::paths$library(project = root), error = function(e) NULL)
      if (is.character(lib) && length(lib) == 1L && dir.exists(lib)) .libPaths(c(lib, .libPaths()))
    }
    source(file.path(script_dir, "..", "lintr", "agent_linters.R"), local = new.env())$value
  }, error = function(e) e)
  if (inherits(res, "error")) lint_state$failure <- conditionMessage(res) else lint_state$linters <- res
  invisible()
}

lint_file <- function(file, root) {
  lint_setup(root)
  if (!is.null(lint_state$failure)) return(list(lines = character(), failure = lint_state$failure))
  res <- tryCatch({
    lints <- lintr::lint(file, linters = lint_state$linters)
    # A parse error (lintr's own pseudo-lint, linter "error") is dropped:
    # r-colcheck's syntax finding and the server carry it already. Lints of
    # type "error" from a real linter stay: a defunct call (lifecycle_linter)
    # is the one class that is certain to crash, so it must reach Claude.
    lints <- Filter(function(l) !identical(l$linter, "error"), lints)
    vapply(lints, function(l) sprintf("%s:%d:%d: %s [%s]", relative_path(file, root), l$line_number,
                                      l$column_number, l$message, l$linter), "")
  }, error = function(e) e)
  if (inherits(res, "error")) list(lines = character(), failure = conditionMessage(res)) else list(lines = res, failure = NULL)
}

# Check one R file. `lint`: "auto" runs the bug linters only when a project or
# home .lintr replaces the profile in the language server (the server reports
# the profile otherwise); "always" runs them whatever the config, for files
# the server will not report on (changed through Bash, or edited inside a
# subagent, whose diagnostics stay in the main conversation). Returns the
# note's lines (none when the file is clean) and whether anything was found.
check_r_file <- function(file, root, lint = c("auto", "always", "never"), in_subagent = FALSE) {
  lint <- match.arg(lint)
  # The file's own project, not the session's: a file in another repository
  # (opened after /add-dir, or edited by a subagent) or in a package nested
  # under the session root is checked against its own R/, .colcheck.json,
  # schema cache and renv library, as `r-colcheck FILE` checks it. The
  # session root bounds the walk, so a file with no nearer marker keeps it.
  root <- tryCatch(colcheck_root(file, within = root), error = function(e) root)
  # A failure of the checker itself is reported, never swallowed: silence must
  # mean "checked and clean".
  findings <- tryCatch(colcheck_files(file, root), error = function(e) list(list(
    kind = "error", file = file, line = 1L, name = NA_character_, source = NA_character_, columns = character(),
    message = sprintf("r-colcheck could not run on this file (%s). This is a checker bug worth reporting.", conditionMessage(e)))))
  lint_winner <- tryCatch(lint_config_file(file), error = function(e) NULL)
  run_lint <- !lint_profile_off() && (lint == "always" || (lint == "auto" && !is.null(lint_winner)))
  lints <- if (run_lint) lint_file(file, root) else list(lines = character(), failure = NULL)

  if (!length(findings) && !length(lints$lines) && is.null(lints$failure)) return(list(lines = character(), any = FALSE))
  kinds <- vapply(findings, function(x) x$kind %||% "column", "")
  columns <- findings[kinds == "column"]
  arguments <- findings[kinds == "argument"]
  notes <- findings[kinds == "note"]
  errors <- findings[kinds == "error"]
  syntax <- findings[kinds == "syntax"]
  from_cache <- any(vapply(columns, function(x) startsWith(x$source %||% "", "cached schema"), TRUE))
  parts <- if (length(findings)) sprintf("r-colcheck (this plugin) on %s:", basename(file)) else character()
  if (length(syntax)) {
    parts <- c(parts, "The file, or a chunk of it, does not parse (R will refuse to run, source or knit it); fix this first:",
               clip(colcheck_format(syntax, root), 3))
  }
  if (length(columns)) {
    parts <- c(parts,
      "Column names that cannot exist at that point in the pipeline, checked against the columns of the data the code reads:",
      clip(colcheck_format(columns, root)),
      if (from_cache)
        "If the data really has that column, the cached schema is stale: refresh it with `r-colcheck learn KEY 'EXPR'` (the key is named above). Otherwise, if another step adds the column, ignore this."
      else
        "If the data really has that column (e.g. the file on disk is stale or another step adds it), ignore this.")
  }
  if (length(arguments)) {
    parts <- c(parts,
      "Calls to functions defined in this project whose argument names do not match the definition (R only fails at run time, or partial-matches silently):",
      clip(colcheck_format(arguments, root)))
  }
  if (length(notes)) {
    parts <- c(parts,
      "Data sources the checker cannot open (nothing downstream of these is checked):",
      clip(colcheck_format(notes, root), 5),
      "Run the learn command once per dataset (--source names the file that defines the loader), or `r-colcheck discover` for every uncached source in the project and `r-colcheck learn --all` to learn them in one run; never from a hook.")
  }
  if (length(errors)) {
    parts <- c(parts,
      "Statements the checker gave up on (a bug in r-colcheck, not in the code; nothing downstream of them is checked):",
      clip(colcheck_format(errors, root), 5))
  }
  if (length(lints$lines) || !is.null(lints$failure)) {
    why <- if (!is.null(lint_winner) && !in_subagent)
      sprintf("%s takes precedence in the language server, so these never arrive as LSP diagnostics; the file's own lintr rules still do",
              relative_path(lint_winner, root))
    else if (in_subagent) "the language server's diagnostics stay in the main conversation; this note is a subagent's check"
    else "the language server does not report on this file: it only sees Edit/Write changes made in the main conversation"
    parts <- c(parts,
      sprintf("Diagnostics from the plugin's bug linters on %s (%s):", basename(file), why),
      if (length(lints$lines)) clip(lints$lines) else sprintf("the linters could not run: %s", lints$failure))
  }
  list(lines = parts, any = TRUE)
}

# Check several files within a time budget; the ones not reached are listed,
# never passed over in silence (the caller records the ledger after the note
# is out, so a hook killed by its timeout loses nothing).
check_r_files <- function(files, root, lint, budget = 40) {
  started <- Sys.time()
  out <- character()
  any <- FALSE
  unchecked <- character()
  for (f in files) {
    if (as.numeric(Sys.time() - started, units = "secs") > budget) { unchecked <- c(unchecked, f); next }
    r <- check_r_file(f, root, lint)
    if (r$any) { any <- TRUE; out <- c(out, r$lines) }
  }
  if (length(unchecked)) {
    out <- c(out, sprintf("Not checked (the hook's time ran out): %s. Run `r-colcheck FILE` and `r-lint FILE` on them yourself.",
                          paste(vapply(unchecked, relative_path, "", root = root), collapse = ", ")))
    any <- TRUE
  }
  list(lines = out, any = any)
}

hook_json <- function(text) {
  cat(jsonlite::toJSON(list(hookSpecificOutput = list(hookEventName = "PostToolUse", additionalContext = text)),
                       auto_unbox = TRUE), "\n")
}
