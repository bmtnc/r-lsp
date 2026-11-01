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
# can compare; the size tells a rewrite within the same second apart. The
# Bash and Stop hooks pass the keys they found the files under (`stamps`),
# so a file rewritten while the check ran is recorded as it was checked and
# comes back at the next hook; the edit hook records the file as it is.
record_checked <- function(files, session_dir, stamps = NULL) {
  if (is.null(session_dir) || !length(files)) return(invisible())
  files <- normalizePath(files, mustWork = FALSE)
  if (!is.null(stamps) && length(stamps) == length(files)) {
    ok <- !is.na(stamps) & nzchar(stamps)
    keys <- sprintf("%s\t%s", stamps[ok], files[ok])
  } else {
    info <- file.info(files)
    m <- floor(as.numeric(info$mtime))
    ok <- !is.na(m)
    keys <- sprintf("%.0f:%.0f\t%s", m[ok], info$size[ok], files[ok])
  }
  if (!length(keys)) return(invisible())
  tryCatch(cat(keys, file = file.path(session_dir, "checked.tsv"), sep = "\n", append = TRUE), error = function(e) NULL)
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
# Said once per session (a marker in the session directory): the sentences
# that explain how to read a finding, and the long remedy of a note about a
# data source, which repeats on every edit of the file otherwise.
once <- function(session_dir, what) {
  if (is.null(session_dir)) return(TRUE)
  marker <- file.path(session_dir, paste0("said-", gsub("[^A-Za-z0-9._-]", "_", what)))
  if (file.exists(marker)) return(FALSE)
  tryCatch(file.create(marker), error = function(e) NULL)
  TRUE
}

# Column findings for one name, over several lines, as one line: a typo
# repeated through a file is one mistake to fix.
collapse_columns <- function(columns, root) {
  by_name <- split(columns, vapply(columns, function(x) paste(x$name, x$source), ""))
  lines <- character()
  order_ <- vapply(by_name, function(g) min(vapply(g, function(x) x$line, 1L)), 1L)
  for (g in by_name[order(order_)]) {
    if (length(g) <= 2L) { lines <- c(lines, colcheck_format(g, root)); next }
    x <- g[[1L]]
    near <- x$nearest %||% character()
    lines <- c(lines, sprintf("%s: '%s' is not a column at lines %s (%d times; data from %s%s)%s", relative_path(x$file, root), x$name,
                              paste(sort(unique(vapply(g, function(y) y$line, 1L))), collapse = ", "), length(g), x$source,
                              if (!is.null(x$via)) sprintf(", called from %s", x$via) else "",
                              if (length(near)) sprintf("; did you mean %s?", paste(near, collapse = " or ")) else ""))
  }
  lines
}

# What a change to `file` broke elsewhere. The project files that read what
# it writes, call its functions or load its targets (dependents_of(), from
# the facts r-colcheck's cache keeps of every file) are checked again, and
# the column, logic and argument findings they did not have before are
# reported: the result the cache held for each is the "before", so a second
# edit reports only what it adds; with no cached result, every such finding
# is said once per session. Data flow first (readers, targets), then callers;
# `max_files` files and `budget` seconds at most, the rest named. Silent when
# nothing new turned up: `r-colmap --diff` is the complete account.
impact_lines <- function(file, root, session_dir = NULL, max_files = 6L, budget = 6) {
  deps <- tryCatch(dependents_of(file, root), error = function(e) list())
  if (!length(deps)) return(list(lines = character(), blocking = FALSE))
  started <- Sys.time()
  st <- cache_state(root)
  # a finding's identity: where and what, not its wording (the column list in
  # the message changes with the edit)
  key_of <- function(x) paste(x$file, x$line, x$kind %||% "column", if (is.na(x$name %||% NA_character_)) x$message else x$name)
  reportable <- function(res) Filter(function(x) (x$kind %||% "column") %in% c("column", "logic", "argument"), unclass(res))
  flow <- vapply(deps, function(w) any(grepl("^(reads|loads) ", w)), TRUE)
  files <- c(names(deps)[flow], names(deps)[!flow])
  out <- character()
  fixed <- 0L
  n_new <- 0L
  checked <- character()
  for (f in files) {
    if (length(checked) >= max_files || as.numeric(Sys.time() - started, units = "secs") > budget) break
    old <- st$entries[[f]]$result
    new <- tryCatch(checked_file(f, root), error = function(e) NULL)
    checked <- c(checked, f)
    if (is.null(new)) next
    new_f <- reportable(new)
    new_keys <- vapply(new_f, key_of, "")
    if (!is.null(old)) {
      old_keys <- vapply(reportable(old), key_of, "")
      fresh <- new_f[!(new_keys %in% old_keys)]
      fixed <- fixed + sum(!(old_keys %in% new_keys))
    } else {
      fresh <- new_f[vapply(new_keys, function(k) once(session_dir, paste0("impact-", str_hash(k))), TRUE)]
    }
    if (length(fresh)) {
      n_new <- n_new + length(fresh)
      out <- c(out, sprintf("  %s (%s):", relative_path(f, root), paste(deps[[f]], collapse = "; ")), paste0("    ", colcheck_format(fresh, root)))
    }
  }
  cache_flush()
  if (n_new == 0L && fixed == 0L) return(list(lines = character(), blocking = FALSE))
  rest <- setdiff(files, checked)
  lines <- c(sprintf("Elsewhere, after this change to %s, in the files that read what it writes, call its functions or load its targets (%d checked again):",
                     relative_path(file, root), length(checked)),
             out,
             if (fixed > 0L) sprintf("  %d earlier finding%s in those files no longer appl%s.", fixed, if (fixed == 1L) "" else "s", if (fixed == 1L) "ies" else "y"),
             if (length(rest)) sprintf("  Not checked again (%s): %s; `r-colmap --diff` covers them.",
                                       if (length(checked) >= max_files) "over the limit" else "the time ran out",
                                       paste(vapply(rest, relative_path, "", root = root), collapse = ", ")))
  list(lines = lines, blocking = n_new > 0L)
}

check_r_file <- function(file, root, lint = c("auto", "always", "never"), in_subagent = FALSE, session_dir = NULL, impact = TRUE) {
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
  imp <- if (impact) tryCatch(impact_lines(file, root, session_dir), error = function(e) list(lines = character(), blocking = FALSE))
    else list(lines = character(), blocking = FALSE)

  if (!length(findings) && !length(lints$lines) && is.null(lints$failure) && !length(imp$lines)) return(list(lines = character(), any = FALSE, blocking = FALSE))
  kinds <- vapply(findings, function(x) x$kind %||% "column", "")
  columns <- findings[kinds == "column"]
  arguments <- findings[kinds == "argument"]
  notes <- findings[kinds == "note"]
  errors <- findings[kinds == "error"]
  syntax <- findings[kinds == "syntax"]
  logic <- findings[kinds == "logic"]
  grouped <- findings[kinds == "grouped"]
  from_cache <- any(vapply(columns, function(x) startsWith(x$source %||% "", "cached schema"), TRUE))
  parts <- if (length(findings)) sprintf("r-colcheck (this plugin) on %s:", basename(file)) else character()
  if (length(syntax)) {
    parts <- c(parts, "The file, or a chunk of it, does not parse (R will refuse to run, source or knit it); fix this first:",
               clip(colcheck_format(syntax, root), 3))
  }
  if (length(columns)) {
    parts <- c(parts,
      "Column names that cannot exist at that point in the pipeline, checked against the columns of the data the code reads:",
      clip(collapse_columns(columns, root)),
      if (from_cache && once(session_dir, "stale-cache"))
        "If the data really has that column, the cached schema is stale: refresh it with `r-colcheck learn KEY 'EXPR'` (the key is named above). Otherwise, if another step adds the column, ignore this."
      else if (!from_cache && once(session_dir, "column-finding"))
        "If the data really has that column (e.g. the file on disk is stale or another step adds it), ignore this.")
  }
  if (length(arguments)) {
    parts <- c(parts,
      "Calls to functions defined in this project whose argument names do not match the definition (R only fails at run time, or partial-matches silently):",
      clip(colcheck_format(arguments, root)))
  }
  if (length(logic)) {
    parts <- c(parts, "Steps whose result cannot be what the code means (R runs them without a word):", clip(colcheck_format(logic, root)))
  }
  if (length(grouped)) {
    parts <- c(parts,
      "Steps that run per group because an earlier summarise() left the frame grouped (as often meant as not; nothing to do if the per-group value is the one wanted):",
      clip(colcheck_format(grouped, root), 5))
  }
  if (length(imp$lines)) parts <- c(parts, imp$lines)
  if (length(notes)) {
    # the full note the first time a source or a stop is met in the session; one line after
    note_lines <- vapply(notes, function(x) {
      key <- x$key %||% paste(x$name, x$line)
      if (once(session_dir, paste0("note-", key))) return(sprintf("%s:%s: %s", relative_path(x$file, root), x$line, x$message))
      if (!is.null(x$command)) sprintf("%s:%s: '%s' from %s cannot be opened; nothing downstream is checked (%s)", relative_path(x$file, root), x$line, x$name, x$expr %||% "its source", x$command)
      else sprintf("%s:%s: '%s' is known up to %s; nothing downstream is checked (said in full earlier this session)", relative_path(x$file, root), x$line, x$name, sub("^'[^']*' is known up to ([^,]*),.*$", "\\1", x$message))
    }, "")
    parts <- c(parts,
      "Data sources the checker cannot open (nothing downstream of these is checked):",
      clip(note_lines, 5),
      if (once(session_dir, "learn-advice"))
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
  # what is worth holding a turn for: a defect in the code (a parse error, a
  # column or argument name, a lint), not a source the checker cannot open,
  # a checker bug or a linter that could not run
  list(lines = parts, any = TRUE,
       blocking = length(syntax) + length(columns) + length(arguments) + length(logic) + length(lints$lines) > 0L || isTRUE(imp$blocking))
}

# Check several files within a time budget; the ones not reached are listed,
# never passed over in silence (the caller records the ledger after the note
# is out, so a hook killed by its timeout loses nothing).
check_r_files <- function(files, root, lint, budget = 40, session_dir = NULL) {
  started <- Sys.time()
  out <- character()
  any <- FALSE
  blocking <- FALSE
  unchecked <- character()
  for (f in files) {
    if (as.numeric(Sys.time() - started, units = "secs") > budget) { unchecked <- c(unchecked, f); next }
    r <- check_r_file(f, root, lint, session_dir = session_dir)
    if (r$any) { any <- TRUE; out <- c(out, r$lines) }
    if (isTRUE(r$blocking)) blocking <- TRUE
  }
  if (length(unchecked)) {
    out <- c(out, sprintf("Not checked (the hook's time ran out): %s. Run `r-colcheck FILE` and `r-lint FILE` on them yourself.",
                          paste(vapply(unchecked, relative_path, "", root = root), collapse = ", ")))
    any <- TRUE
    blocking <- TRUE   # unchecked code may hold a defect
  }
  list(lines = out, any = any, blocking = blocking)
}

hook_json <- function(text) {
  cat(jsonlite::toJSON(list(hookSpecificOutput = list(hookEventName = "PostToolUse", additionalContext = text)),
                       auto_unbox = TRUE), "\n")
}
