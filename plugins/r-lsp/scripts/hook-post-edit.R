# PostToolUse hook body (Edit|Write): read the hook JSON from stdin, run
# colcheck on the edited R file, and hand findings to Claude as
# additionalContext. Prints nothing when there is nothing to say.
`%||%` <- function(a, b) if (is.null(a)) b else a
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
input <- tryCatch(jsonlite::fromJSON(readLines(file("stdin"), warn = FALSE), simplifyVector = FALSE),
                  error = function(e) NULL)
file <- input$tool_input$file_path
if (is.null(file) || !is.character(file) || !grepl("[.](R|r|Rmd|rmd|Rmarkdown|qmd|Rnw|rnw)$", file) || !file.exists(file)) quit(save = "no", status = 0)
root <- Sys.getenv("CLAUDE_PROJECT_DIR", input$cwd %||% getwd())
source(file.path(script_dir, "colcheck-lib.R"), local = TRUE)
# A failure of the checker itself is reported, never swallowed: silence must
# mean "checked and clean".
findings <- tryCatch(colcheck_files(file, root), error = function(e) list(list(
  kind = "error", file = file, line = 1L, name = NA_character_, source = NA_character_, columns = character(),
  message = sprintf("r-colcheck could not run on this file (%s). This is a checker bug worth reporting.", conditionMessage(e)))))

# --- the plugin's bug linters, when the server cannot run them ---------------
# A project or home .lintr replaces the profile in the language server (lintr
# takes the first config it finds and merges nothing), so in those projects
# the deprecated, namespace and undefined-function checks run here instead,
# on the edited file, and reach subagents too. A failure is reported, not
# swallowed: silence must mean "checked and clean".
source(file.path(script_dir, "lint-config.R"), local = TRUE)
lint_winner <- tryCatch(lint_config_file(file), error = function(e) NULL)
lint_lines <- character()
lint_failure <- NULL
if (!is.null(lint_winner) && !lint_profile_off()) {
  res <- tryCatch({
    # lintr itself from the global library, as the server runs it; only the
    # packages the code calls are looked up in the project's renv library
    # (prepended next), so lifecycle stages and exports are judged against
    # the versions the project pins.
    loadNamespace("lintr")
    if (file.exists(file.path(root, "renv", "activate.R")) && requireNamespace("renv", quietly = TRUE)) {
      lib <- tryCatch(renv::paths$library(project = root), error = function(e) NULL)
      if (is.character(lib) && length(lib) == 1L && dir.exists(lib)) .libPaths(c(lib, .libPaths()))
    }
    linters <- source(file.path(script_dir, "..", "lintr", "agent_linters.R"), local = new.env())$value
    lints <- lintr::lint(file, linters = linters)
    # Parse errors reach Claude from the server whichever config wins.
    lints <- Filter(function(l) !identical(l$type, "error"), lints)
    vapply(lints, function(l) sprintf("%s:%d:%d: %s [%s]", relative_path(file, root), l$line_number,
                                      l$column_number, l$message, l$linter), "")
  }, error = function(e) e)
  if (inherits(res, "error")) lint_failure <- conditionMessage(res) else lint_lines <- res
}

if (!length(findings) && !length(lint_lines) && is.null(lint_failure)) quit(save = "no", status = 0)
kinds <- vapply(findings, function(x) x$kind %||% "column", "")
columns <- findings[kinds == "column"]
arguments <- findings[kinds == "argument"]
notes <- findings[kinds == "note"]
errors <- findings[kinds == "error"]
syntax <- findings[kinds == "syntax"]
from_cache <- any(vapply(columns, function(x) startsWith(x$source %||% "", "cached schema"), TRUE))

clip <- function(lines, n = 15) {
  shown <- head(lines, n)
  paste0(paste(shown, collapse = "\n"),
         if (length(lines) > length(shown)) sprintf("\n... and %d more", length(lines) - length(shown)) else "")
}
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
    "The r-lsp skill explains `r-colcheck learn`: run it once per dataset, with --source for the file that defines the loader.")
}
if (length(errors)) {
  parts <- c(parts,
    "Statements the checker gave up on (a bug in r-colcheck, not in the code; nothing downstream of them is checked):",
    clip(colcheck_format(errors, root), 5))
}
if (length(lint_lines) || !is.null(lint_failure)) {
  parts <- c(parts,
    sprintf("Diagnostics from the plugin's bug linters on %s (%s takes precedence in the language server, so these never arrive as LSP diagnostics; the file's own lintr rules still do):",
            basename(file), relative_path(lint_winner, root)),
    if (length(lint_lines)) clip(lint_lines) else sprintf("the linters could not run: %s", lint_failure))
}
text <- paste(parts, collapse = "\n")
cat(jsonlite::toJSON(list(hookSpecificOutput = list(hookEventName = "PostToolUse", additionalContext = text)),
                     auto_unbox = TRUE), "\n")
