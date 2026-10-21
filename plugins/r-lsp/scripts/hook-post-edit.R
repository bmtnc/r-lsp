# PostToolUse hook body (Edit|Write): read the hook JSON from stdin, run
# colcheck on the edited R file, and hand findings to Claude as
# additionalContext. Prints nothing when there is nothing to say.
`%||%` <- function(a, b) if (is.null(a)) b else a
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
input <- tryCatch(jsonlite::fromJSON(readLines(file("stdin"), warn = FALSE), simplifyVector = FALSE),
                  error = function(e) NULL)
file <- input$tool_input$file_path
if (is.null(file) || !is.character(file) || !grepl("[.][Rr]$", file) || !file.exists(file)) quit(save = "no", status = 0)
root <- Sys.getenv("CLAUDE_PROJECT_DIR", input$cwd %||% getwd())
source(file.path(script_dir, "colcheck-lib.R"), local = TRUE)
findings <- tryCatch(colcheck_files(file, root), error = function(e) list())
if (!length(findings)) quit(save = "no", status = 0)
kinds <- vapply(findings, function(x) x$kind %||% "column", "")
columns <- findings[kinds == "column"]
notes <- findings[kinds == "note"]
from_cache <- any(vapply(columns, function(x) startsWith(x$source %||% "", "cached schema"), TRUE))

clip <- function(lines, n = 15) {
  shown <- head(lines, n)
  paste0(paste(shown, collapse = "\n"),
         if (length(lines) > length(shown)) sprintf("\n... and %d more", length(lines) - length(shown)) else "")
}
parts <- sprintf("r-colcheck (this plugin) on %s:", basename(file))
if (length(columns)) {
  parts <- c(parts,
    "Column names that cannot exist at that point in the pipeline, checked against the columns of the data the code reads:",
    clip(colcheck_format(columns, root)),
    if (from_cache)
      "If the data really has that column, the cached schema is stale: refresh it with `r-colcheck learn KEY 'EXPR'` (the key is named above). Otherwise, if another step adds the column, ignore this."
    else
      "If the data really has that column (e.g. the file on disk is stale or another step adds it), ignore this.")
}
if (length(notes)) {
  parts <- c(parts,
    "Data sources the checker cannot open (nothing downstream of these is checked):",
    clip(colcheck_format(notes, root), 5),
    "The r-lsp skill explains `r-colcheck learn`: run it once per dataset, with --source for the file that defines the loader.")
}
text <- paste(parts, collapse = "\n")
cat(jsonlite::toJSON(list(hookSpecificOutput = list(hookEventName = "PostToolUse", additionalContext = text)),
                     auto_unbox = TRUE), "\n")
