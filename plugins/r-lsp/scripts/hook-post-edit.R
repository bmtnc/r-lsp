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
lines <- colcheck_format(findings, root)
shown <- head(lines, 15)
text <- paste0(
  "r-colcheck (this plugin): column names used in ", basename(file),
  " that cannot exist at that point in the pipeline, checked against the columns of the data the code reads:\n",
  paste(shown, collapse = "\n"),
  if (length(lines) > length(shown)) sprintf("\n... and %d more", length(lines) - length(shown)) else "",
  "\nIf the data really has that column (e.g. the file on disk is stale or another step adds it), ignore this.")
cat(jsonlite::toJSON(list(hookSpecificOutput = list(hookEventName = "PostToolUse", additionalContext = text)),
                     auto_unbox = TRUE), "\n")
