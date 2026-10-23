# PostToolUse hook body (Edit|Write): read the hook JSON from stdin, check the
# edited R file (scripts/hook-check-lib.R) and hand findings to Claude as
# additionalContext. Prints nothing when there is nothing to say. Inside a
# subagent (the payload carries agent_id) the bug linters run whatever the
# lint config, because the language server's diagnostics reach only the main
# conversation.
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
input <- tryCatch(jsonlite::fromJSON(readLines(file("stdin"), warn = FALSE), simplifyVector = FALSE),
                  error = function(e) NULL)
file <- input$tool_input$file_path
if (is.null(file) || !is.character(file) || !grepl("[.](R|r|Rmd|rmd|Rmarkdown|qmd|Rnw|rnw)$", file) || !file.exists(file)) quit(save = "no", status = 0)
source(file.path(script_dir, "hook-check-lib.R"), local = TRUE)
root <- hook_root(input)
in_subagent <- is.character(input$agent_id) && nzchar(input$agent_id)
r <- check_r_file(file, root, lint = if (in_subagent) "always" else "auto", in_subagent = in_subagent)
if (r$any) hook_json(paste(r$lines, collapse = "\n"))
record_checked(file, hook_session_dir(input))
