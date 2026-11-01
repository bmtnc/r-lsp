# PostToolUse hook body (Edit|Write): read the hook JSON from stdin, check the
# edited R file (scripts/hook-check-lib.R) and hand findings to Claude as
# additionalContext. Prints nothing when there is nothing to say. Inside a
# subagent (the payload carries agent_id) the bug linters run whatever the
# lint config, because the language server's diagnostics reach only the main
# conversation.
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
input <- tryCatch(jsonlite::fromJSON(readLines(file("stdin"), warn = FALSE), simplifyVector = FALSE),
                  error = function(e) NULL)
# The path as the shell read it (hooks/post-edit.sh exports it): native
# bytes, found in any locale. jsonlite marks its strings UTF-8, and R
# translating one into a C locale loses the non-ASCII characters, so a name
# with an accent in it would not exist for file.exists() and go unchecked.
file <- Sys.getenv("R_LSP_HOOK_FILE", "")
if (!nzchar(file) || !file.exists(file)) file <- input$tool_input$file_path
if (is.null(file) || !is.character(file) || !grepl("[.](R|r|Rmd|rmd|Rmarkdown|qmd|Rnw|rnw)$", file) || !file.exists(file)) quit(save = "no", status = 0)
source(file.path(script_dir, "hook-check-lib.R"), local = TRUE)
root <- hook_root(input)
in_subagent <- is.character(input$agent_id) && nzchar(input$agent_id)
session_dir <- hook_session_dir(input)
# The file's key before the check, so a file rewritten while the check ran is
# recorded as it was checked and comes back at the next Bash or Stop hook.
info <- file.info(file)
stamp <- if (is.na(info$mtime)) NA_character_ else sprintf("%.0f:%.0f", floor(as.numeric(info$mtime)), info$size)
r <- check_r_file(file, root, lint = if (in_subagent) "always" else "auto", in_subagent = in_subagent, session_dir = session_dir)
if (r$any) hook_json(paste(r$lines, collapse = "\n"))
record_checked(file, session_dir, stamps = stamp)
