# PostToolUse hook body (Bash) and Stop/SubagentStop body: the R files named
# as arguments changed in this session and no check has seen them at their
# current modification time (a heredoc, sed -i, styler, roxygenise, git
# checkout, a script that writes code, or a hook that timed out). Check them
# as the post-edit hook would, bug linters included (the language server
# never learns of these changes), and report in one note.
#
#   R_LSP_HOOK_VIA      bash (additionalContext) or stop (a block decision)
#   R_LSP_HOOK_ROOT     the project root
#   R_LSP_HOOK_SESSION  the hooks' session directory (the ledger lives there)
#   R_LSP_HOOK_LIST     a file with one changed file per line, "mtime:size<TAB>path"
#                       as the shell found it (a bare path is accepted too); else the arguments
#   stdin               the hook JSON (stop_hook_active, agent_id)
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
list_file <- Sys.getenv("R_LSP_HOOK_LIST", "")
lines <- if (nzchar(list_file) && file.exists(list_file)) readLines(list_file, warn = FALSE) else commandArgs(TRUE)
lines <- lines[nzchar(lines)]
keyed <- grepl("^[0-9]+:[0-9]+\t", lines)
stamps <- ifelse(keyed, sub("\t.*$", "", lines), NA_character_)
files <- ifelse(keyed, sub("^[^\t]*\t", "", lines), lines)
keep <- !duplicated(files) & file.exists(files)
files <- files[keep]
stamps <- stamps[keep]
if (!length(files)) quit(save = "no", status = 0)
input <- tryCatch(jsonlite::fromJSON(readLines(file("stdin"), warn = FALSE), simplifyVector = FALSE),
                  error = function(e) NULL)
source(file.path(script_dir, "hook-check-lib.R"), local = TRUE)
via <- Sys.getenv("R_LSP_HOOK_VIA", "bash")
root <- Sys.getenv("R_LSP_HOOK_ROOT", hook_root(input))
session_dir <- hook_session_dir(input)
total <- length(files)
# Most recently changed first, so a `git checkout` of hundreds of files
# still checks the ones the command was about.
ord <- order(-as.numeric(file.info(files)$mtime))
files <- files[ord]
stamps <- stamps[ord]
limit <- suppressWarnings(as.integer(Sys.getenv("R_LSP_HOOK_MAX_FILES", "10")))
if (is.na(limit) || limit < 1L) limit <- 10L
checked <- head(files, limit)
rest <- files[-seq_along(checked)]
rel <- function(f) vapply(f, relative_path, "", root = root, USE.NAMES = FALSE)

r <- check_r_files(checked, root, lint = "always", session_dir = session_dir)
left <- total - length(checked)
head_line <- if (via == "stop") {
  sprintf("r-lsp plugin: %d R file%s changed in this session without a post-edit check (changed outside Edit/Write, or the check did not run): %s.",
          total, if (total == 1L) "" else "s", paste(rel(checked), collapse = ", "))
} else {
  sprintf("r-lsp plugin: this command changed %d R file%s (%s)%s. The language server re-reads files changed on disk before its next answer; a file you Read earlier is stale in your context, so Read it again before editing it.",
          total, if (total == 1L) "" else "s", paste(rel(checked), collapse = ", "),
          if (left > 0L) sprintf(" and %d more", left) else "")
}
body <- if (r$any) r$lines else sprintf("Checked%s: no column, argument or parse findings, and no bug-linter diagnostics.",
                                         if (length(checked) == 1L) "" else sprintf(" (%d files)", length(checked)))
tail_line <- if (left > 0L) {
  shown <- head(rel(rest), 20)
  sprintf("Not checked (over the limit of %d per %s): %s%s. Run `r-colcheck FILE...` and `r-lint FILE...` on the ones you changed on purpose.",
          limit, if (via == "stop") "check" else "command", paste(shown, collapse = ", "),
          if (left > length(shown)) sprintf(" and %d more", left - length(shown)) else "")
} else character()
text <- paste(c(head_line, body, tail_line), collapse = "\n")
# The note goes out before the ledger is written, so a hook killed by its
# timeout loses nothing: the files come back at the next hook. The files
# past the limit are recorded too, with the note saying they were not
# checked, so they do not return at every command.
emit <- function() {
  if (via == "stop") {
    # a defect in the code (a parse error, a column or argument name, a
    # lint) or a file left unchecked is worth holding the turn once for; a
    # clean check stays silent; a note about a data source the checker
    # cannot open, a linter that could not run, or the files past the
    # per-check limit are said to the user without holding the turn
    if (!r$any && !left) return(invisible())
    if (isTRUE(r$blocking)) {
      cat(jsonlite::toJSON(list(decision = "block",
                                reason = paste(text, "Fix what is real (or say why a finding is not), then stop again; this check will not hold the turn twice.", sep = "\n")),
                           auto_unbox = TRUE), "\n")
    } else {
      cat(jsonlite::toJSON(list(systemMessage = paste("r-lsp plugin, at the end of the turn (not holding it):", text, sep = "\n")), auto_unbox = TRUE), "\n")
    }
  } else {
    hook_json(text)
  }
}
emit()
record_checked(c(checked, rest), session_dir, stamps = stamps)
