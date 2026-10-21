# PostToolUse hook body for the LSP tool: say what the language server's
# answer does not cover.
#
#  - If the server is still indexing this project, say so (the answer may
#    be incomplete; it is complete once the index is ready).
#  - For findReferences / incomingCalls: grep the project for the symbol and
#    list every mention the server's answer did not return, line by line, in
#    whatever file (tests, scripts, roxygen links, comments, other
#    assignments of the same variable). Files git ignores are skipped.
#  - For goToDefinition with no result: list lines that look like a
#    definition of the symbol anywhere in the project.
#  - For a null answer at the first character of a symbol: suggest retrying
#    one character to the right (a known off-by-one in the server).
`%||%` <- function(a, b) if (is.null(a)) b else a
input <- tryCatch(jsonlite::fromJSON(readLines(file("stdin"), warn = FALSE), simplifyVector = FALSE),
                  error = function(e) NULL)
if (is.null(input)) quit(save = "no", status = 0)
ti <- input$tool_input %||% list()
op <- ti$operation %||% ""
file <- ti$filePath %||% ""
root <- Sys.getenv("CLAUDE_PROJECT_DIR", input$cwd %||% getwd())
if (nzchar(file) && !file.exists(file) && file.exists(file.path(root, file))) file <- file.path(root, file)
resp <- input$tool_response
resp_text <- if (is.character(resp)) paste(resp, collapse = "\n") else
  if (is.list(resp) && is.character(resp$formatted)) resp$formatted else
  tryCatch(as.character(jsonlite::toJSON(resp, auto_unbox = TRUE)), error = function(e) "")
notes <- character()

# --- index state ---------------------------------------------------------------
status_file <- Sys.getenv("R_LSP_STATUS_FILE",
  file.path(Sys.getenv("CLAUDE_PLUGIN_DATA", ""), "status.log"))
if (file.exists(status_file)) {
  lines <- tryCatch(readLines(status_file, warn = FALSE), error = function(e) character())
  mine <- grep(paste0("[", root, "]"), lines, fixed = TRUE, value = TRUE)
  if (length(mine)) {
    last <- tail(mine, 1)
    if (grepl(": indexing ", last, fixed = TRUE) && !grepl("index ready", last, fixed = TRUE)) {
      notes <- c(notes, paste0("The R language server is still indexing this project (",
        sub("^.*R LSP: ", "", last), "). This answer may be incomplete; the same call returns complete",
        " results once `r-lsp-status` shows 'index ready'."))
    }
  }
}

# --- symbol at the position ----------------------------------------------------
ident <- "[A-Za-z0-9._]"
symbol_at <- function(path, line, col) {
  if (!file.exists(path) || is.null(line) || is.null(col)) return(NULL)
  txt <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  if (line < 1 || line > length(txt)) return(NULL)
  s <- txt[[line]]
  chars <- strsplit(s, "")[[1L]]
  is_id <- grepl(ident, chars)
  for (start in c(col, col + 1L)) {
    if (start < 1 || start > length(chars) || !is_id[[start]]) next
    a <- start; b <- start
    while (a > 1 && is_id[[a - 1L]]) a <- a - 1L
    while (b < length(chars) && is_id[[b + 1L]]) b <- b + 1L
    prev <- if (start > 1) chars[[start - 1L]] else ""
    return(list(name = paste(chars[a:b], collapse = ""), shifted = start != col,
                first_after_punct = (start == a) && prev %in% c("(", ",", "$", "[", "{", "-", "@", "!")))
  }
  NULL
}

sym <- if (op %in% c("findReferences", "incomingCalls", "outgoingCalls", "goToDefinition", "hover",
                     "prepareCallHierarchy", "goToImplementation")) {
  symbol_at(file, as.integer(ti$line), as.integer(ti$character))
} else NULL

null_answer <- grepl("^No (references|definition|hover|call hierarchy|incoming|outgoing|implementation)", resp_text) ||
  !nzchar(resp_text)
if (!is.null(sym) && !sym$shifted && sym$first_after_punct && null_answer) {
  notes <- c(notes, sprintf("The server has an off-by-one for the first character of a symbol that directly follows punctuation: repeat the call at character %d (inside `%s`) before concluding there is nothing.",
    as.integer(ti$character) + 1L, sym$name))
}

# --- grep complement -------------------------------------------------------------
root_norm <- normalizePath(root, mustWork = FALSE)
rel <- function(p) {
  q <- normalizePath(p, mustWork = FALSE)
  if (startsWith(q, paste0(root_norm, "/"))) substring(q, nchar(root_norm) + 2L) else q
}

# R files of the project; what git ignores is left out when the project is a repository.
project_files <- function() {
  pat <- "[.](R|r|Rmd|rmd|qmd)$"
  tracked <- tryCatch(suppressWarnings(system2("git", c("-C", shQuote(root), "ls-files", "--cached", "--others",
                                                           "--exclude-standard"), stdout = TRUE, stderr = FALSE)),
                      error = function(e) NULL)
  if (!is.null(tracked) && length(tracked) && is.null(attr(tracked, "status"))) {
    f <- file.path(root, grep(pat, tracked, value = TRUE))
  } else {
    f <- list.files(root, pattern = pat, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  }
  f[!grepl("/(renv|packrat|[.]git|node_modules|[.]Rproj[.]user)/", f) & file.exists(f)]
}

# The (file, line) pairs the server's answer names, in Claude Code's format
# "R/tax.R: Line 5:1, Line 9:20". Empty when the answer carries no lines.
answered_lines <- function(text) {
  out <- character()
  for (ln in strsplit(text, "\n", fixed = TRUE)[[1L]]) {
    m <- regmatches(ln, regexec("^\\s*(\\S+?):\\s*(Line\\s+\\d+.*)$", ln))[[1L]]
    if (length(m) < 3L) next
    nums <- regmatches(m[[3L]], gregexpr("Line\\s+(\\d+)", m[[3L]]))[[1L]]
    for (n in sub("Line\\s+", "", nums)) out <- c(out, paste0(m[[2L]], ":", n))
  }
  out
}
answered_files <- function(text) unique(sub(":.*$", "", answered_lines(text)))

if (!is.null(sym) && nzchar(sym$name) && op %in% c("findReferences", "incomingCalls", "goToDefinition")) {
  files <- project_files()
  if (length(files) <= 3000L) {
    name <- sym$name
    esc <- gsub("([.])", "\\\\\\1", name)
    pat <- paste0("(^|[^A-Za-z0-9._])", esc, "([^A-Za-z0-9._]|$)")
    def_pat <- paste0("^[[:space:]]*`?", esc, "`?[[:space:]]*(<-|=|<<-)|",
                      "(setMethod|setGeneric|assign)\\([[:space:]]*[\"']", name, "[\"']")
    top_def_pat <- paste0("^`?", esc, "`?[[:space:]]*(<-|=|<<-)")
    call_pat <- paste0("(^|[^A-Za-z0-9._])", esc, "[[:space:]]*\\(")
    known <- answered_lines(resp_text)
    known_files <- answered_files(resp_text)
    by_line <- length(known) > 0L
    hits <- character()
    call_hits <- character()
    top_level_def <- FALSE
    for (p in files) {
      txt <- tryCatch(readLines(p, warn = FALSE), error = function(e) character())
      idx <- grep(pat, txt, perl = TRUE)
      if (!length(idx)) next
      r <- rel(p)
      if (any(grepl(top_def_pat, txt[idx], perl = TRUE))) top_level_def <- TRUE
      if (op == "goToDefinition") {
        idx <- idx[grepl(def_pat, txt[idx], perl = TRUE)]
      } else if (by_line) {
        idx <- idx[!(paste0(r, ":", idx) %in% known)]
      } else if (r %in% known_files || basename(p) %in% basename(known_files) ||
                 grepl(r, resp_text, fixed = TRUE)) {
        next
      }
      for (i in idx) {
        line <- sprintf("%s:%d: %s", r, i, substr(trimws(txt[[i]]), 1, 110))
        hits <- c(hits, line)
        if (grepl(call_pat, txt[[i]], perl = TRUE) && !grepl("^[[:space:]]*#", txt[[i]])) call_hits <- c(call_hits, line)
      }
    }
    if (length(hits)) {
      if (op == "goToDefinition" && null_answer) {
        notes <- c(notes, paste0("Lines that look like a definition of `", name, "` (grep, whole project):\n  ",
          paste(head(hits, 10), collapse = "\n  ")))
      } else if (op == "incomingCalls") {
        if (length(call_hits)) {
          notes <- c(notes, paste0(
            if (null_answer) "The server's 'no incoming calls' is incomplete. " else "The server's caller list is incomplete. ",
            "grep finds ", length(call_hits), " call(s) of `", name, "` it did not return (scripts, tests and Rmd are outside",
            " the server's call hierarchy; so are calls through purrr/apply or do.call). Treat these as callers:\n  ",
            paste(head(call_hits, 15), collapse = "\n  "),
            if (length(call_hits) > 15) sprintf("\n  ... and %d more (rg -nw '%s\\(' )", length(call_hits) - 15, name) else ""))
        }
      } else if (!top_level_def) {
        # no `name <- ...` at column 1 anywhere: a local variable, argument or data column
        notes <- c(notes, paste0("`", name, "` has no top-level definition in the project, so it is probably a local variable, ",
          "argument or data column; the server's scoped answer is the one to trust. For the record, grep finds ",
          length(hits), " other mention(s) of that name in ", length(unique(sub(":.*$", "", hits))),
          " file(s), most likely unrelated: ", paste(head(unique(sub(":.*$", "", hits)), 5), collapse = ", "),
          if (length(unique(sub(":.*$", "", hits))) > 5) ", ..." else ""))
      } else {
        nfiles <- length(unique(sub(":.*$", "", hits)))
        notes <- c(notes, paste0("grep finds ", length(hits), " mention(s) of `", name, "` in ", nfiles,
          " file(s) that the server's answer does not list. The server resolves references by scope (a package's R/,",
          " open files and source()d files) and skips roxygen `[", name, "()]` links, comments, strings and tests/scripts",
          " it does not treat as related. Check these before a rename or signature change:\n  ",
          paste(head(hits, 15), collapse = "\n  "),
          if (length(hits) > 15) sprintf("\n  ... and %d more (rg -nw %s)", length(hits) - 15, name) else ""))
      }
    }
  }
}

if (length(notes)) {
  text <- paste0("r-lsp plugin note: ", paste(notes, collapse = "\n"))
  if (nchar(text) > 6000) text <- paste0(substr(text, 1, 6000), "\n... (truncated)")
  cat(jsonlite::toJSON(list(hookSpecificOutput = list(hookEventName = "PostToolUse", additionalContext = text)),
                       auto_unbox = TRUE), "\n")
}
quit(save = "no", status = 0)
