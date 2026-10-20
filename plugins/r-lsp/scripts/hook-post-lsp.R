# PostToolUse hook body for the LSP tool: say what the language server's
# answer does not cover.
#
#  - If the server is still indexing this project, say so (the answer may
#    be incomplete; it is complete once the index is ready).
#  - For findReferences / incomingCalls / outgoingCalls: the server searches
#    a package's R/ files, open files and source()d files. List the mentions
#    grep finds in files it does not index (tests/, scripts, Rmd, ...), so a
#    refactor does not miss them.
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
project_files <- function() {
  f <- list.files(root, pattern = "[.](R|r|Rmd|rmd|qmd)$", recursive = TRUE, full.names = TRUE, all.files = FALSE)
  f[!grepl("/(renv|packrat|[.]git|node_modules|[.]Rproj[.]user)/", f)]
}

if (!is.null(sym) && nzchar(sym$name) && op %in% c("findReferences", "incomingCalls", "goToDefinition")) {
  files <- project_files()
  if (length(files) <= 3000L) {
    name <- sym$name
    pat <- paste0("(^|[^A-Za-z0-9._])", gsub("([.])", "\\\\\\1", name), "([^A-Za-z0-9._]|$)")
    def_pat <- paste0("^[[:space:]]*`?", gsub("([.])", "\\\\\\1", name), "`?[[:space:]]*(<-|=|<<-)|",
                      "(setMethod|setGeneric|assign)\\([[:space:]]*[\"']", name, "[\"']")
    root_norm <- normalizePath(root, mustWork = FALSE)
    rel <- function(p) {
      q <- normalizePath(p, mustWork = FALSE)
      if (startsWith(q, paste0(root_norm, "/"))) substring(q, nchar(root_norm) + 2L) else q
    }
    covered <- function(p) {
      r <- rel(p)
      grepl(r, resp_text, fixed = TRUE) || grepl(basename(p), resp_text, fixed = TRUE)
    }
    hits <- list()
    for (p in files) {
      txt <- tryCatch(readLines(p, warn = FALSE), error = function(e) character())
      idx <- grep(pat, txt, perl = TRUE)
      if (!length(idx)) next
      if (op == "goToDefinition") {
        idx <- idx[grepl(def_pat, txt[idx], perl = TRUE)]
        if (!length(idx)) next
      } else if (covered(p)) next
      for (i in idx) hits[[length(hits) + 1L]] <- sprintf("%s:%d: %s", rel(p), i, substr(trimws(txt[[i]]), 1, 110))
    }
    if (length(hits)) {
      if (op == "goToDefinition" && null_answer) {
        notes <- c(notes, paste0("Lines that look like a definition of `", name, "` (grep, whole project):\n  ",
          paste(head(hits, 10), collapse = "\n  ")))
      } else if (op != "goToDefinition") {
        nfiles <- length(unique(sub(":.*$", "", hits)))
        notes <- c(notes, paste0("grep finds ", length(hits), " more mention(s) of `", name, "` in ", nfiles,
          " file(s) the language server does not index (it covers a package's R/, open files and source()d files; not tests/, scripts or Rmd). Check these before a rename or signature change:\n  ",
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
