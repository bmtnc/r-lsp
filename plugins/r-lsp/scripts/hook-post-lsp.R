# PostToolUse hook body for the LSP tool: say what the language server's
# answer does not cover.
#
#  - If the server is still indexing this project, say so (the answer may
#    be incomplete; it is complete once the index is ready).
#  - For findReferences / incomingCalls: grep the project for the symbol and
#    list every mention the server's answer did not return, line by line, in
#    whatever file (tests, scripts, roxygen links, comments, other
#    assignments of the same variable), returned files included. Files git
#    ignores are skipped. A name with no top-level definition is classified
#    with the parser: assigned at script level inside a block (the server
#    scopes it to the block, so its answer is partial), created as an
#    argument name (a data-frame column), or local to a function.
#  - For goToDefinition: list other lines that assign the symbol; with no
#    result, lines that look like a definition anywhere in the project.
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
# A file from another repository (opened from this session): grep that
# repository, not this one. Its root is found the way the server finds it.
project_root_of <- function(path) {
  dir <- dirname(normalizePath(path, mustWork = FALSE))
  home <- normalizePath("~", mustWork = FALSE)
  markers <- c("DESCRIPTION", "renv.lock", "_targets.R", ".here", ".git")
  while (nzchar(dir) && dir != dirname(dir) && dir != home) {
    if (any(file.exists(file.path(dir, markers))) || length(list.files(dir, pattern = "[.]Rproj$"))) return(dir)
    dir <- dirname(dir)
  }
  NULL
}
session_root <- root
if (nzchar(file) && file.exists(file) &&
    !startsWith(normalizePath(file, mustWork = FALSE), paste0(normalizePath(root, mustWork = FALSE), "/"))) {
  other <- project_root_of(file)
  if (!is.null(other)) root <- other
}
resp <- input$tool_response
resp_text <- if (is.character(resp)) paste(resp, collapse = "\n") else
  if (is.list(resp) && is.character(resp$formatted)) resp$formatted else
  tryCatch(as.character(jsonlite::toJSON(resp, auto_unbox = TRUE)), error = function(e) "")
notes <- character()

# --- index state ---------------------------------------------------------------
status_file <- Sys.getenv("R_LSP_STATUS_FILE",
  file.path(Sys.getenv("CLAUDE_PLUGIN_DATA", ""), "status.log"))
still_indexing <- NULL
if (file.exists(status_file)) {
  lines <- tryCatch(readLines(status_file, warn = FALSE), error = function(e) character())
  mine <- grep(paste0("[", session_root, "]"), lines, fixed = TRUE, value = TRUE)
  # only the most recently started server's events; a server that has
  # exited (plugin reload) left "indexing" lines that mean nothing now
  marker <- grep("R LSP: server started", mine, fixed = TRUE)
  if (length(marker)) {
    mine <- mine[tail(marker, 1):length(mine)]
    pid <- sub("^.*\\(pid ([0-9]+).*$", "\\1", mine[[1L]])
    if (grepl("^[0-9]+$", pid) && !isTRUE(tools::pskill(as.integer(pid), 0L))) mine <- character()
  }
  if (length(mine)) {
    last <- tail(mine, 1)
    if (grepl(": indexing ", last, fixed = TRUE) && !grepl("index ready", last, fixed = TRUE)) {
      still_indexing <- sub("^.*R LSP: ", "", last)
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

null_answer <- grepl("^No (references|definition|hover|call hierarchy|incoming|outgoing|implementation|symbols)", resp_text) ||
  !nzchar(resp_text)
# An answer given while the index was still building: an empty one is not
# an answer at all, a non-empty one may be short.
if (!is.null(still_indexing)) {
  notes <- c(notes, if (null_answer) paste0(
    "This empty result is not an answer: the R language server was still indexing this project (", still_indexing,
    ") when it replied. Repeat the same call once `r-lsp-status` shows 'index ready' (the index wait is capped at ",
    "R_LSP_INDEX_WAIT seconds, 30 by default; a larger project can need more).") else paste0(
    "The R language server is still indexing this project (", still_indexing,
    "). This answer may be incomplete; the same call returns complete results once `r-lsp-status` shows 'index ready'."))
}
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

# The (file, line) pairs the server's answer names. Claude Code prints the
# LSP tool's result in three shapes:
#   findReferences   "R/a.R:" on a line of its own, then "  Line 5:1" lines
#   call hierarchy   "R/a.R:" then "  f (Function) - Line 59 [calls at: 97:15]"
#   goToDefinition   "Defined in R/a.R:27:1"
# and the older one-line "R/a.R: Line 5:1, Line 9:20". For a call hierarchy
# item the call positions count as answered, not only the item's line.
line_numbers <- function(s) {
  nums <- sub("Line\\s+", "", regmatches(s, gregexpr("Line\\s+\\d+", s))[[1L]])
  calls <- regmatches(s, regexec("\\[call(s|ed) (at|from): ([^]]*)\\]", s))[[1L]]
  if (length(calls) >= 4L) nums <- c(nums, sub(":.*$", "", trimws(strsplit(calls[[4L]], ",")[[1L]])))
  nums
}
answered_lines <- function(text) {
  out <- character()
  cur <- NULL
  add <- function(f, nums) {
    if (startsWith(f, "/")) f <- rel(f)
    for (n in nums) out <<- c(out, paste0(f, ":", n))
  }
  for (ln in strsplit(text, "\n", fixed = TRUE)[[1L]]) {
    m <- regmatches(ln, regexec("^\\s*Defined in (\\S+?):(\\d+)(:\\d+)?\\s*$", ln, perl = TRUE))[[1L]]
    if (length(m) >= 3L) { add(m[[2L]], m[[3L]]); next }
    m <- regmatches(ln, regexec("^(\\S+?):\\s*(Line\\s+\\d+.*)$", ln, perl = TRUE))[[1L]]
    if (length(m) >= 3L) { cur <- m[[2L]]; add(cur, line_numbers(m[[3L]])); next }
    m <- regmatches(ln, regexec("^(\\S+?):\\s*$", ln, perl = TRUE))[[1L]]
    if (length(m) >= 2L) { cur <- m[[2L]]; next }
    if (!is.null(cur) && grepl("^\\s+.*Line\\s+\\d+", ln)) add(cur, line_numbers(ln))
  }
  unique(out)
}
answered_files <- function(text) unique(sub(":[0-9]+$", "", answered_lines(text)))

# How the queried file assigns `name`: "script" when some assignment sits
# outside every function body (inside an if/else or loop at script level
# counts), "function" when all of them are inside one, "none" otherwise.
# The server scopes a block assignment to its block, so a script-level
# variable assigned in two branches gets two disjoint reference sets.
assignment_kind <- function(path, name) {
  pd <- tryCatch(utils::getParseData(parse(path, keep.source = TRUE)), error = function(e) NULL)
  if (is.null(pd) || !nrow(pd)) return("none")
  pd <- pd[order(pd$line1, pd$col1, -pd$line2, -pd$col2), ]
  parent_of <- setNames(pd$parent, pd$id)
  token_of <- setNames(pd$token, pd$id)
  kids <- split(pd$id, pd$parent)
  kinds <- character()
  for (sid in pd$id[pd$token == "SYMBOL" & pd$text == name]) {
    p <- parent_of[[as.character(sid)]]
    if (is.na(p) || p <= 0) next
    g <- parent_of[[as.character(p)]]
    if (is.na(g) || g <= 0) next
    ch <- kids[[as.character(g)]]
    if (length(ch) < 2L || ch[[1L]] != p ||
        !(token_of[[as.character(ch[[2L]])]] %in% c("LEFT_ASSIGN", "EQ_ASSIGN"))) next
    inside <- FALSE
    a <- parent_of[[as.character(g)]]
    while (!is.na(a) && a > 0) {
      if (any(token_of[as.character(kids[[as.character(a)]])] %in% c("FUNCTION", "OP-LAMBDA"))) { inside <- TRUE; break }
      a <- parent_of[[as.character(a)]]
    }
    kinds <- c(kinds, if (inside) "function" else "script")
  }
  if ("script" %in% kinds) "script" else if (length(kinds)) "function" else "none"
}

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
    # `name = value` as an argument (indented, or after "(" or ","): how a
    # column is created inside mutate()/summarise()/tibble()
    col_pat <- paste0("(^[[:space:]]+|[(,][[:space:]]*)`?", esc, "`?[[:space:]]*=[^=]")
    # ... and only inside a data-masking call: a default in function(x, name = 1)
    # or an ordinary named argument is not a column
    verb_ctx <- "(^|[^A-Za-z0-9._])(mutate|summarise|summarize|transmute|reframe|tibble|data[.]frame|tribble|group_by|count|rename|select)[[:space:]]*\\("
    doc_pat <- paste0("^[[:space:]]*#'.*\\[", esc, "\\(\\)\\]")
    # Is line i inside a data-masking call? Walk up to the line that opens
    # the enclosing call: a verb means yes; a function signature, a blank
    # line or a block opener means the argument belongs to something else.
    in_masking_call <- function(txt, i) {
      for (j in i:max(1L, i - 40L)) {
        if (grepl("function[[:space:]]*\\(", txt[[j]])) return(FALSE)
        if (grepl(verb_ctx, txt[[j]], perl = TRUE)) return(TRUE)
        if (j < i && (!nzchar(trimws(txt[[j]])) || grepl("[{]\\s*$", txt[[j]]))) return(FALSE)
      }
      FALSE
    }
    known <- answered_lines(resp_text)
    known_files <- answered_files(resp_text)
    by_line <- length(known) > 0L
    hits <- character()
    call_hits <- character()
    doc_hits <- character()
    col_hits <- character()
    top_level_def <- FALSE
    queried <- rel(file)
    same_file <- character()
    for (p in files) {
      txt <- tryCatch(readLines(p, warn = FALSE), error = function(e) character())
      idx <- grep(pat, txt, perl = TRUE)
      if (!length(idx)) next
      r <- rel(p)
      if (any(grepl(top_def_pat, txt[idx], perl = TRUE))) top_level_def <- TRUE
      if (op == "goToDefinition") {
        idx <- idx[grepl(def_pat, txt[idx], perl = TRUE) & !(paste0(r, ":", idx) %in% known)]
      } else if (by_line) {
        idx <- idx[!(paste0(r, ":", idx) %in% known)]
      } else if (r %in% known_files || basename(p) %in% basename(known_files) ||
                 grepl(r, resp_text, fixed = TRUE)) {
        next
      }
      for (i in idx) {
        line <- sprintf("%s:%d: %s", r, i, substr(trimws(txt[[i]]), 1, 110))
        hits <- c(hits, line)
        if (r == queried) same_file <- c(same_file, line)
        is_comment <- grepl("^[[:space:]]*#", txt[[i]])
        if (grepl(call_pat, txt[[i]], perl = TRUE) && !is_comment) call_hits <- c(call_hits, line)
        if (grepl(doc_pat, txt[[i]], perl = TRUE)) doc_hits <- c(doc_hits, line)
        if (grepl(col_pat, txt[[i]], perl = TRUE) && !is_comment && in_masking_call(txt, i)) col_hits <- c(col_hits, line)
      }
    }
    files_of <- function(h) unique(sub(":.*$", "", h))
    listed <- function(h, n = 15, hint = "rg -nw %s") paste0(
      paste(head(h, n), collapse = "\n  "),
      if (length(h) > n) sprintf(paste0("\n  ... and %d more (", hint, ")"), length(h) - n, name) else "")
    if (length(hits)) {
      if (op == "goToDefinition") {
        notes <- c(notes, if (null_answer) {
          paste0("Lines that look like a definition of `", name, "` (grep, whole project):\n  ", listed(hits, 10))
        } else {
          paste0("Other lines that assign `", name, "`, which the server's definition does not cover (a variable",
            " assigned in several branches or files, or a redefinition):\n  ", listed(hits, 10))
        })
      } else if (op == "incomingCalls") {
        if (length(call_hits)) {
          notes <- c(notes, paste0(
            if (null_answer) "The server's 'no incoming calls' is incomplete. " else "The server's caller list is incomplete. ",
            "grep finds ", length(call_hits), " call(s) of `", name, "` it did not return (Rmd files and calls through",
            " purrr/apply or do.call are outside the server's call hierarchy). Treat these as callers:\n  ",
            listed(call_hits, hint = "rg -nw '%s\\(' ")))
        }
        if (length(doc_hits)) {
          notes <- c(notes, paste0("Roxygen links to `", name, "` (not calls, but a rename must update them):\n  ",
            listed(doc_hits, 10)))
        }
      } else if (!top_level_def && assignment_kind(file, name) == "script") {
        notes <- c(notes, paste0("`", name, "` is assigned at script level inside a block (an if/else or loop body) in ",
          queried, ". The server scopes such assignments to their block, so its answer can be one branch's",
          " assignment and the uses after it, not the whole variable. Treat the server's list plus these",
          " as the reference set before a rename:\n  ", listed(same_file, 20),
          if (length(setdiff(files_of(hits), queried))) paste0("\n  Other files mentioning the name: ",
            paste(head(setdiff(files_of(hits), queried), 5), collapse = ", ")) else ""))
      } else if (!top_level_def && length(col_hits)) {
        notes <- c(notes, paste0("`", name, "` has no script-level definition in the project, but it is created as",
          " an argument name (`", name, " = ...`) at ", sub(": .*$", "", col_hits[[1L]]), ", so it may be a",
          " data-frame column rather than a variable. The server does not track columns, and 'No references",
          " found' means nothing for one. Check these before a rename:\n  ", listed(hits)))
      } else if (!top_level_def) {
        # no `name <- ...` at column 1 anywhere: a local variable, argument or data column
        notes <- c(notes, paste0("`", name, "` has no top-level definition in the project, so it is probably a local variable, ",
          "argument or data column; the server's scoped answer is the one to trust. For the record, grep finds ",
          length(hits), " other mention(s) of that name in ", length(files_of(hits)),
          " file(s), most likely unrelated: ", paste(head(files_of(hits), 5), collapse = ", "),
          if (length(files_of(hits)) > 5) ", ..." else ""))
      } else {
        notes <- c(notes, paste0("grep finds ", length(hits), " mention(s) of `", name, "` in ", length(files_of(hits)),
          " file(s) that the server's answer does not list. The server resolves references by scope and",
          " skips roxygen `[", name, "()]` links, comments, strings and Rmd files.",
          " Check these before a rename or signature change:\n  ", listed(hits)))
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
