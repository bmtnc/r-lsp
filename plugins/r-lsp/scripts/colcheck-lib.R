# colcheck: bare column names in tidyverse code vs the real columns of the
# data the code reads.
#
# For each data frame whose source can be resolved (a path given to
# read_csv()/read.csv()/fread()/readRDS()/read_parquet()/read_excel()/
# read_dta()/read_sav(), a built-in dataset, or a tibble()/data.frame()/
# tribble() literal), the column set is tracked through dplyr, tidyr and
# ggplot2 verbs. A bare name used as a column that cannot exist is reported
# with its line.
#
# Only column NAMES are read: one header line for delimited files, file
# metadata for parquet/feather/excel/stata/spss/fst. RDS files are loaded only
# when smaller than 20 MB. The moment a step makes the column set unknowable
# (across(), tidyselect helpers, pivot_wider(), clean_names(), joins to
# unknown frames, user functions, ...) the frame is marked unknown and nothing
# downstream is checked. Rmd/qmd files are skipped.
#
# Data the checker cannot open (S3 objects, databases, project loaders) is
# covered by the schema cache, <root>/.colcheck/schemas.json, written by
# `r-colcheck learn KEY 'EXPR'`. A source call is matched to a cache entry by
# any string literal in the call (the S3 key, URI or dataset name the code
# passes to its loader), or by a `# cols: KEY` comment on the assignment line.
# A frame whose source stays unresolved and is then piped into verbs is
# reported as a note, so the cache can be filled; nothing is reported for
# unresolved frames that are never used that way.
#
# Entry points: colcheck_files(files, root) -> list of findings (kind "column"
#               or "note"); colcheck_format(findings, root) -> character lines.

`%||%` <- function(a, b) if (is.null(a)) b else a
# Arguments of a call without empty ones (`x[, 1]`): the empty symbol cannot
# be bound to a loop variable or passed to a function without an error.
call_args <- function(e) {
  args <- as.list(e)[-1L]
  args[!vapply(seq_along(args), function(i) identical(args[[i]], quote(expr = )), TRUE)]
}
quiet <- function(expr, default = NULL) {
  tryCatch(suppressWarnings(suppressMessages(expr)), error = function(e) default)
}

# Run one unit of checking (a top-level statement, a followed function) and,
# if the checker itself fails, record that against the statement instead of
# dropping it silently: a lost statement must not look like a clean one.
# R_LSP_COLCHECK_DEBUG=1 prints the call stack of each failure to stderr.
guarded <- function(ctx, where, expr) {
  debug <- nzchar(Sys.getenv("R_LSP_COLCHECK_DEBUG"))
  at <- list(file = ctx$file, line = ctx$line)
  tryCatch(
    withCallingHandlers(suppressWarnings(suppressMessages(expr)), error = function(err) {
      if (debug) {
        cat(file = stderr(), sprintf("r-colcheck: %s:%s: %s%s\n", at$file, at$line, conditionMessage(err),
                                     if (is.null(where)) "" else sprintf(" (following %s)", where)))
        calls <- sys.calls()
        cat(file = stderr(), paste0("  ", vapply(calls, function(cl) paste(deparse(cl)[[1L]], collapse = ""), "")[seq_len(min(30L, length(calls)))], collapse = "\n"), "\n")
      }
    }),
    error = function(err) {
      ctx$errors[[length(ctx$errors) + 1L]] <- list(file = at$file, line = at$line, where = where,
                                                    message = conditionMessage(err))
      NULL
    })
}

error_findings <- function(ctx) {
  lapply(ctx$errors, function(er) list(
    kind = "error", file = er$file, line = er$line, name = NA_character_, source = NA_character_,
    columns = character(),
    message = sprintf("r-colcheck hit an error %sand gave up on this statement (%s); nothing downstream of it is checked. This is a checker bug worth reporting; R_LSP_COLCHECK_DEBUG=1 shows where it happened.",
                      if (is.null(er$where)) "" else sprintf("while following %s() ", er$where), er$message)))
}

colcheck_root <- function(path) {
  dir <- normalizePath(dirname(path), mustWork = FALSE)
  markers <- c("DESCRIPTION", ".git", "renv.lock", "_targets.R", ".here")
  d <- dir
  while (nzchar(d) && d != dirname(d)) {
    if (any(file.exists(file.path(d, markers))) || length(list.files(d, pattern = "[.]Rproj$"))) return(d)
    d <- dirname(d)
  }
  dir
}

# --- schema cache: .colcheck/schemas.json -----------------------------------
# Column names observed from data the checker cannot open itself. Keyed by the
# string the code passes to its loader. Written by colcheck-learn.R.

schemas_path <- function(root) file.path(root, ".colcheck", "schemas.json")

load_schemas <- function(root) {
  p <- schemas_path(root)
  if (!file.exists(p)) return(list())
  s <- quiet(jsonlite::fromJSON(p, simplifyVector = FALSE))
  if (!is.list(s) || is.null(names(s))) return(list())
  s
}

schema_columns <- function(key, ctx) {
  entry <- ctx$schemas[[key]]
  if (is.null(entry) || !is.list(entry$columns)) return(NULL)
  cols <- vapply(entry$columns, function(cl) {
    if (is.list(cl)) as.character(cl$name %||% "") else as.character(cl)
  }, "")
  cols <- cols[nzchar(cols)]
  if (!length(cols)) return(NULL)
  observed <- substr(as.character(entry$observed_at %||% ""), 1L, 10L)
  attr(cols, "source") <- sprintf("cached schema \"%s\"%s", key,
                                  if (nzchar(observed)) paste0(", observed ", observed) else "")
  age <- quiet(as.integer(Sys.Date() - as.Date(observed)))
  if (!is.null(age) && !is.na(age) && age > 90L && is.null(ctx$stale[[key]])) {
    ctx$stale[[key]] <- list(line = ctx$line, days = age, observed = observed)
  }
  cols
}

string_literals <- function(e) {
  if (is.character(e)) return(e)
  if (!is.call(e)) return(character())
  unlist(lapply(call_args(e), string_literals))
}

# Column set for a source call the checker could not open: a cache entry for
# any string literal in the call, else the `# cols: KEY` annotation of the
# assignment being walked (consumed by the first unresolved source, which is
# the head of the pipeline).
cached_columns <- function(e, ctx) {
  if (length(ctx$schemas)) {
    for (s in unique(string_literals(e))) {
      cols <- schema_columns(s, ctx)
      if (!is.null(cols)) return(cols)
    }
  }
  # `# cols: KEY` names the columns of the expression the line assigns (its
  # pipeline head), never of a sub-expression met on the way, such as the
  # loader's first argument
  key <- ctx$annotation
  if (!is.null(key) && (is.null(ctx$annotation_head) || identical(e, ctx$annotation_head))) {
    ctx$annotation <- NULL
    return(schema_columns(key, ctx))
  }
  # an entry keyed on the function itself (`r-colcheck learn "pkg::fn" '<expr>'`):
  # a helper whose output is the same every call, with no literal to match
  # and no comment on any call line
  if (length(ctx$schemas)) {
    for (k in function_keys(e, ctx)) {
      cols <- schema_columns(k, ctx)
      if (!is.null(cols)) return(cols)
    }
  }
  NULL
}

# The cache keys a call matches by its function: `pkg::fn` and `fn` for a
# namespaced call; for a bare call, `fn` and any `<pkg>::fn` entry.
function_keys <- function(e, ctx) {
  fn <- call_name(e)
  if (is.na(fn)) return(character())
  head <- e[[1L]]
  if (is.call(head)) return(c(paste0(as.character(head[[2L]]), "::", fn), fn))
  c(fn, grep(paste0("^[A-Za-z][A-Za-z0-9.]*::", gsub(".", "[.]", fn, fixed = TRUE), "$"), names(ctx$schemas), value = TRUE))
}

line_annotation <- function(ctx) {
  if (is.na(ctx$line) || ctx$line > length(ctx$src_lines)) return(NULL)
  m <- regmatches(ctx$src_lines[[ctx$line]],
                  regexec("#\\s*(?:cols?|colcheck|schema)\\s*:\\s*(\"[^\"]+\"|'[^']+'|\\S+)", ctx$src_lines[[ctx$line]], perl = TRUE))[[1L]]
  if (length(m) < 2L) return(NULL)
  gsub("^[\"']|[\"']$", "", m[[2L]])
}

# The expression a pipeline starts from: strip pipes and verbs from the left.
pipeline_head <- function(e) {
  while (is.call(e)) {
    fn <- call_name(e)
    if (is.na(fn) || !(fn %in% c("%>%", "%<>%", "%T>%", verb_names)) || length(e) < 2L) break
    e <- e[[2L]]
  }
  e
}

# Remember that `target` came from a source the checker could not resolve,
# when the source looks like a data loader (a reader, or a call carrying a
# string literal such as an S3 key) or the line names a key that is not cached.
note_unresolved <- function(target, rhs, key, ctx) {
  # only script-level loaders: inside a function the data usually comes from
  # a parameter, and the learn command needs a self-contained expression
  if (isTRUE(ctx$in_function)) return(invisible())
  # tests build their data in place; nothing there is learnable
  if (grepl("(^|/)tests?/", relative_path(ctx$file, ctx$root))) return(invisible())
  head <- pipeline_head(rhs)
  if (!is.call(head)) return(invisible())
  fn <- call_name(head)
  if (is.na(fn) || fn %in% not_loaders) return(invisible())
  # A call `learn` cannot evaluate on its own (variables, function literals,
  # nested project calls, no key) used to leave no note. When it is a package
  # function the checker followed and could not read, name it instead, so a
  # skipped pipeline has a cause. A self-contained loader call with a literal
  # key keeps the learn suggestion below, package or not.
  head_def <- lookup_function(head, fn, ctx)
  package_note <- function() {
    if (!is.null(head_def) && !is.null(head_def$pkg)) {
      ctx$unresolved[[target]] <- list(line = ctx$line, keys = character(), package_fn = head_def$file,
                                       expr = paste(deparse(head), collapse = " "), used = !identical(head, rhs),
                                       self_contained = !length(all.vars(head)) && !("function" %in% all.names(head)))
    }
    invisible()
  }
  # `learn` evaluates the expression on its own, so only a self-contained
  # loader call qualifies: no variables, no function literals, no project
  # function calls nested in the arguments. update_log(log, "step"),
  # list_rbind(imap(chunks, function(...) ...)) and friends are computations.
  if (length(all.vars(head)) || "function" %in% all.names(head)) return(package_note())
  nested <- setdiff(all.names(head), c(fn, "::", ":::"))
  if (any(nested %in% names(ctx$functions))) return(package_note())
  # the key is a direct argument of the loader, or of a path built in place
  direct <- call_args(head)
  literals <- unique(unlist(lapply(direct, function(a) {
    if (is.character(a)) a
    else if (is.call(a) && call_name(a) %in% c("file.path", "here", "paste0", "paste", "sprintf", "glue")) string_literals(a)
    else NULL
  })))
  if (fn %in% readers) {
    # a reader with a literal path that does not exist locally (an S3 URI);
    # read.csv(text = x) or read_csv(path_variable) are not learnable sources
    path_arg <- if (length(call_args(head))) call_args(head)[[1L]] else NULL
    if (!is.character(path_arg) && !(is.call(path_arg) && call_name(path_arg) %in% c("here", "file.path"))) return(invisible())
  } else if (is.null(key) && !length(literals)) return(package_note())
  ctx$unresolved[[target]] <- list(
    line = ctx$line, keys = unique(c(key, literals)), expr = paste(deparse(head), collapse = " "),
    used = !identical(head, rhs))
  invisible()
}

# Calls that combine or wrap frames rather than load them.
not_loaders <- c("bind_rows", "bind_cols", "rbind", "cbind", "merge", "rbindlist", "tryCatch", "try",
                 "withCallingHandlers", "suppressWarnings", "suppressMessages", "switch", "if", "ifelse",
                 "c", "list", "paste", "paste0", "sprintf", "format", "as.Date", "as.numeric",
                 "as.character", "as.integer", "unique", "sort", "rev", "seq", "rep", "Reduce", "do.call",
                 "lapply", "sapply", "vapply", "map", "map_dfr", "map_df", "purrr::map")

mark_used <- function(nm, ctx) {
  if (!is.null(ctx$unresolved[[nm]])) ctx$unresolved[[nm]]$used <- TRUE
  invisible()
}

# --- project functions ------------------------------------------------------
# Logic is often factored out: `cleaned <- clean(sales)`. When a known frame
# is passed to a function defined in the project, the parameter is bound to
# that column set, the body is checked (with its own lines), and the columns
# of the body's last expression become the columns of the call. Functions
# come from the file itself, files it source()s, and, in an R package, every
# file under R/.

function_defs <- function(exprs, file) {
  out <- list()
  for (e in exprs) {
    if (is.call(e) && call_name(e) %in% c("<-", "=") && length(e) == 3L && is.symbol(e[[2L]]) &&
        is.call(e[[3L]]) && identical(call_name(e[[3L]]), "function")) {
      out[[as.character(e[[2L]])]] <- list(fn = e[[3L]], file = file)
    }
  }
  out
}

source_calls <- function(e, acc = character()) {
  if (!is.call(e)) return(acc)
  if (call_name(e) %in% c("source", "sys.source") && length(e) >= 2L && is.character(e[[2L]])) acc <- c(acc, e[[2L]])
  for (p in call_args(e)) if (is.call(p)) acc <- source_calls(p, acc)
  acc
}

project_functions <- function(file, exprs, ctx) {
  self <- normalizePath(file, mustWork = FALSE)
  others <- character()
  if (file.exists(file.path(ctx$root, "DESCRIPTION"))) {
    others <- list.files(file.path(ctx$root, "R"), pattern = "[.][rR]$", full.names = TRUE)
  }
  for (e in exprs) for (s in source_calls(e)) {
    p <- resolve_path(s, ctx)
    if (!is.null(p)) others <- c(others, p)
  }
  defs <- list()
  for (f in setdiff(unique(normalizePath(others, mustWork = FALSE)), self)) {
    ex <- quiet(parse(f, keep.source = TRUE))
    if (!is.null(ex)) defs[names(function_defs(ex, f))] <- function_defs(ex, f)
  }
  own <- function_defs(exprs, file)
  defs[names(own)] <- own
  defs
}

# Statements of a `{` block, each at its own line.
walk_block <- function(block, ctx, last = walk_expr) {
  stmts <- as.list(block)[-1L]
  srcs <- attr(block, "srcref")
  saved_line <- ctx$line
  saved_end <- ctx$line_end
  result <- NULL
  for (i in seq_along(stmts)) {
    if (!is.null(srcs) && length(srcs) > i) {
      ctx$line <- as.integer(srcs[[i + 1L]][[1L]])
      ctx$line_end <- as.integer(srcs[[i + 1L]][[3L]])
    }
    result <- if (i == length(stmts)) last(stmts[[i]], ctx) else walk_expr(stmts[[i]], ctx)
  }
  ctx$line <- saved_line
  ctx$line_end <- saved_end
  result
}

# Walk a function body in the current scope; the columns of what it returns.
body_columns <- function(body, ctx) {
  if (is.call(body) && identical(call_name(body), "{")) return(walk_block(body, ctx, last = return_columns))
  return_columns(body, ctx)
}

return_columns <- function(e, ctx) {
  if (is.call(e) && call_name(e) %in% c("return", "invisible") && length(e) >= 2L) e <- e[[2L]]
  walk_expr(e, ctx)
  if (is.call(e) && call_name(e) %in% c("<-", "=") && is.symbol(e[[2L]])) return(ctx$frames[[as.character(e[[2L]])]])
  if (is.call(e) && identical(call_name(e), "{")) return(walk_block(e, ctx, last = return_columns))
  frame_columns(e, ctx)
}

# --- installed package functions ---------------------------------------------
# Raw data usually passes through a team's own package before any dplyr code:
# `orders <- teamdata::clean_orders(raw, customers)`. The function
# is installed, so its body is available without running it, and the walker
# reads it like a project function: parameters bound to the caller's column
# sets, the columns of the last expression returned. Only the result crosses
# back out: nothing inside a package body is reported or counted, since the
# user cannot edit it. Resolution: `pkg::fn` always; a bare name through the
# packages the file attached with library(); inside a package body, that
# package's own functions. Reading a function loads the package namespace
# (its .onLoad runs, as for hover and go-to-definition); set
# R_LSP_COLCHECK_PACKAGES=false to turn this off.
package_skip <- c("base", "stats", "utils", "methods", "graphics", "grDevices", "tools", "parallel",
                  "compiler", "datasets", "dplyr", "tidyr", "purrr", "tibble", "ggplot2", "stringr", "forcats",
                  "readr", "lubridate", "rlang", "vctrs", "magrittr", "tidyselect", "data.table", "arrow",
                  "glue", "cli", "withr", "testthat", "janitor", "dbplyr", "DBI", "jsonlite", "httr", "httr2")

# The project's renv library first, so in-house packages are found and
# judged at the versions the project pins. Once per process.
use_project_library <- function(root) {
  if (isTRUE(getOption("r_lsp.colcheck_library_set"))) return(invisible())
  options(r_lsp.colcheck_library_set = TRUE)
  if (!file.exists(file.path(root, "renv", "activate.R")) || !requireNamespace("renv", quietly = TRUE)) return(invisible())
  lib <- quiet(renv::paths$library(project = root))
  if (is.character(lib) && length(lib) == 1L && dir.exists(lib)) .libPaths(c(lib, .libPaths()))
  invisible()
}

packages_enabled <- function() !identical(Sys.getenv("R_LSP_COLCHECK_PACKAGES", "true"), "false")

# A definition for pkg::fn read from the installed package, in the shape
# project_functions() produces, or NULL (not installed, not a closure).
package_function <- function(pkg, fn, ctx, exported_only = TRUE) {
  if (!packages_enabled() || pkg %in% package_skip || is.na(fn) || !nzchar(fn)) return(NULL)
  if (is.null(ctx$package_defs)) ctx$package_defs <- new.env()
  key <- paste0(pkg, "::", fn, if (exported_only) "" else ":::")
  hit <- ctx$package_defs[[key]]
  if (!is.null(hit)) return(if (identical(hit, FALSE)) NULL else hit)
  def <- NULL
  if (quiet(requireNamespace(pkg, quietly = TRUE), FALSE)) {
    ok <- !exported_only || fn %in% quiet(getNamespaceExports(pkg), character())
    f <- if (ok) quiet(get0(fn, envir = asNamespace(pkg), inherits = FALSE)) else NULL
    if (is.function(f) && !is.primitive(f)) {
      def <- list(fn = as.call(list(as.name("function"), formals(f), body(f))),
                  file = paste0(pkg, "::", fn), pkg = pkg)
    }
  }
  ctx$package_defs[[key]] <- def %||% FALSE
  def
}

# The definition a call reaches: a project function, or an installed package
# function (see above). `e` is the call, `fn` its bare name.
lookup_function <- function(e, fn, ctx) {
  if (is.na(fn)) return(NULL)
  head <- if (is.call(e)) e[[1L]] else NULL
  if (!is.na(ns_name(head)) && is.symbol(head[[2L]])) {
    return(package_function(as.character(head[[2L]]), fn, ctx, exported_only = identical(as.character(head[[1L]]), "::")))
  }
  if (!is.null(ctx$pkg_scope)) return(package_function(ctx$pkg_scope, fn, ctx, exported_only = FALSE))
  if (!is.null(ctx$functions[[fn]])) return(ctx$functions[[fn]])
  for (pkg in ctx$packages) {
    def <- package_function(pkg, fn, ctx)
    if (!is.null(def)) return(def)
  }
  NULL
}

# Column set of a call to a project function, or NULL.
negative_expectations <- c("expect_error", "expect_warning", "expect_condition", "expect_snapshot_error",
                           "expect_snapshot_warning", "expect_failure")

call_user_function <- function(e, name, ctx, def = lookup_function(e, name, ctx)) {
  if (is.null(def) || def$file %in% ctx$call_stack || length(ctx$call_stack) >= 4L) return(NULL)
  if (isTRUE(ctx$in_negative_test)) return(NULL)
  saved <- list(frames = ctx$frames, variables = ctx$variables, unknown = ctx$unknown_frames,
                line = ctx$line, line_end = ctx$line_end, file = ctx$file, call_stack = ctx$call_stack,
                call_sites = ctx$call_sites, in_function = ctx$in_function, pkg_scope = ctx$pkg_scope)
  result <- guarded(ctx, def$file, follow_function(e, def, ctx))
  ctx$frames <- saved$frames
  ctx$variables <- saved$variables
  ctx$unknown_frames <- saved$unknown
  ctx$line <- saved$line
  ctx$line_end <- saved$line_end
  ctx$file <- saved$file
  ctx$call_stack <- saved$call_stack
  ctx$call_sites <- saved$call_sites
  ctx$in_function <- saved$in_function
  ctx$pkg_scope <- saved$pkg_scope
  # Columns that came out of a package function: say so, since the function
  # may have dropped or renamed what the cached source had.
  if (!is.null(result) && !is.null(def$pkg) && is.null(saved$pkg_scope)) {
    attr(result, "source") <- sprintf("%s() (read from its code)", def$file)
  }
  result
}

follow_function <- function(e, def, ctx) {
  fdef <- def$fn
  proto <- quiet(eval(call("function", fdef[[2L]], NULL)))
  if (!is.function(proto)) return(NULL)
  # forwarded dots (`f(x, ...)`) cannot be matched statically: drop them
  args <- call_args(e)
  args <- args[!vapply(args, function(a) is.symbol(a) && identical(as.character(a), "..."), TRUE)]
  matched <- quiet(match.call(proto, as.call(c(e[[1L]], args)), expand.dots = FALSE))
  if (is.null(matched)) return(NULL)
  supplied <- as.list(matched)[-1L]
  params <- setdiff(names(fdef[[2L]]) %||% character(), "...")
  arg_cols <- list()
  for (p in names(supplied)) {
    a <- supplied[[p]]
    if (!(is.symbol(a) || is.call(a))) next
    arg_cols[[p]] <- frame_columns(a, ctx, used = param_used_as_frame(fdef[[3L]], p))
  }
  # `if ("x" %in% names(df))`: the body handles frames with and without x,
  # so x counts as present for the parameter rather than reported
  guarded <- guarded_columns(fdef[[3L]])
  for (p in params) {
    cols <- arg_cols[[p]]
    if (!is.null(cols) && length(guarded)) cols <- keep_source(structure(unique(c(cols, guarded)), groups = attr(cols, "groups")), cols)
    ctx$frames[[p]] <- cols
    if (is.null(cols)) ctx$unknown_frames <- union(ctx$unknown_frames, p)
    ctx$variables <- union(ctx$variables, p)
  }
  ctx$call_sites <- c(ctx$call_sites, sprintf("%s:%s", relative_path(ctx$file, ctx$root), ctx$line))
  ctx$file <- def$file
  ctx$call_stack <- c(ctx$call_stack, def$file)
  ctx$in_function <- TRUE
  ctx$pkg_scope <- def$pkg
  body_columns(fdef[[3L]], ctx)
}

# Column names a body tests for before using: string literals inside `if`
# conditions that look at names()/colnames()/has_name()/hasName().
guarded_columns <- function(e, acc = character()) {
  if (!is.call(e)) return(acc)
  if (identical(call_name(e), "if") && length(e) >= 2L) {
    cond <- e[[2L]]
    if (any(all.names(cond) %in% c("names", "colnames", "has_name", "hasName", "exists"))) {
      acc <- c(acc, string_literals(cond))
    }
  }
  for (a in call_args(e)) acc <- guarded_columns(a, acc)
  unique(acc)
}

# Does the body pipe parameter `p` into a verb, or pass it as a verb's data?
param_used_as_frame <- function(e, p) {
  if (!is.call(e)) return(FALSE)
  fn <- call_name(e)
  args <- call_args(e)
  first <- if (length(args)) args[[1L]] else NULL
  if (!is.na(fn) && (fn %in% c("%>%", "%<>%", "%T>%") || fn %in% verb_names) &&
      is.symbol(first) && identical(as.character(first), p)) return(TRUE)
  for (a in args) if (is.call(a) && param_used_as_frame(a, p)) return(TRUE)
  FALSE
}

# `lhs %>% f(...)`: the call f(lhs, ...) (or with lhs in place of `.`).
pipe_into <- function(rhs, lhs_symbol) {
  args <- call_args(rhs)
  dots <- vapply(args, function(a) is.symbol(a) && identical(as.character(a), "."), TRUE)
  if (any(dots)) args[dots] <- list(lhs_symbol) else args <- c(list(lhs_symbol), args)
  as.call(c(rhs[[1L]], args))
}

# --- reading column names without loading rows ------------------------------

header_delim <- function(path, sep = NULL) {
  if (requireNamespace("readr", quietly = TRUE)) {
    df <- quiet(if (is.null(sep)) readr::read_csv(path, n_max = 0, show_col_types = FALSE, progress = FALSE)
                else readr::read_delim(path, delim = sep, n_max = 0, show_col_types = FALSE, progress = FALSE))
    if (!is.null(df)) return(names(df))
  }
  line <- quiet(readLines(path, n = 1L, warn = FALSE))
  if (is.null(line) || !length(line)) return(NULL)
  if (is.null(sep)) {
    counts <- c(lengths(regmatches(line, gregexpr("\t", line))),
                lengths(regmatches(line, gregexpr(";", line))),
                lengths(regmatches(line, gregexpr(",", line))))
    sep <- c("\t", ";", ",")[which.max(counts)]
  }
  nm <- quiet(scan(text = line, what = "", sep = sep, quiet = TRUE, strip.white = TRUE))
  if (is.null(nm)) return(NULL)
  gsub('^"|"$', "", nm)
}

columns_of_file <- function(path, reader) {
  if (!file.exists(path) || dir.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  delim_readers <- c("read_csv", "read.csv", "fread", "vroom", "read_delim", "read.delim", "read_tsv",
                     "read_csv2", "read.csv2", "read.table")
  if (reader %in% delim_readers || ext %in% c("csv", "tsv", "txt", "dat")) {
    sep <- switch(reader, read_tsv = "\t", read.delim = "\t", read_csv2 = ";", read.csv2 = ";", NULL)
    return(header_delim(path, sep))
  }
  if (ext %in% c("parquet", "feather", "arrow") || reader %in% c("read_parquet", "read_feather", "open_dataset")) {
    if (!requireNamespace("arrow", quietly = TRUE)) return(NULL)
    sch <- quiet(if (ext == "parquet" || reader %in% c("read_parquet", "open_dataset"))
      arrow::open_dataset(path)$schema else arrow::read_feather(path, as_data_frame = FALSE)$schema)
    return(if (is.null(sch)) NULL else names(sch))
  }
  if (ext == "rds" || reader %in% c("readRDS", "read_rds")) {
    if (file.size(path) > 20 * 1024^2) return(NULL)
    obj <- quiet(readRDS(path))
    return(if (is.data.frame(obj)) names(obj) else NULL)
  }
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) return(NULL)
    df <- quiet(readxl::read_excel(path, n_max = 0))
    return(if (is.null(df)) NULL else names(df))
  }
  if (ext %in% c("dta", "sav")) {
    if (!requireNamespace("haven", quietly = TRUE)) return(NULL)
    df <- quiet(if (ext == "dta") haven::read_dta(path, n_max = 0) else haven::read_sav(path, n_max = 0))
    return(if (is.null(df)) NULL else names(df))
  }
  if (ext == "fst" && requireNamespace("fst", quietly = TRUE)) {
    m <- quiet(fst::metadata_fst(path))
    return(if (is.null(m)) NULL else m$columnNames)
  }
  NULL
}

readers <- c("read_csv", "read_csv2", "read_tsv", "read_delim", "read.csv", "read.csv2", "read.delim",
             "read.table", "fread", "vroom", "readRDS", "read_rds", "read_parquet", "read_feather",
             "open_dataset", "read_excel", "read_xlsx", "read_dta", "read_sav")

# --- expression helpers -----------------------------------------------------

# `pkg::name` or `pkg:::name` as a call head: the bare name, else NA. The head
# of `pkg::factory(x)()` is a call whose own head is another call, never a
# symbol, so every helper that reads a head goes through here.
ns_name <- function(h) {
  if (is.call(h) && length(h) == 3L && is.symbol(h[[1L]]) && as.character(h[[1L]]) %in% c("::", ":::") &&
      (is.symbol(h[[3L]]) || is.character(h[[3L]]))) as.character(h[[3L]]) else NA_character_
}

call_name <- function(e) {
  if (!is.call(e)) return(NA_character_)
  h <- e[[1L]]
  if (is.symbol(h)) return(as.character(h))
  ns_name(h)
}

string_arg <- function(e) if (is.character(e) && length(e) == 1L) e else NULL

resolve_path <- function(e, ctx) {
  if (is.character(e) && length(e) == 1L) {
    cands <- c(e, file.path(ctx$root, e), file.path(ctx$filedir, e))
    hit <- cands[file.exists(cands)]
    return(if (length(hit)) normalizePath(hit[[1L]]) else NULL)
  }
  if (is.call(e) && call_name(e) %in% c("here", "file.path")) {
    parts <- lapply(call_args(e), string_arg)
    if (any(vapply(parts, is.null, TRUE))) return(NULL)
    return(resolve_path(do.call(file.path, parts), ctx))
  }
  NULL
}

builtin_dataset <- function(nm, ctx) {
  for (pkg in c("datasets", ctx$packages)) {
    if (!requireNamespace(pkg, quietly = TRUE)) next
    obj <- quiet(get0(nm, envir = asNamespace(pkg), inherits = FALSE))
    if (is.null(obj)) {
      lazy <- quiet(asNamespace(pkg)[[".__NAMESPACE__."]][["lazydata"]])
      if (!is.null(lazy)) obj <- quiet(get0(nm, envir = lazy, inherits = FALSE))
    }
    if (is.data.frame(obj)) {
      cols <- names(obj)
      attr(cols, "source") <- paste0(pkg, "::", nm)
      return(cols)
    }
  }
  NULL
}

keep_source <- function(cols, template) {
  attr(cols, "source") <- attr(template, "source")
  cols
}

# Column set of an expression that produces a data frame, or NULL (unknown).
# `used`: the caller applies a column-using verb to the result, so an
# unresolved frame here is worth a note.
frame_columns <- function(e, ctx, used = FALSE) {
  if (is.symbol(e)) {
    nm <- as.character(e)
    if (!is.null(ctx$frames[[nm]])) return(ctx$frames[[nm]])
    if (nm %in% ctx$unknown_frames) {
      if (used) mark_used(nm, ctx)
      return(NULL)
    }
    if (nm %in% ctx$variables) return(NULL)
    return(builtin_dataset(nm, ctx))
  }
  if (!is.call(e)) return(NULL)
  fn <- call_name(e)
  if (is.na(fn)) return(NULL)
  if (fn %in% c("%>%", "%<>%", "%T>%")) {
    rhs <- e[[3L]]
    if (is.symbol(rhs)) rhs <- as.call(list(rhs))
    if (!is.call(rhs)) return(NULL)
    rhs_fn <- call_name(rhs)
    rhs_def <- lookup_function(rhs, rhs_fn, ctx)
    lhs <- frame_columns(e[[2L]], ctx, used = used || rhs_fn %in% verb_names || !is.null(rhs_def))
    if (is.null(lhs)) {
      if (!is.na(rhs_fn)) count_step(ctx, "skipped", rhs_fn)
      return(NULL)
    }
    if (!is.null(rhs_def)) {
      ctx$frames[[".colcheck_lhs"]] <- lhs
      return(call_user_function(pipe_into(rhs, as.name(".colcheck_lhs")), rhs_fn, ctx, def = rhs_def))
    }
    return(apply_verb(rhs, lhs, ctx, piped = TRUE))
  }
  if (fn %in% readers) {
    path_arg <- if (length(e) >= 2L) e[[2L]] else NULL
    nms <- names(e)
    if (!is.null(nms)) {
      for (k in c("file", "path", "input", "sources")) if (k %in% nms) path_arg <- e[[k]]
    }
    path <- resolve_path(path_arg, ctx)
    if (is.null(path)) return(cached_columns(e, ctx))
    cols <- columns_of_file(path, fn)
    if (!is.null(cols)) attr(cols, "source") <- basename(path)
    return(cols)
  }
  if (fn %in% c("tibble", "data.frame", "data_frame")) {
    nms <- names(e)[-1L]
    if (is.null(nms) || any(!nzchar(nms))) return(NULL)
    cols <- nms
    attr(cols, "source") <- paste0(fn, "() literal")
    return(cols)
  }
  if (fn == "tribble") {
    hdr <- Filter(function(a) is.call(a) && identical(a[[1L]], as.name("~")), call_args(e))
    cols <- vapply(hdr, function(a) as.character(a[[2L]]), "")
    if (!length(cols)) return(NULL)
    attr(cols, "source") <- "tribble() literal"
    return(cols)
  }
  # native pipe is parsed into nested calls: verb(frame_expr, ...); a ggplot
  # chain is `+` calls nested the same way
  if (fn %in% verb_names || fn == "+") {
    if (length(e) < 2L) return(NULL)
    inner <- frame_columns(e[[2L]], ctx, used = TRUE)
    if (!is.null(inner)) return(apply_verb(e, inner, ctx, piped = FALSE))
    count_step(ctx, "skipped", fn)
    return(NULL)
  }
  if (fn %in% c("{", "(")) return(if (length(e) >= 2L) frame_columns(e[[length(e)]], ctx, used = used) else NULL)
  if (fn %in% c("function", "if", "for", "while", "repeat", "$", "@", "[", "[[")) return(NULL)
  # any other call: a cache entry matching one of its literals, or the line's
  # `# cols:`, is the user's own statement of what comes out and beats reading
  # the function (a loader's columns depend on its arguments, not its code)
  cached <- cached_columns(e, ctx)
  if (!is.null(cached)) return(cached)
  # a project or package function: bind its parameters, check its body, take its result
  fn_def <- lookup_function(e, fn, ctx)
  if (!is.null(fn_def)) return(call_user_function(e, fn, ctx, def = fn_def))
  # any other call around a frame expression, `tryCatch({ pipeline }, ...)`,
  # `suppressWarnings(pipeline)`: the pipeline inside is checked, and a wrapper
  # that returns its argument passes the columns through
  if (length(e) >= 2L) {
    inner <- frame_columns(e[[2L]], ctx, used = used)
    if (!is.null(inner) && fn %in% passthrough_calls) return(inner)
  }
  NULL
}

# Calls whose value is their first argument, as far as columns go.
passthrough_calls <- c("tryCatch", "try", "withCallingHandlers", "suppressWarnings", "suppressMessages",
                       "invisible", "identity", "local", "force", "structure")

# Calls whose arguments are selections or quoted code, never bare columns to check.
quoting_calls <- c("across", "if_any", "if_all", "pick", "c_across", "where", "all_of", "any_of",
                   "starts_with", "ends_with", "contains", "matches", "num_range", "everything",
                   "last_col", "vars", "quote", "bquote", "expr", "sym", "syms", "!!", "!!!",
                   "glue", "str_glue")

# Names used as bare symbols inside an expression evaluated with data masking,
# minus the names the expression itself assigns (`{ idx <- which(...); x[idx] }`)
# or binds (`function(i) ...`, `for (i in ...)`).
masked_symbols <- function(e) {
  setdiff(collect_symbols(e), local_names(e))
}

collect_symbols <- function(e, acc = character()) {
  if (is.symbol(e)) return(c(acc, as.character(e)))
  if (!is.call(e)) return(acc)
  fn <- call_name(e)
  if (!is.na(fn) && fn %in% c("$", "@", "::", ":::")) return(acc)
  if (!is.na(fn) && fn %in% quoting_calls) return(c(acc, island_selection(e) %||% character()))
  for (p in call_args(e)) acc <- collect_symbols(p, acc)
  acc
}

# A tidy-select island inside data-masked code: `across(<selection>, .fns)`,
# `pick(<selection>)`, `if_any(<selection>, ...)`, `c_across(<selection>)`,
# `all_of("a")`. The selection slot is resolved like a select() argument and
# its names are checked; the function slot (`.fns`, a lambda) is never
# walked, since it holds function names, not columns. A selection the
# resolver cannot read (`starts_with()`, `where()`, `all_of(variable)`,
# `any_of()`) leaves the whole island unchecked, as before.
island_selection <- function(e) {
  fn <- call_name(e)
  if (is.na(fn)) return(NULL)
  args <- call_args(e)
  nms <- names(args) %||% rep("", length(args))
  if (fn == "all_of") return(selection_names(list(e)))
  if (fn == "pick") return(selection_names(args))
  if (fn %in% c("across", "if_any", "if_all", "c_across")) {
    slot <- if (fn == "c_across") "cols" else ".cols"
    sel <- if (slot %in% nms) args[[slot]] else if (any(!nzchar(nms))) args[!nzchar(nms)][[1L]] else NULL
    if (is.null(sel)) return(NULL)
    return(selection_names(list(sel)))
  }
  NULL
}

# The grouping columns a group_by() names: a selection, an island, or a
# computed group (`group_by(yr = year(date))`, which also adds the column).
# NULL when any of them cannot be resolved.
grouping_names <- function(pos) {
  out <- character()
  for (a in pos) {
    g <- if (is.call(a) && call_name(a) %in% c("across", "pick")) island_selection(a) else selection_names(list(a))
    if (is.null(g)) return(NULL)
    out <- c(out, g)
  }
  out
}

local_names <- function(e, acc = character()) {
  if (!is.call(e)) return(acc)
  fn <- call_name(e)
  if (!is.na(fn) && fn %in% c("<-", "=", "<<-") && length(e) == 3L && is.symbol(e[[2L]])) acc <- c(acc, as.character(e[[2L]]))
  if (!is.na(fn) && fn == "for" && is.symbol(e[[2L]])) acc <- c(acc, as.character(e[[2L]]))
  if (!is.na(fn) && fn == "function") acc <- c(acc, names(e[[2L]]) %||% character())
  for (p in call_args(e)) acc <- local_names(p, acc)
  acc
}

selection_names <- function(args) {
  out <- character()
  for (a in args) {
    if (is.symbol(a)) out <- c(out, as.character(a))
    else if (is.character(a)) out <- c(out, a)
    else if (is.call(a) && call_name(a) %in% c("-", "!") && length(a) == 2L) {
      inner <- selection_names(list(a[[2L]]))
      if (is.null(inner)) return(NULL)
      out <- c(out, inner)
    } else if (is.call(a) && call_name(a) %in% c(":", "c")) {
      inner <- selection_names(as.list(a)[-1L])
      if (is.null(inner)) return(NULL)
      out <- c(out, inner)
    } else if (is.call(a) && call_name(a) == "all_of" && length(a) == 2L) {
      # all_of("a") or all_of(c("a", "b")): the names must exist, so they are
      # checked; all_of(keys) with a variable, or any_of() (missing names
      # allowed), stays unresolved
      inner <- if (is.character(a[[2L]])) a[[2L]] else
        if (is.call(a[[2L]]) && identical(call_name(a[[2L]]), "c") &&
            all(vapply(as.list(a[[2L]])[-1L], is.character, TRUE))) unlist(as.list(a[[2L]])[-1L]) else NULL
      if (is.null(inner)) return(NULL)
      out <- c(out, inner)
    } else return(NULL)
  }
  out
}

# Lines of a file the walker is in (the checked file or a project function's).
file_lines <- function(ctx, file) {
  hit <- ctx$line_cache[[file]]
  if (is.null(hit)) {
    hit <- quiet(readLines(file, warn = FALSE), character())
    ctx$line_cache[[file]] <- hit
  }
  hit
}

# The line to report for `name`: ctx$line is where the current statement
# starts, which for a pipeline is its first line; the first line of the
# statement that mentions the name (outside a comment) is the one to show.
pin_line <- function(ctx, name) {
  if (is.null(ctx$line) || is.na(ctx$line)) return(ctx$line)
  txt <- file_lines(ctx, ctx$file)
  to <- ctx$line_end
  if (is.null(to) || is.na(to) || to < ctx$line) to <- ctx$line
  rng <- seq.int(ctx$line, min(to, length(txt)))
  if (!length(rng) || rng[[1L]] > length(txt)) return(ctx$line)
  pat <- paste0("(^|[^A-Za-z0-9._])`?", gsub("([.])", "\\\\\\1", name), "`?([^A-Za-z0-9._]|$)")
  code <- sub("#.*$", "", txt[rng])
  hit <- rng[grepl(pat, code, perl = TRUE)]
  if (length(hit)) hit[[1L]] else ctx$line
}

# Pipeline steps seen: checked against a known column set, or skipped
# because the frame's columns are unknowable. Keyed so that the walker
# visiting a pipeline twice counts it once.
count_step <- function(ctx, kind, fn) {
  if (!(fn %in% verbs) || !is.null(ctx$pkg_scope)) return(invisible())
  ctx$stats[[kind]] <- union(ctx$stats[[kind]], paste(ctx$file, ctx$line, fn))
  invisible()
}

report <- function(ctx, name, cols) {
  if (!is.null(ctx$pkg_scope)) return(invisible())
  src <- attr(cols, "source") %||% "the data"
  via <- if (length(ctx$call_sites)) ctx$call_sites[[length(ctx$call_sites)]] else NULL
  ctx$findings[[length(ctx$findings) + 1L]] <- list(
    kind = "column", file = ctx$file, line = pin_line(ctx, name), name = name, source = src,
    columns = as.character(cols), via = via,
    message = sprintf("'%s' is not a column at this point (columns: %s; data from %s%s)", name,
                      paste(head(cols, 12), collapse = ", "), src,
                      if (is.null(via)) "" else sprintf(", called from %s", via)))
}

check_symbols <- function(syms, cols, ctx) {
  syms <- unique(syms)
  bad <- syms[!(syms %in% cols) & !(syms %in% ctx$variables) & !(syms %in% ctx$known_names) &
              !vapply(syms, function(s) exists(s, envir = baseenv()), TRUE)]
  for (b in bad) report(ctx, b, cols)
  invisible(bad)
}

# Apply one verb call to a known column set; returns the new set or NULL (unknown).
apply_verb <- function(call, cols, ctx, piped) {
  fn <- call_name(call)
  if (is.na(fn)) return(NULL)
  count_step(ctx, "checked", fn)
  args <- call_args(call)
  if (!piped && length(args)) args <- args[-1L]
  nms <- names(args) %||% rep("", length(args))
  dot_args <- startsWith(nms, ".")
  pos <- args[!nzchar(nms)]
  named <- args[nzchar(nms) & !dot_args]
  group_cols <- attr(cols, "groups") %||% character()
  groups_unknown <- anyNA(group_cols)
  with_groups <- function(x, g = group_cols) { attr(x, "groups") <- g; keep_source(x, cols) }
  check_all <- function() check_symbols(unlist(lapply(c(pos, named), masked_symbols)), cols, ctx)
  # mutate(a = ..., b = a + 1): each argument sees the columns made before it
  check_sequential <- function() {
    cur <- cols
    for (i in seq_along(args)) {
      if (dot_args[[i]]) next
      check_symbols(masked_symbols(args[[i]]), cur, ctx)
      if (nzchar(nms[[i]])) cur <- unique(c(cur, nms[[i]]))
    }
  }
  has_nonsymbol_pos <- function() any(vapply(pos, function(p) !is.symbol(p), TRUE))
  # `.by =` is the one dot-argument that names columns of the incoming frame
  # (bare, `c(a, b)`, a helper); check it like group_by()'s arguments. When a
  # name is wrong the result is unknown: the pipeline fails there at run time,
  # and carrying the misspelling into the output would only flag the correct
  # name downstream.
  by_sel <- if (".by" %in% nms) selection_names(list(args[[".by"]])) else NULL
  if (!is.null(by_sel) && length(check_symbols(by_sel, cols, ctx))) return(NULL)

  if (fn %in% c("filter", "arrange", "slice", "slice_head", "slice_tail", "slice_min", "slice_max",
                "slice_sample", "drop_na", "fill", "relocate", "ungroup", "rowwise", "glimpse", "print",
                "head", "tail", "collect", "compute", "as_tibble", "as.data.frame")) {
    check_all()
    return(with_groups(cols, if (fn == "ungroup") character() else group_cols))
  }
  if (fn == "group_by") {
    check_all()
    g <- grouping_names(pos)
    computed <- names(named)
    add <- isTRUE(args[[".add"]] %||% FALSE)
    # a grouping that cannot be resolved is unknown (NA), never empty: the
    # columns stay known for mutate()/filter(), and the verbs whose output is
    # built from the groups (summarise, transmute, count) give up instead of
    # reporting every grouping column downstream as missing
    groups <- if (is.null(g) || (add && groups_unknown)) NA_character_ else unique(c(if (add) group_cols, g, computed))
    return(with_groups(unique(c(cols, computed)), groups))
  }
  if (fn %in% c("mutate", "transmute")) {
    check_sequential()
    if (has_nonsymbol_pos()) return(NULL)
    new <- names(named)
    if (fn == "mutate") return(with_groups(unique(c(cols, new))))
    if (groups_unknown) return(NULL)
    return(with_groups(unique(c(group_cols, new))))
  }
  if (fn %in% c("summarise", "summarize", "reframe")) {
    check_sequential()
    if (has_nonsymbol_pos()) return(NULL)
    by <- if (".by" %in% nms) by_sel else group_cols
    if (is.null(by) || anyNA(by)) return(NULL)
    return(with_groups(unique(c(by, names(named))), character()))
  }
  if (fn == "select") {
    sel <- selection_names(c(pos, unname(named)))
    if (is.null(sel)) {
      check_symbols(unlist(lapply(pos, masked_symbols)), cols, ctx)
      return(NULL)
    }
    check_symbols(sel, cols, ctx)
    neg <- any(vapply(pos, function(p) is.call(p) && call_name(p) %in% c("-", "!"), TRUE))
    renamed_from <- unlist(lapply(named, function(a) if (is.symbol(a) || is.character(a)) as.character(a) else NULL))
    kept <- if (neg) setdiff(cols, sel) else c(names(named), sel[!(sel %in% renamed_from)])
    return(with_groups(unique(kept)))
  }
  if (fn == "rename") {
    old <- vapply(named, function(a) if (is.symbol(a) || is.character(a)) as.character(a) else NA_character_, "")
    if (anyNA(old) || !length(old)) return(NULL)
    check_symbols(old, cols, ctx)
    return(with_groups(unique(c(setdiff(cols, old), names(named)))))
  }
  if (fn %in% c("count", "tally", "add_count")) {
    check_all()
    g <- selection_names(pos)
    if (is.null(g)) return(NULL)
    nm <- if (!is.null(args[["name"]])) string_arg(args[["name"]]) %||% "n" else "n"
    if (fn != "add_count" && groups_unknown) return(NULL)
    return(with_groups(if (fn == "add_count") unique(c(cols, nm)) else unique(c(group_cols, g, nm)), character()))
  }
  if (fn == "distinct") {
    check_all()
    g <- selection_names(pos)
    if (is.null(g)) return(NULL)
    if (!length(g) || isTRUE(args[[".keep_all"]] %||% FALSE)) return(with_groups(cols))
    if (groups_unknown) return(NULL)
    return(with_groups(unique(c(group_cols, g))))
  }
  if (fn == "pivot_longer") {
    spec <- if (length(pos)) pos[[1L]] else args[["cols"]]
    if (is.null(spec)) return(NULL)
    sel <- selection_names(list(spec))
    if (is.null(sel)) return(NULL)
    check_symbols(sel, cols, ctx)
    neg <- is.call(spec) && call_name(spec) %in% c("-", "!")
    gone <- if (neg) setdiff(cols, sel) else sel
    nt <- string_arg(args[["names_to"]]) %||% "name"
    vt <- string_arg(args[["values_to"]]) %||% "value"
    return(with_groups(unique(c(setdiff(cols, gone), nt, vt))))
  }
  if (fn %in% c("left_join", "right_join", "inner_join", "full_join", "semi_join", "anti_join")) {
    by <- args[["by"]]
    if (!is.null(by)) {
      lhs_names <- if (is.character(by)) { n <- names(by); if (is.null(n)) by else ifelse(nzchar(n), n, by) } else NULL
      if (!is.null(lhs_names)) check_symbols(lhs_names, cols, ctx)
    }
    if (fn %in% c("semi_join", "anti_join")) return(with_groups(cols))
    other <- frame_columns(if (length(pos)) pos[[1L]] else args[["y"]], ctx, used = TRUE)
    if (is.null(other)) return(NULL)
    return(with_groups(unique(c(cols, other))))
  }
  if (fn %in% c("pivot_wider")) {
    for (k in c("names_from", "values_from", "id_cols")) if (!is.null(args[[k]])) {
      s <- selection_names(list(args[[k]]))
      if (!is.null(s)) check_symbols(s, cols, ctx)
    }
    return(NULL)
  }
  if (fn == "ggplot") {
    for (a in c(pos, named)) if (is.call(a) && identical(call_name(a), "aes")) check_symbols(masked_symbols(a), cols, ctx)
    return(with_groups(cols))
  }
  NULL
}

# ggplot chains: ggplot(df, aes(...)) + layer(aes(...)) ...
check_ggplot <- function(e, ctx) {
  if (!is.call(e) || !identical(call_name(e), "+") || length(e) != 3L) return(invisible())
  base <- e[[2L]]
  while (is.call(base) && identical(call_name(base), "+")) base <- base[[2L]]
  if (!is.call(base) || !identical(call_name(base), "ggplot") || length(base) < 2L) return(invisible())
  cols <- frame_columns(base[[2L]], ctx, used = TRUE)
  if (is.null(cols)) return(invisible())
  layers <- list()
  node <- e
  while (is.call(node) && identical(call_name(node), "+")) {
    layers <- c(layers, list(node[[3L]]))
    node <- node[[2L]]
  }
  for (l in c(list(base), layers)) {
    if (!is.call(l)) next
    for (a in as.list(l)[-1L]) if (is.call(a) && identical(call_name(a), "aes")) check_symbols(masked_symbols(a), cols, ctx)
  }
  invisible()
}

# Verb calls the walker checks when they appear on their own.
verbs <- c("mutate", "filter", "summarise", "summarize", "select", "arrange", "group_by", "rename",
           "transmute", "distinct", "count", "pivot_longer", "pivot_wider", "ggplot", "slice", "reframe",
           "left_join", "inner_join", "right_join", "full_join", "semi_join", "anti_join")

# Everything apply_verb() understands: a call to one of these is a pipeline
# step, never a data source.
verb_names <- unique(c(verbs, "slice_head", "slice_tail", "slice_min", "slice_max", "slice_sample",
                       "drop_na", "fill", "relocate", "ungroup", "rowwise", "glimpse", "print", "head",
                       "tail", "collect", "compute", "as_tibble", "as.data.frame", "tally", "add_count"))

# --- argument names ----------------------------------------------------------
# A call to a project function with an argument name its definition does not
# have runs anyway when R can partial-match it (flow_metric= for flow_metrics)
# and fails only at run time otherwise; lintr does not look. Functions with
# `...` accept anything, so they are not checked; neither are names that a
# default package also defines (a project's own `filter` is not the call's
# target for sure).

builtin_name <- function(nm) {
  for (p in c("base", "stats", "utils", "methods", "graphics", "grDevices")) {
    if (exists(nm, envir = asNamespace(p), inherits = FALSE)) return(TRUE)
  }
  FALSE
}

check_call_args <- function(e, name, ctx) {
  def <- ctx$functions[[name]]
  if (is.null(def) || isTRUE(ctx$in_negative_test) || builtin_name(name)) return(invisible())
  head <- e[[1L]]
  if (is.call(head)) {
    # pkg::f(): only the project's own package names these functions
    if (is.null(ctx$pkg_name) || !identical(as.character(head[[2L]]), ctx$pkg_name)) return(invisible())
  }
  formals <- names(def$fn[[2L]]) %||% character()
  if ("..." %in% formals) return(invisible())
  args <- as.list(e)[-1L]
  nms <- names(args) %||% rep("", length(args))
  forwards <- any(vapply(args, function(a) is.symbol(a) && identical(as.character(a), "..."), TRUE))
  shown <- paste(formals, collapse = ", ")
  used <- character()
  add <- function(nm, message) {
    ctx$findings[[length(ctx$findings) + 1L]] <<- list(
      kind = "argument", file = ctx$file, line = pin_line(ctx, nm), name = nm, source = NA_character_,
      columns = character(), message = message)
  }
  for (nm in unique(nms[nzchar(nms)])) {
    if (nm %in% formals) { used <- c(used, nm); next }
    partial <- formals[startsWith(formals, nm)]
    if (length(partial) == 1L) {
      used <- c(used, partial)
      add(nm, sprintf("%s() has no argument '%s'; R partial-matches it to '%s' silently, so use the full name",
                      name, nm, partial))
    } else if (length(partial) > 1L) {
      add(nm, sprintf("'%s' is ambiguous in %s(): it partially matches %s", nm, name, paste(partial, collapse = " and ")))
    } else {
      add(nm, sprintf("%s() has no argument '%s' (its arguments are: %s)", name, nm, shown))
    }
  }
  npos <- sum(!nzchar(nms))
  if (!forwards && npos > length(formals) - length(unique(used))) {
    add(name, sprintf("%s() takes %d argument(s) (%s) but %d were given", name, length(formals), shown, length(args)))
  }
  invisible()
}

# `x %>% f(a = 1)`: f(x, a = 1) is the call to check, for every step of the chain.
check_pipe_args <- function(e, ctx) {
  while (is.call(e) && call_name(e) %in% c("%>%", "%<>%", "%T>%") && length(e) == 3L) {
    rhs <- e[[3L]]
    if (is.symbol(rhs)) rhs <- as.call(list(rhs))
    if (is.call(rhs)) {
      fn <- call_name(rhs)
      if (!is.na(fn) && !is.null(ctx$functions[[fn]])) check_call_args(pipe_into(rhs, as.name(".lhs")), fn, ctx)
    }
    e <- e[[2L]]
  }
  invisible()
}

walk_expr <- function(e, ctx) {
  if (!is.call(e)) return(invisible())
  fn <- call_name(e)
  if (!is.na(fn) && fn %in% c("<-", "=", "<<-") && length(e) == 3L && is.symbol(e[[2L]])) {
    target <- as.character(e[[2L]])
    if (is.call(e[[3L]]) && identical(call_name(e[[3L]]), "function")) {
      # a named function: check its body now with unknown parameters (its
      # own loaders and globals resolve); calls with known frames check it again
      ctx$variables <- union(ctx$variables, target)
      walk_expr(e[[3L]], ctx)
      return(invisible())
    }
    key <- line_annotation(ctx)
    ctx$annotation <- key
    ctx$annotation_head <- pipeline_head(e[[3L]])
    cols <- frame_columns(e[[3L]], ctx)
    # an annotation no source consumed (the right-hand side is a symbol or a
    # user function) still names the columns of the result
    if (is.null(cols) && !is.null(ctx$annotation)) cols <- schema_columns(ctx$annotation, ctx)
    ctx$annotation <- NULL
    ctx$annotation_head <- NULL
    # nested expressions first, while the target still means what it meant
    # before this line: in `x <- x %>% a() %>% b()` every step reads the old x
    check_ggplot(e[[3L]], ctx)
    if (is.call(e[[3L]]) && !identical(call_name(e[[3L]]), "function")) walk_expr(e[[3L]], ctx)
    if (!is.null(cols)) {
      ctx$frames[[target]] <- cols
      ctx$unresolved[[target]] <- NULL
    } else {
      ctx$frames[[target]] <- NULL
      ctx$unknown_frames <- union(ctx$unknown_frames, target)
      note_unresolved(target, e[[3L]], key, ctx)
    }
    ctx$variables <- union(ctx$variables, target)
    return(invisible())
  }
  if (!is.na(fn) && fn %in% c("library", "require")) {
    if (length(e) >= 2L && is.symbol(e[[2L]])) ctx$packages <- union(ctx$packages, as.character(e[[2L]]))
    return(invisible())
  }
  if (!is.na(fn) && fn == "function") {
    params <- names(e[[2L]]) %||% character()
    saved_vars <- ctx$variables
    saved_unknown <- ctx$unknown_frames
    ctx$variables <- union(ctx$variables, params)
    ctx$unknown_frames <- union(ctx$unknown_frames, params)
    saved_in <- ctx$in_function
    ctx$in_function <- TRUE
    walk_expr(e[[3L]], ctx)
    ctx$in_function <- saved_in
    ctx$variables <- saved_vars
    ctx$unknown_frames <- saved_unknown
    return(invisible())
  }
  # negative tests pass deliberately wrong frames: do not follow calls inside
  if (!is.na(fn) && fn %in% negative_expectations) {
    saved_neg <- ctx$in_negative_test
    ctx$in_negative_test <- TRUE
    for (p in call_args(e)) if (is.call(p)) walk_expr(p, ctx)
    ctx$in_negative_test <- saved_neg
    return(invisible())
  }
  if (!is.na(fn) && fn %in% c("%>%", "%<>%", "%T>%")) {
    check_pipe_args(e, ctx)
    frame_columns(e, ctx)
    return(invisible())
  }
  if (!is.na(fn) && fn == "+") {
    check_ggplot(e, ctx)
    return(invisible())
  }
  if (!is.na(fn) && fn == "{") return(walk_block(e, ctx))
  if (!is.na(fn) && !is.null(ctx$functions[[fn]])) check_call_args(e, fn, ctx)
  if (!is.na(fn) && (fn %in% verbs || !is.null(ctx$functions[[fn]]))) frame_columns(e, ctx)
  for (p in call_args(e)) if (is.call(p)) walk_expr(p, ctx)
  invisible()
}

colcheck_file <- function(file, root = NULL) {
  ctx <- new.env()
  ctx$file <- file
  ctx$root <- root %||% colcheck_root(file)
  use_project_library(ctx$root)
  ctx$filedir <- dirname(normalizePath(file, mustWork = FALSE))
  ctx$frames <- list()
  ctx$unknown_frames <- character()
  ctx$variables <- character()
  ctx$packages <- character()
  ctx$findings <- list()
  ctx$schemas <- load_schemas(ctx$root)
  ctx$unresolved <- list()
  ctx$stale <- list()
  ctx$annotation <- NULL
  ctx$src_lines <- quiet(readLines(file, warn = FALSE), character())
  ctx$line_cache <- list()
  ctx$line_end <- NA_integer_
  ctx$stats <- list(checked = character(), skipped = character())
  ctx$pkg_name <- if (file.exists(file.path(ctx$root, "DESCRIPTION")))
    quiet(unname(read.dcf(file.path(ctx$root, "DESCRIPTION"), fields = "Package")[1L, 1L])) else NULL
  ctx$known_names <- c("TRUE", "FALSE", "NA", "NULL", "T", "F", "Inf", "NaN", ".", ".x", ".y", ".data", ".env",
                       "desc", "n", "everything", "where", "across", "pi", "letters", "LETTERS",
                       "month.name", "month.abb", "self", "private")
  exprs <- tryCatch(suppressWarnings(parse(file, keep.source = TRUE)), error = function(err) err)
  if (inherits(exprs, "error")) return(syntax_finding(file, exprs))
  src <- attr(exprs, "srcref")
  ctx$functions <- project_functions(file, exprs, ctx)
  ctx$call_stack <- character()
  # Names assigned anywhere in the file count as variables: data masking falls back to the environment.
  for (e in exprs) {
    if (is.call(e) && call_name(e) %in% c("<-", "=", "<<-") && is.symbol(e[[2L]])) {
      ctx$variables <- union(ctx$variables, as.character(e[[2L]]))
    }
  }
  for (i in seq_along(exprs)) {
    ctx$line <- if (!is.null(src)) as.integer(src[[i]][[1L]]) else NA_integer_
    ctx$line_end <- if (!is.null(src)) as.integer(src[[i]][[3L]]) else NA_integer_
    guarded(ctx, NULL, walk_expr(exprs[[i]], ctx))
  }
  structure(c(ctx$findings, source_notes(ctx), error_findings(ctx)),
            stats = list(checked = length(ctx$stats$checked), skipped = length(ctx$stats$skipped)))
}

# A file that does not parse: said in its own finding, on the line the parser
# names, so that an agent that has just broken the file does not read the
# checker's silence as "clean". The parser's message already carries
# file:line:col, the offending line and a caret.
syntax_finding <- function(file, err) {
  msg <- conditionMessage(err)
  m <- regmatches(msg, regexec("^(?:.*?):(\\d+):(\\d+): *([^\n]*)", msg, perl = TRUE))[[1L]]
  line <- if (length(m) >= 2L) as.integer(m[[2L]]) else 1L
  what <- if (length(m) >= 4L) sprintf("%s:%s: %s", m[[2L]], m[[3L]], m[[4L]]) else strsplit(msg, "\n")[[1L]][[1L]]
  rest <- strsplit(msg, "\n")[[1L]][-1L]
  structure(list(list(
    kind = "syntax", file = file, line = line, name = NA_character_, source = NA_character_, columns = character(),
    message = sprintf("this file does not parse (%s), so nothing in it was checked%s", what,
                      if (length(rest)) paste0(":\n", paste(rest, collapse = "\n")) else ""))),
    stats = list(checked = 0L, skipped = 0L, unparsed = 1L))
}

# Notes about data sources: frames the checker could not resolve that are
# then piped into verbs, and cache entries older than 90 days.
source_notes <- function(ctx) {
  notes <- list()
  note <- function(line, name, message) {
    notes[[length(notes) + 1L]] <<- list(kind = "note", file = ctx$file, line = line, name = name,
                                          source = NA_character_, columns = character(), message = message)
  }
  for (nm in names(ctx$unresolved)) {
    u <- ctx$unresolved[[nm]]
    if (!isTRUE(u$used)) next
    if (!is.null(u$package_fn)) {
      learn_expr <- if (isTRUE(u$self_contained)) gsub("'", "'\\\\''", u$expr) else "<an expression that returns its table>"
      note(u$line, nm, sprintf(
        "'%s' comes from %s(), whose output columns could not be read from its code (%s), so column names downstream are not checked. If it returns the same columns every call, cache it once by name: r-colcheck learn \"%s\" '%s' (every call then resolves); if its columns depend on an argument, add `# cols: KEY` on this line and learn KEY instead.",
        nm, u$package_fn, u$expr, u$package_fn, learn_expr))
      next
    }
    keys <- u$keys[!(u$keys %in% names(ctx$schemas))]
    key <- if (length(keys)) keys[[1L]] else if (length(u$keys)) u$keys[[1L]] else nm
    note(u$line, nm, sprintf(
      "'%s' comes from a source the checker cannot open (%s); no cached schema for key \"%s\", so column names downstream are not checked. Learn it once: r-colcheck learn \"%s\" '%s'",
      nm, u$expr, key, key, gsub("'", "'\\\\''", u$expr)))
  }
  # Age is only worth mentioning when it could explain a finding: a column
  # reported against an old entry may be a column the data has since gained.
  cited <- unique(vapply(ctx$findings, function(f) f$source %||% "", ""))
  for (key in names(ctx$stale)) {
    s <- ctx$stale[[key]]
    if (!any(startsWith(cited, sprintf("cached schema \"%s\"", key)))) next
    note(s$line, key, sprintf(
      "the findings above for \"%s\" rely on a cached schema %d days old (observed %s); if the data gained those columns, refresh it: r-colcheck learn \"%s\" '<expr>'",
      key, s$days, s$observed, key))
  }
  notes
}

# Findings for FILES, with a "stats" attribute: pipeline steps checked
# against known columns, steps skipped (columns unknowable), files read.
colcheck_files <- function(files, root = NULL) {
  all <- list()
  stats <- list(checked = 0L, skipped = 0L, files = 0L, errors = 0L, unparsed = 0L)
  for (f in files) {
    if (!file.exists(f) || grepl("[.](Rmd|rmd|qmd)$", f)) next
    res <- tryCatch(colcheck_file(f, root), error = function(err) list(list(
      kind = "error", file = f, line = 1L, name = NA_character_, source = NA_character_, columns = character(),
      message = sprintf("r-colcheck could not check this file at all (%s). This is a checker bug worth reporting.",
                        conditionMessage(err)))))
    st <- attr(res, "stats")
    all <- c(all, res)
    stats$files <- stats$files + 1L
    stats$checked <- stats$checked + (st$checked %||% 0L)
    stats$skipped <- stats$skipped + (st$skipped %||% 0L)
    stats$errors <- stats$errors + sum(vapply(res, function(x) identical(x$kind, "error"), TRUE))
    stats$unparsed <- stats$unparsed + (st$unparsed %||% 0L)
  }
  # The walker can visit a pipeline twice (assignment and its right-hand side).
  keys <- vapply(all, function(x) paste(x$file, x$line, x$name, x$source), "")
  structure(all[!duplicated(keys)], stats = stats)
}

relative_path <- function(file, root) {
  shown <- normalizePath(file, mustWork = FALSE)
  base <- normalizePath(root %||% colcheck_root(file), mustWork = FALSE)
  if (startsWith(shown, paste0(base, "/"))) substring(shown, nchar(base) + 2L) else shown
}

colcheck_format <- function(findings, root = NULL) {
  vapply(findings, function(x) sprintf("%s:%s: %s", relative_path(x$file, root), x$line, x$message), "")
}
