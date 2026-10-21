# The schema cache side of r-colcheck. Run by bin/r-colcheck from the project
# root WITHOUT --vanilla, so the project's .Rprofile (renv) applies and the
# expression runs with the project's packages and the shell's AWS credentials.
#
#   r-colcheck learn KEY 'EXPR' [--source FILE.R ...]   evaluate EXPR, record its column names under KEY
#   r-colcheck schemas                                   list cached keys
#   r-colcheck forget KEY                                drop one entry
#
# --source files are sourced for their definitions (loader helpers) before
# EXPR is evaluated; pass files of function definitions, not scripts that run
# a pipeline. The cache is <root>/.colcheck/schemas.json; the directory is
# created with a .gitignore so the cache stays local unless that file is
# deleted.

`%||%` <- function(a, b) if (is.null(a)) b else a
# Project library first (renv), global libraries after it (bin/r-colcheck
# passes them), so jsonlite is found when renv does not have it.
global_libs <- strsplit(Sys.getenv("R_LSP_GLOBAL_LIBS"), ":", fixed = TRUE)[[1L]]
global_libs <- global_libs[nzchar(global_libs) & dir.exists(global_libs)]
if (length(global_libs)) .libPaths(c(.libPaths(), global_libs))
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
source(file.path(script_dir, "colcheck-lib.R"), local = TRUE)

args <- commandArgs(TRUE)
cmd <- if (length(args)) args[[1L]] else ""
args <- args[-1L]
root <- getwd()
path <- schemas_path(root)
rel_path <- file.path(".colcheck", "schemas.json")

die <- function(...) { cat("r-colcheck: ", ..., "\n", sep = "", file = stderr()); quit(save = "no", status = 1) }

read_cache <- function() {
  s <- load_schemas(root)
  if (!is.list(s)) list() else s
}

write_cache <- function(s) {
  dir <- dirname(path)
  created <- !dir.exists(dir)
  if (created) {
    dir.create(dir, recursive = TRUE)
    writeLines("*", file.path(dir, ".gitignore"))
  }
  s <- s[order(names(s))]
  # One line per column, so a wide frame stays readable and diffs stay small.
  str <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null"))
  entries <- vapply(names(s), function(key) {
    e <- s[[key]]
    cols <- vapply(e$columns, function(cl) str(cl), "")
    paste0("  ", str(key), ": {\n",
           "    \"expr\": ", str(e$expr), ",\n",
           "    \"observed_at\": ", str(e$observed_at), ",\n",
           "    \"method\": ", str(e$method), ",\n",
           "    \"columns\": [\n      ", paste(cols, collapse = ",\n      "), "\n    ]\n  }")
  }, "")
  writeLines(paste0("{\n", paste(entries, collapse = ",\n"), "\n}"), path)
  created
}

# Column names and types of whatever a loader returned, reading no rows when
# the object exposes a schema.
describe <- function(obj) {
  cols <- function(names, types, method, inner = NULL) {
    types <- as.character(types)
    if (is.null(inner)) inner <- vector("list", length(names))
    list(method = method, columns = Map(function(n, t, i) c(list(name = n, type = if (is.na(t)) NULL else t),
                                                             if (!is.null(i)) list(inner = i)), names, types, inner))
  }
  # a list-column of data frames: the columns of its first non-empty element
  # are recorded under it, so unnest() can be followed; nested data is one
  # shape per row in nearly all real tables, so one sample is enough
  inner_columns <- function(x) {
    if (!is.list(x) || is.data.frame(x)) return(NULL)
    el <- Filter(function(v) !is.null(v) && is.data.frame(v) && length(v), x)
    if (!length(el)) return(NULL)
    unname(Map(function(n, t) list(name = n, type = t), names(el[[1L]]), vapply(el[[1L]], function(v) class(v)[[1L]], "")))
  }
  if (inherits(obj, "ArrowObject") || inherits(obj, "arrow_dplyr_query")) {
    sch <- quiet(obj$schema)
    if (is.null(sch)) sch <- quiet(obj$.data$schema)
    if (!is.null(sch)) {
      return(cols(names(sch), vapply(sch$fields, function(f) f$type$ToString(), ""), "arrow_schema"))
    }
    return(cols(names(obj), rep(NA_character_, length(names(obj))), "arrow_names"))
  }
  if (is.data.frame(obj)) {
    return(cols(names(obj), vapply(obj, function(x) class(x)[[1L]], ""), "data_frame", lapply(obj, inner_columns)))
  }
  if (inherits(obj, "tbl_lazy")) {
    nm <- colnames(obj)
    return(cols(nm, rep(NA_character_, length(nm)), "lazy_table"))
  }
  nm <- quiet(colnames(obj))
  if (is.character(nm) && length(nm)) return(cols(nm, rep(NA_character_, length(nm)), "colnames"))
  die("the expression returned ", paste(class(obj), collapse = "/"),
      ", not a data frame, arrow dataset or lazy table")
}

if (cmd == "learn") {
  sources <- character()
  positional <- character()
  i <- 1L
  while (i <= length(args)) {
    if (args[[i]] == "--source" && i < length(args)) { sources <- c(sources, args[[i + 1L]]); i <- i + 2L; next }
    positional <- c(positional, args[[i]])
    i <- i + 1L
  }
  if (length(positional) != 2L) die("usage: r-colcheck learn KEY 'EXPR' [--source FILE.R ...]")
  key <- positional[[1L]]
  expr <- positional[[2L]]
  env <- new.env(parent = globalenv())
  for (f in sources) {
    if (!file.exists(f)) die("--source file not found: ", f)
    tryCatch(sys.source(f, envir = env), error = function(e) die("sourcing ", f, " failed: ", conditionMessage(e)))
  }
  parsed <- tryCatch(parse(text = expr, keep.source = FALSE), error = function(e) die("cannot parse expression: ", conditionMessage(e)))
  if (!length(parsed)) die("empty expression")
  obj <- tryCatch(eval(parsed[[length(parsed)]], envir = env),
                  error = function(e) die("evaluating the expression failed: ", conditionMessage(e)))
  d <- describe(obj)
  s <- read_cache()
  s[[key]] <- list(expr = expr, observed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                   method = d$method, columns = unname(d$columns))
  created <- write_cache(s)
  nested <- vapply(d$columns, function(cl) if (!is.null(cl$inner)) cl$name else NA_character_, "")
  nested <- nested[!is.na(nested)]
  cat(sprintf("learned \"%s\": %d columns via %s -> %s", key, length(d$columns), d$method, rel_path),
      if (length(nested)) sprintf(" (inner columns recorded for list-column %s)", paste(nested, collapse = ", ")) else "",
      if (created) "\n.colcheck/ is gitignored (its own .gitignore); delete .colcheck/.gitignore to commit the cache." else "",
      "\n", sep = "")
  quit(save = "no", status = 0)
}

if (cmd == "schemas") {
  s <- read_cache()
  if (!length(s)) { cat("no cached schemas (", rel_path, " not found or empty)\n", sep = ""); quit(save = "no", status = 0) }
  for (key in names(s)) {
    e <- s[[key]]
    observed <- substr(as.character(e$observed_at %||% ""), 1L, 10L)
    age <- quiet(as.integer(Sys.Date() - as.Date(observed)))
    n <- length(e$columns)
    cat(sprintf("%s  %d columns  observed %s%s  via %s\n", key, n, observed,
                if (!is.null(age) && !is.na(age)) sprintf(" (%d days ago)", age) else "", e$method %||% "?"))
  }
  quit(save = "no", status = 0)
}

if (cmd == "forget") {
  if (length(args) != 1L) die("usage: r-colcheck forget KEY")
  s <- read_cache()
  if (is.null(s[[args[[1L]]]])) die("no cached schema for \"", args[[1L]], "\"")
  s[[args[[1L]]]] <- NULL
  write_cache(s)
  cat("forgot \"", args[[1L]], "\"\n", sep = "")
  quit(save = "no", status = 0)
}

die("unknown command: ", cmd)
