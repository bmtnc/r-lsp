# The schema cache side of r-colcheck. Run by bin/r-colcheck from the project
# root WITHOUT --vanilla, so the project's .Rprofile (renv) applies and the
# expression runs with the project's packages and the shell's AWS credentials.
#
#   r-colcheck learn KEY 'EXPR' [--source FILE.R ...] [--load-all]
#                                                        evaluate EXPR, record its column names under KEY
#   r-colcheck discover                                  every source in the project the checker cannot
#                                                        open, where it is used, and its learn command
#   r-colcheck learn --all [--dry-run]                   learn every source `discover` lists, each with
#                                                        the expression and flags its note gives
#   r-colcheck refresh [--stale] [KEY ...]               re-run the cached expressions (all, the stale
#                                                        ones, or the keys given) with the flags they
#                                                        were learned with; one failure does not stop the rest
#   r-colcheck schemas                                   list cached keys
#   r-colcheck forget KEY                                drop one entry
#
# --source files are sourced for their definitions (loader helpers) before
# EXPR is evaluated; pass files of function definitions, not scripts that run
# a pipeline. --load-all loads the package at the root with pkgload (as
# devtools::load_all() does: its .onLoad runs, compiled code is built) and
# evaluates EXPR inside its namespace, so the package's own functions and
# its imports (`%>%` among them) resolve with no --source and no library(). The cache is <root>/.colcheck/schemas.json; the directory is
# created with a .gitignore so the cache stays local unless that file is
# deleted.

`%||%` <- function(a, b) if (is.null(a)) b else a
# Project library first (renv, through the .Rprofile this script runs with),
# the global libraries after it, so jsonlite is found when renv does not have
# it. renv rewrites R_LIBS_USER and friends for this process (and for any
# child it starts), so the paths a plain R session sees come from the shell
# wrapper, which read them before renv ran (R_LSP_GLOBAL_LIBS); R's own site
# and default libraries and the environment as it stands are added after.
env_libs <- function(v) strsplit(Sys.getenv(v), .Platform$path.sep, fixed = TRUE)[[1L]]
global_libs <- c(env_libs("R_LSP_GLOBAL_LIBS"), env_libs("R_LIBS_USER"), env_libs("R_LIBS_SITE"), env_libs("R_LIBS"),
                 .Library.site, .Library, file.path(R.home(), "site-library"), file.path(R.home(), "library"))
global_libs <- unique(global_libs[nzchar(global_libs) & dir.exists(global_libs)])
if (length(global_libs)) .libPaths(c(.libPaths(), global_libs))
# run by hand without the wrapper: a plain R says where its libraries are
# (with renv's rewritten variables cleared, so it answers for the shell)
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  vanilla <- suppressWarnings(tryCatch(system2("Rscript", c("--vanilla", "-e", shQuote("cat(.libPaths(), sep = .Platform$path.sep)")),
                                               env = c("R_LIBS_USER=", "R_LIBS_SITE=", "R_LIBS=", "RENV_PROJECT="), stdout = TRUE, stderr = FALSE),
                                       error = function(e) character()))
  vanilla <- strsplit(paste(vanilla, collapse = ""), .Platform$path.sep, fixed = TRUE)[[1L]]
  vanilla <- vanilla[nzchar(vanilla) & dir.exists(vanilla)]
  if (length(vanilla)) .libPaths(c(.libPaths(), vanilla))
}
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
source(file.path(script_dir, "colcheck-lib.R"), local = TRUE)

args <- commandArgs(TRUE)
cmd <- if (length(args)) args[[1L]] else ""
args <- args[-1L]
root <- getwd()
path <- schemas_path(root)
rel_path <- file.path(".colcheck", "schemas.json")

# status 1: the work failed (an expression that does not evaluate, a key
# that is not cached); status 2: the arguments are wrong, as for the check.
die <- function(...) { cat("r-colcheck: ", ..., "\n", sep = "", file = stderr()); quit(save = "no", status = 1) }
die_usage <- function(...) { cat("r-colcheck: ", ..., "\n", sep = "", file = stderr()); quit(save = "no", status = 2) }

# The cache as it is. A file that is there but does not parse stops the
# run: rewriting it would lose every schema learned before.
read_cache <- function() {
  if (!file.exists(path)) return(list())
  s <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                error = function(e) die("cannot parse ", rel_path, " (", conditionMessage(e), "); fix or delete it first"))
  if (!is.list(s) || (length(s) && is.null(names(s)))) die(rel_path, " is not an object of entries keyed by name; fix or delete it first")
  s
}

# One cache entry: what learn observed, the flags it was learned with, and
# any other field the entry held (a hand-written note, a field a later
# version writes) kept as it was.
schema_entry <- function(expr, d, load_all = FALSE, sources = character(), keep = NULL) {
  e <- c(list(expr = expr, observed_at = now(), method = d$method, columns = unname(d$columns)),
         if (load_all) list(load_all = TRUE), if (length(sources)) list(sources = sources))
  extra <- if (is.list(keep)) keep[setdiff(names(keep), c(names(e), "load_all", "sources"))] else list()
  c(e, extra)
}

write_cache <- function(s) {
  dir <- dirname(path)
  created <- !dir.exists(dir)
  if (created) {
    dir.create(dir, recursive = TRUE)
    writeLines("*", file.path(dir, ".gitignore"))
  }
  s <- s[order(names(s))]
  # One line per column, so a wide frame stays readable and diffs stay small;
  # every other field of the entry as JSON, so nothing an entry holds is lost.
  str <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null"))
  entries <- vapply(names(s), function(key) {
    e <- s[[key]]
    cols <- vapply(e$columns %||% list(), function(cl) str(cl), "")
    rest <- e[setdiff(names(e), "columns")]
    if (!is.null(rest$sources)) rest$sources <- I(as.character(unlist(rest$sources)))
    fields <- vapply(names(rest), function(k) paste0("    ", str(k), ": ", str(rest[[k]])), "")
    paste0("  ", str(key), ": {\n", paste(c(fields, paste0("    \"columns\": [\n      ", paste(cols, collapse = ",\n      "), "\n    ]")), collapse = ",\n"), "\n  }")
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
    # the schema of what the object yields, computed without reading rows:
    # for a dplyr query over a dataset that is the query's output (its
    # select(), mutate(), rename() applied), where obj$.data$schema would be
    # the dataset it reads. A Schema itself is recorded as is.
    sch <- if (inherits(obj, "Schema")) obj else quiet(arrow::infer_schema(obj)) %||% quiet(obj$schema)
    if (!is.null(sch) && !is.null(quiet(sch$fields))) {
      return(cols(names(sch), vapply(sch$fields, function(f) f$type$ToString(), ""), "arrow_schema"))
    }
    if (inherits(obj, "arrow_dplyr_query")) return(cols(names(obj), rep(NA_character_, length(names(obj))), "arrow_names"))
    # any other arrow object (an array, a type) is not a table: refused below
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
  stop("the expression returned ", paste(class(obj), collapse = "/"),
       ", not a data frame, arrow dataset or lazy table")
}

# The package at the root, loaded with pkgload once per process; its name.
loaded_package <- NULL
load_package <- function() {
  if (!is.null(loaded_package)) return(loaded_package)
  if (!file.exists(file.path(root, "DESCRIPTION"))) stop("--load-all needs a package: no DESCRIPTION at ", root)
  if (!requireNamespace("pkgload", quietly = TRUE)) stop("--load-all needs the pkgload package (install.packages(\"pkgload\"))")
  pkg <- tryCatch(unname(read.dcf(file.path(root, "DESCRIPTION"), fields = "Package")[1L, 1L]),
                  error = function(e) stop("cannot read DESCRIPTION: ", conditionMessage(e)))
  tryCatch(suppressMessages(pkgload::load_all(root, quiet = TRUE, export_all = TRUE)),
           error = function(e) stop("pkgload::load_all(\"", root, "\") failed: ", conditionMessage(e)))
  loaded_package <<- pkg
  pkg
}

# Where a --source file is: as given (absolute, or relative to the project
# root, where bin/r-colcheck runs this script), else relative to where the
# command was typed. NULL when neither exists.
find_source <- function(f) {
  if (file.exists(f)) return(f)
  invoked <- Sys.getenv("R_LSP_INVOKE_DIR", "")
  if (nzchar(invoked) && file.exists(file.path(invoked, f))) return(file.path(invoked, f))
  NULL
}

# The form a --source path is recorded in: relative to the project root when
# the file is inside it (the wrapper hands over an absolute path for a file
# that exists where the command was typed), so a committed cache refreshes
# in another checkout or after the project moves, as `learn --all` records
# its sources; absolute only when the file is outside the root. A path that
# does not exist stays as typed, and observe() reports it.
recorded_source <- function(f) {
  hit <- find_source(f)
  if (is.null(hit)) return(f)
  abs <- normalizePath(hit)
  top <- paste0(normalizePath(root), "/")
  if (startsWith(abs, top)) substring(abs, nchar(top) + 1L) else abs
}

# Evaluate one loader expression as `learn` does: in a fresh environment
# under the package namespace (--load-all) or the global one, with the
# --source files sourced first. Errors carry the stage that failed.
observe <- function(expr, sources = character(), load_all = FALSE) {
  env <- if (load_all) new.env(parent = asNamespace(load_package())) else new.env(parent = globalenv())
  invoked <- Sys.getenv("R_LSP_INVOKE_DIR", "")
  for (f in sources) {
    hit <- find_source(f)
    if (is.null(hit)) stop("--source file not found: ", f, " (looked under the project root ", root,
                           if (nzchar(invoked)) paste0(" and ", invoked) else "", ")")
    tryCatch(sys.source(hit, envir = env), error = function(e) stop("sourcing ", hit, " failed: ", conditionMessage(e)))
  }
  parsed <- tryCatch(parse(text = expr, keep.source = FALSE), error = function(e) stop("cannot parse expression: ", conditionMessage(e)))
  if (!length(parsed)) stop("empty expression")
  obj <- tryCatch(eval(parsed[[length(parsed)]], envir = env),
                  error = function(e) stop("evaluating the expression failed: ", conditionMessage(e)))
  describe(obj)
}

now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
column_names <- function(cols) vapply(cols, function(cl) if (is.list(cl)) as.character(cl$name %||% "") else as.character(cl), "")

if (cmd == "discover") {
  if (length(args)) die_usage("usage: r-colcheck discover")
  cat(discover_format(colcheck_discover(root)), sep = "\n")
  quit(save = "no", status = 0)
}

if (cmd == "learn" && "--all" %in% args) {
  dry <- "--dry-run" %in% args
  if (length(setdiff(args, c("--all", "--dry-run")))) die_usage("usage: r-colcheck learn --all [--dry-run]")
  d <- colcheck_discover(root)
  if (!length(d$sources)) { cat("nothing to learn: no uncached source is used in the project\n"); quit(save = "no", status = 0) }
  if (dry) {
    for (src in d$sources) cat(src$command, "\n", sep = "")
    cat(sprintf("%d source(s) would be learned\n", length(d$sources)))
    quit(save = "no", status = 0)
  }
  s <- read_cache()
  failed <- 0L
  for (src in d$sources) {
    a <- src$learn_args
    srcs <- a[which(a == "--source") + 1L]
    srcs <- srcs[!is.na(srcs)]
    load_all <- "--load-all" %in% a
    r <- tryCatch(observe(src$learn_expr, srcs, load_all), error = function(e) e)
    if (inherits(r, "error")) {
      failed <- failed + 1L
      cat(sprintf("failed   \"%s\": %s\n", src$key, conditionMessage(r)))
      next
    }
    s[[src$key]] <- schema_entry(src$learn_expr, r, load_all, srcs, keep = s[[src$key]])
    cat(sprintf("learned  \"%s\": %d columns via %s\n", src$key, length(r$columns), r$method))
  }
  created <- write_cache(s)
  cat(sprintf("learned %d of %d source(s) -> %s%s\n", length(d$sources) - failed, length(d$sources), rel_path,
              if (created) "\n.colcheck/ is gitignored (its own .gitignore); delete .colcheck/.gitignore to commit the cache." else ""))
  quit(save = "no", status = if (failed) 1L else 0L)
}

if (cmd == "learn") {
  sources <- character()
  positional <- character()
  load_all <- FALSE
  i <- 1L
  while (i <= length(args)) {
    if (args[[i]] == "--source" && i < length(args)) { sources <- c(sources, args[[i + 1L]]); i <- i + 2L; next }
    if (args[[i]] == "--load-all") { load_all <- TRUE; i <- i + 1L; next }
    positional <- c(positional, args[[i]])
    i <- i + 1L
  }
  if (length(positional) != 2L) die_usage("usage: r-colcheck learn KEY 'EXPR' [--source FILE.R ...] [--load-all]")
  key <- positional[[1L]]
  expr <- positional[[2L]]
  # a source inside the project is recorded relative to the root, so the cache travels
  sources <- vapply(sources, recorded_source, "", USE.NAMES = FALSE)
  d <- tryCatch(observe(expr, sources, load_all), error = function(e) die(conditionMessage(e)))
  loaded <- loaded_package
  s <- read_cache()
  # the flags travel with the entry, so `refresh` can run it the same way
  s[[key]] <- schema_entry(expr, d, load_all, sources, keep = s[[key]])
  created <- write_cache(s)
  nested <- vapply(d$columns, function(cl) if (!is.null(cl$inner)) cl$name else NA_character_, "")
  nested <- nested[!is.na(nested)]
  cat(sprintf("learned \"%s\": %d columns via %s -> %s", key, length(d$columns), d$method, rel_path),
      if (length(nested)) sprintf(" (inner columns recorded for list-column %s)", paste(nested, collapse = ", ")) else "",
      if (!is.null(loaded)) sprintf(" (package %s loaded with pkgload)", loaded) else "",
      if (created) "\n.colcheck/ is gitignored (its own .gitignore); delete .colcheck/.gitignore to commit the cache." else "",
      "\n", sep = "")
  quit(save = "no", status = 0)
}

if (cmd == "refresh") {
  stale_only <- "--stale" %in% args
  keys <- setdiff(args, "--stale")
  if (any(startsWith(keys, "-"))) die_usage("usage: r-colcheck refresh [--stale] [KEY ...]")
  s <- read_cache()
  if (!length(s)) die("no cached schemas (", rel_path, " not found or empty)")
  if (length(keys)) {
    missing <- keys[!(keys %in% names(s))]
    if (length(missing)) die("no cached schema for ", paste0("\"", missing, "\"", collapse = ", "))
  } else keys <- names(s)
  if (stale_only) {
    ttl <- ttl_days(load_config(root))
    age <- vapply(keys, function(k) {
      a <- quiet(as.integer(Sys.Date() - as.Date(substr(as.character(s[[k]]$observed_at %||% ""), 1L, 10L))))
      if (is.null(a) || is.na(a)) ttl + 1L else a
    }, 1L)
    keys <- keys[age > ttl]
  }
  if (!length(keys)) { cat("nothing to refresh\n"); quit(save = "no", status = 0) }
  failed <- 0L
  changed <- 0L
  for (key in keys) {
    e <- s[[key]]
    srcs <- as.character(unlist(e$sources))
    d <- tryCatch(observe(e$expr, srcs, isTRUE(e$load_all)), error = function(err) err)
    if (inherits(d, "error")) {
      failed <- failed + 1L
      cat(sprintf("failed    \"%s\": %s%s\n", key, conditionMessage(d),
                  if (is.null(e$load_all) && is.null(e$sources))
                    " (learned before the flags were recorded? learn it again with its --load-all or --source flags)" else ""))
      next
    }
    old <- column_names(e$columns)
    new <- column_names(d$columns)
    added <- setdiff(new, old)
    dropped <- setdiff(old, new)
    s[[key]] <- schema_entry(e$expr, d, isTRUE(e$load_all), srcs, keep = e)
    if (length(added) || length(dropped)) {
      changed <- changed + 1L
      cat(sprintf("changed   \"%s\": %d columns%s%s\n", key, length(new),
                  if (length(added)) paste0(", added ", paste(added, collapse = ", ")) else "",
                  if (length(dropped)) paste0(", dropped ", paste(dropped, collapse = ", ")) else ""))
    } else cat(sprintf("unchanged \"%s\": %d columns\n", key, length(new)))
  }
  write_cache(s)
  cat(sprintf("refreshed %d of %d: %d changed, %d unchanged, %d failed -> %s%s\n", length(keys) - failed, length(keys),
              changed, length(keys) - failed - changed, failed, rel_path,
              if (!is.null(loaded_package)) sprintf(" (package %s loaded with pkgload)", loaded_package) else ""))
  quit(save = "no", status = if (failed) 1L else 0L)
}

if (cmd == "schemas") {
  if (length(args)) die_usage("usage: r-colcheck schemas")
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
  if (length(args) != 1L) die_usage("usage: r-colcheck forget KEY")
  s <- read_cache()
  if (is.null(s[[args[[1L]]]])) die("no cached schema for \"", args[[1L]], "\"")
  s[[args[[1L]]]] <- NULL
  write_cache(s)
  cat("forgot \"", args[[1L]], "\"\n", sep = "")
  quit(save = "no", status = 0)
}

die_usage("unknown command: ", cmd)
