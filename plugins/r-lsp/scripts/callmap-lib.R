# callmap: a static map of which project functions call which.
#
# Claude Code's LSP tool hands out the call graph one node per request
# (incomingCalls/outgoingCalls at a cursor position), so an agent planning a
# refactor greps a name and reads whole files instead. This prints the whole
# map at once from the parse trees, so the agent reads one listing and opens
# only the files it names. Nothing is evaluated.
#
# Definitions: top-level `name <- function(...)` (also `=`, `<<-`, `\(x)`),
# R6 methods (`Class$method`) and, in _targets.R, `tar_target(name, ...)`.
# Nested functions are part of the function they sit in. Edges: direct calls
# (`f(x)`, `x %>% f`, `pkg::f()` when pkg is this package), functions passed
# as values (`lapply(x, f)`, `do.call(f, ...)`, any bare name of a project
# function used as a value) and names passed as strings to do.call()/
# match.fun()/get() and the apply family, plus Recall(). Only edges to
# functions defined in the project are kept: calls into packages are noise in
# a dependency map. Code outside any function (tests, scripts, top-level
# statements) is attributed to its file. R Markdown, Quarto and Sweave files
# are read chunk by chunk (parse_r_source() in colcheck-lib.R) and treated
# as scripts: knitr runs them top to bottom, so a function defined in a
# chunk is private to its document.
#
# Entry points: callmap_build(root) -> map; callmap_format(map),
#               callmap_neighbourhood(map, names, depth), callmap_json(map, ids).
# Helpers (call_name, call_args, quiet, local_names, relative_path, %||%) come
# from colcheck-lib.R, sourced by the caller.

# Same discovery as session-context.R, so the counts agree.
callmap_skip_dirs <- "/(renv|packrat|[.]git|node_modules)/"

callmap_files <- function(root) {
  rfiles <- list.files(root, pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE)
  rfiles <- rfiles[!grepl(callmap_skip_dirs, rfiles)]
  rmd <- list.files(root, pattern = "[.](Rmd|rmd|Rmarkdown|qmd|Rnw|rnw)$", recursive = TRUE, full.names = TRUE)
  rmd <- rmd[!grepl(callmap_skip_dirs, rmd)]
  list(r = rfiles, rmd = rmd)
}

# Base and recommended generics a project commonly writes methods for. A
# method of a generic from another package is recognized through NAMESPACE.
callmap_base_generics <- c(
  "print", "format", "summary", "plot", "toString", "as.character", "as.data.frame", "as.list",
  "as.numeric", "as.double", "as.integer", "as.logical", "as.vector", "as.Date", "as.POSIXct",
  "length", "names", "dim", "c", "[", "[[", "$", "[<-", "[[<-", "$<-", "Ops", "Math", "Summary",
  "mean", "median", "head", "tail", "unique", "rev", "sort", "merge", "split", "subset",
  "transform", "with", "update", "predict", "fitted", "residuals", "coef", "anova", "levels",
  "str", "all.equal", "seq", "rep", "xtfrm", "range", "t", "labels", "duplicated", "anyNA")

# Calls whose string arguments name a function: do.call("f", ...), the apply
# family (which goes through match.fun). purrr is left out on purpose:
# map(x, "name") extracts an element, it does not call anything.
callmap_string_takers <- c(
  "do.call", "match.fun", "get", "get0", "getFunction", "lapply", "sapply", "vapply", "mapply",
  "Map", "Reduce", "Filter", "Find", "Position", "apply", "tapply", "outer", "rapply", "eapply",
  "Negate", "Vectorize")

callmap_pipes <- c("%>%", "%<>%", "%T>%", "%!>%")

# Top-level statements a file of definitions may also carry. Anything else
# (test_that(), print(), a pipeline) makes the file a script: see
# resolve_edges() for why that matters.
callmap_setup_calls <- c("<-", "=", "<<-", "library", "require", "requireNamespace", "loadNamespace",
                         "source", "sys.source", "suppressPackageStartupMessages", "options", "Sys.setenv",
                         "set.seed", "globalVariables", "use", "from", "invisible", "tar_source", "tar_option_set")

# targets factories whose first argument is the target name.
callmap_target_calls <- "^tar_(target|target_raw|file|files|files_input|file_read|age|change|force|format|rds|qs|fst|fst_tbl|fst_dt|parquet|feather|url|aws_.*)$"

# A name as written in NAMESPACE or an argument: symbol, string or pkg::name.
arg_name <- function(x) {
  if (is.symbol(x) || (is.character(x) && length(x) == 1L)) return(as.character(x))
  ns_name(x)
}

namespace_info <- function(root) {
  out <- list(exports = character(), patterns = character(), s3 = list())
  ex <- quiet(parse(file.path(root, "NAMESPACE"), keep.source = FALSE))
  for (e in ex) {
    fn <- call_name(e)
    if (is.na(fn)) next
    a <- vapply(call_args(e), arg_name, "")
    if (fn == "export") out$exports <- c(out$exports, a)
    if (fn == "exportPattern") out$patterns <- c(out$patterns, a)
    if (fn == "S3method" && length(a) >= 2L) {
      method <- if (length(a) >= 3L && !is.na(a[[3L]])) a[[3L]] else paste0(a[[1L]], ".", a[[2L]])
      out$s3[[method]] <- a[[1L]]
    }
  }
  out
}

# Files in reading order: a package's R/ by its Collate field (else
# alphabetical), then tests/, then everything else; C-locale order so the
# listing does not depend on the user's locale.
callmap_order <- function(rel, collate) {
  top <- ifelse(grepl("/", rel), sub("/.*", "", rel), "")
  group <- ifelse(top == "R", 1L, ifelse(top %in% c("tests", "test"), 2L, 3L))
  pos <- match(sub("^R/", "", rel), collate)
  pos[group != 1L] <- NA_integer_
  order(group, is.na(pos), pos, rel, method = "radix")
}

# `name <- function(...)` and friends: list(name, rhs), or NULL.
assignment_parts <- function(e) {
  if (!is.call(e) || length(e) != 3L || !(call_name(e) %in% c("<-", "=", "<<-"))) return(NULL)
  lhs <- e[[2L]]
  name <- if (is.symbol(lhs)) as.character(lhs) else if (is.character(lhs) && length(lhs) == 1L) lhs else return(NULL)
  list(name = name, rhs = e[[3L]])
}

is_function_expr <- function(e) is.call(e) && identical(call_name(e), "function")

has_call <- function(e, names) {
  if (!is.call(e)) return(FALSE)
  if (call_name(e) %in% names) return(TRUE)
  for (p in call_args(e)) if (has_call(p, names)) return(TRUE)
  FALSE
}

fn_args <- function(fn) paste(names(fn[[2L]]) %||% character(), collapse = ", ")
fn_line <- function(fn, fallback) {
  sr <- if (length(fn) >= 4L) fn[[4L]] else NULL
  if (inherits(sr, "srcref")) sr[[1L]] else fallback
}

# Methods of an `R6Class(...)` call: list of list(name, fn).
r6_methods <- function(rhs) {
  if (!is.call(rhs) || !identical(call_name(rhs), "R6Class")) return(NULL)
  out <- list()
  args <- call_args(rhs)
  for (section in intersect(names(args), c("public", "private", "active"))) {
    lst <- args[[section]]
    if (!is.call(lst) || !identical(call_name(lst), "list")) next
    items <- call_args(lst)
    for (nm in names(items)) if (nzchar(nm) && is_function_expr(items[[nm]])) out[[length(out) + 1L]] <- list(name = nm, fn = items[[nm]])
  }
  out
}

# --- pass 1: parse every file and collect definitions -------------------------

new_defs <- function() {
  env <- new.env()
  env$n <- 0L
  env$rows <- list()
  env
}

add_def <- function(defs, name, label, file, line, args, kind, fn = NULL, class = NA_character_) {
  defs$n <- defs$n + 1L
  defs$rows[[defs$n]] <- list(name = name, label = label, file = file, line = as.integer(line), args = args,
                              kind = kind, fn = fn, class = class)
  defs$n
}

scan_file <- function(path, rel, defs) {
  parsed <- tryCatch(parse_r_source(path), error = function(e) e)
  if (inherits(parsed, "error")) {
    msg <- strsplit(conditionMessage(parsed), "\n", fixed = TRUE)[[1L]]
    return(list(rel = rel, error = paste("does not parse:", sub(paste0("^", path, ":"), "", msg[nzchar(msg)][1L] %||% "parse error"))))
  }
  ex <- parsed$exprs
  srcrefs <- attr(ex, "srcref")
  lines <- parsed$lines
  # A chunk that does not parse is named like a file that does not; the
  # other chunks are read.
  chunk_errors <- vapply(parsed$errors, function(e) {
    msg <- strsplit(conditionMessage(e$cond), "\n", fixed = TRUE)[[1L]]
    sprintf("the R chunk starting at line %d does not parse: %s", e$line, sub("^<text>:", "", msg[nzchar(msg)][1L] %||% "parse error"))
  }, "")
  units <- vector("list", length(ex))
  script <- is_literate_file(path)
  locals <- character()
  for (i in seq_along(ex)) {
    line <- srcrefs[[i]][[1L]]
    parts <- assignment_parts(ex[[i]])
    if (is.null(parts) || !is_function_expr(parts$rhs)) {
      if (is.call(ex[[i]]) && !(call_name(ex[[i]]) %in% callmap_setup_calls)) script <- TRUE
    }
    if (is.null(parts)) {
      locals <- local_names(ex[[i]], locals)
      next
    }
    if (is_function_expr(parts$rhs)) {
      units[[i]] <- add_def(defs, parts$name, parts$name, rel, line, fn_args(parts$rhs), "function", parts$rhs)
    } else if (length(methods <- r6_methods(parts$rhs))) {
      units[[i]] <- vapply(methods, function(m) {
        add_def(defs, paste0(parts$name, "$", m$name), paste0(parts$name, "$", m$name), rel,
                fn_line(m$fn, line), fn_args(m$fn), "r6_method", m$fn, parts$name)
      }, 1L)
    } else {
      locals <- local_names(ex[[i]], locals)
    }
  }
  # source("x.R") with a literal path: an edge from this file to that file.
  sources <- unique(unlist(lapply(ex, source_calls)))
  list(rel = rel, path = path, exprs = ex, srcrefs = srcrefs, lines = lines, units = units, sources = sources,
       script = script, locals = unique(locals), chunk_errors = chunk_errors)
}

resolve_source <- function(s, file_rel, root) {
  cands <- c(file.path(root, dirname(file_rel), s), file.path(root, s), s)
  hit <- cands[file.exists(cands)]
  if (!length(hit)) return(s)
  relative_path(hit[[1L]], root)
}

# --- pass 2: walk each unit and collect edges ---------------------------------

# The line of the call: the first line of the statement that names it.
pin_line <- function(w, nm, l1, l2) {
  if (is.na(l2) || l2 <= l1) return(l1)
  pat <- w$patterns[[nm]]
  if (is.null(pat)) {
    pat <- paste0("(^|[^A-Za-z0-9._])", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", nm), "($|[^A-Za-z0-9._])")
    w$patterns[[nm]] <- pat
  }
  hit <- grep(pat, w$lines[l1:l2])
  if (length(hit)) l1 + hit[[1L]] - 1L else l1
}

add_edge <- function(w, to, kind, l1, l2, pin = to) {
  w$n <- w$n + 1L
  w$edges[[w$n]] <- list(from_def = w$owner_def, from = w$owner, file = w$file, to = to, kind = kind,
                         line = pin_line(w, pin, l1, l2))
}

is_project_fn <- function(w, nm) !is.na(nm) && nzchar(nm) && !is.null(w$names[[nm]])

walk_node <- function(e, l1, l2, w) {
  if (is.symbol(e)) {
    nm <- as.character(e)
    if (nzchar(nm) && is_project_fn(w, nm) && !(nm %in% w$locals)) add_edge(w, nm, "value", l1, l2)
    return(invisible())
  }
  if (!is.call(e)) return(invisible())
  head <- e[[1L]]
  fn <- NA_character_
  if (is.symbol(head)) {
    fn <- as.character(head)
    if (is_project_fn(w, fn) && !(fn %in% w$local_fns)) add_edge(w, fn, "direct", l1, l2)
  } else if (is.call(head) && is.symbol(head[[1L]])) {
    hh <- as.character(head[[1L]])
    if (hh %in% c("::", ":::") && length(head) == 3L) {
      fn <- as.character(head[[3L]])
      if (!is.null(w$pkg) && identical(as.character(head[[2L]]), w$pkg) && is_project_fn(w, fn)) add_edge(w, fn, "direct", l1, l2)
    } else if (hh == "$" && length(head) == 3L && !is.na(w$class) && is.symbol(head[[2L]]) &&
               as.character(head[[2L]]) %in% c("self", "private")) {
      target <- paste0(w$class, "$", arg_name(head[[3L]]))
      if (is_project_fn(w, target)) add_edge(w, target, "direct", l1, l2, pin = arg_name(head[[3L]]))
    } else {
      walk_node(head, l1, l2, w)
    }
  } else if (is.call(head)) {
    walk_node(head, l1, l2, w)
  }
  if (is.na(fn)) {
    for (p in call_args(e)) walk_node(p, l1, l2, w)
    return(invisible())
  }
  if (fn == "function") {
    sr <- if (length(e) >= 4L) e[[4L]] else NULL
    if (inherits(sr, "srcref")) { l1 <- sr[[1L]]; l2 <- sr[[3L]] }
    fm <- as.list(e[[2L]])
    for (k in seq_along(fm)) if (!identical(fm[[k]], quote(expr = ))) walk_node(fm[[k]], l1, l2, w)
    walk_node(e[[3L]], l1, l2, w)
    return(invisible())
  }
  if (fn == "{") {
    srs <- attr(e, "srcref")
    for (k in seq_along(e)[-1L]) {
      sr <- if (length(srs) >= k) srs[[k]] else NULL
      if (inherits(sr, "srcref")) walk_node(e[[k]], sr[[1L]], sr[[3L]], w) else walk_node(e[[k]], l1, l2, w)
    }
    return(invisible())
  }
  if (fn %in% c("$", "@") && length(e) == 3L) {
    walk_node(e[[2L]], l1, l2, w)
    return(invisible())
  }
  if (fn %in% c("<-", "=", "<<-", "->", "->>") && length(e) == 3L) {
    # The target of `x$f <- v` or `names(x) <- v`: through walk_node, so `$`
    # keeps its field name out of the edges.
    if (is.call(e[[2L]])) walk_node(e[[2L]], l1, l2, w)
    walk_node(e[[3L]], l1, l2, w)
    return(invisible())
  }
  if (fn %in% callmap_pipes && length(e) == 3L && is.symbol(e[[3L]])) {
    walk_node(e[[2L]], l1, l2, w)
    nm <- as.character(e[[3L]])
    if (is_project_fn(w, nm) && !(nm %in% w$local_fns)) add_edge(w, nm, "direct", l1, l2)
    return(invisible())
  }
  if (fn == "Recall" && !is.na(w$owner_def)) add_edge(w, w$owner, "string", l1, l2, pin = "Recall")
  if (fn %in% callmap_string_takers) {
    for (p in call_args(e)) {
      if (is.character(p) && length(p) == 1L && is_project_fn(w, p)) add_edge(w, p, "string", l1, l2)
    }
  }
  if (w$targets && grepl(callmap_target_calls, fn) && length(e) >= 3L && !is.na(arg_name(e[[2L]]))) {
    walk_target(e, fn, l1, l2, w)
    return(invisible())
  }
  for (p in call_args(e)) walk_node(p, l1, l2, w)
  invisible()
}

# tar_target(name, command): a pseudo-definition `target name` that owns the
# calls in its command.
walk_target <- function(e, fn, l1, l2, w) {
  name <- arg_name(e[[2L]])
  line <- pin_line(w, name, l1, l2)
  id <- add_def(w$defs, name, paste("target", name), w$file, line, "", "target")
  saved <- list(w$owner, w$owner_def)
  w$owner <- paste("target", name)
  w$owner_def <- id
  args <- call_args(e)
  for (p in args[-1L]) walk_node(p, l1, l2, w)
  w$owner <- saved[[1L]]
  w$owner_def <- saved[[2L]]
}

walk_file <- function(f, w) {
  w$file <- f$rel
  w$lines <- f$lines
  w$targets <- basename(f$rel) == "_targets.R"
  for (i in seq_along(f$exprs)) {
    sr <- f$srcrefs[[i]]
    ids <- f$units[[i]]
    if (is.null(ids)) {
      w$owner <- f$rel; w$owner_def <- NA_integer_; w$class <- NA_character_
      # Variables of top-level code (`a <- result[...]` in a test) are not
      # the project function of the same name either.
      w$locals <- f$locals; w$local_fns <- character()
      walk_node(f$exprs[[i]], sr[[1L]], sr[[3L]], w)
      next
    }
    for (id in ids) {
      d <- w$defs$rows[[id]]
      w$owner <- d$label; w$owner_def <- id; w$class <- d$class
      # A parameter or local variable with the name of a project function is
      # not that function: `wrap <- function(x, f) lapply(x, f)`.
      formals_nm <- names(d$fn[[2L]]) %||% character()
      w$locals <- c(formals_nm, local_names(d$fn[[3L]]))
      w$local_fns <- c(formals_nm, local_function_names(d$fn[[3L]]))
      walk_node(d$fn, sr[[1L]], sr[[3L]], w)
    }
  }
}

# Names assigned a function inside a body: calls to them are not project calls.
local_function_names <- function(e, acc = character()) {
  if (!is.call(e)) return(acc)
  parts <- assignment_parts(e)
  if (!is.null(parts) && is_function_expr(parts$rhs)) acc <- c(acc, parts$name)
  for (p in call_args(e)) acc <- local_function_names(p, acc)
  acc
}

# --- the map --------------------------------------------------------------------

callmap_build <- function(root) {
  root <- normalizePath(root, mustWork = FALSE)
  found <- callmap_files(root)
  desc <- file.path(root, "DESCRIPTION")
  is_pkg <- file.exists(desc)
  dcf <- if (is_pkg) quiet(read.dcf(desc, fields = c("Package", "Collate"))) else NULL
  pkg <- if (!is.null(dcf) && !is.na(dcf[1L, "Package"])) unname(dcf[1L, "Package"]) else NULL
  collate <- if (!is.null(dcf) && !is.na(dcf[1L, "Collate"])) scan(text = dcf[1L, "Collate"], what = "", quiet = TRUE) else character()
  ns <- if (is_pkg) namespace_info(root) else list(exports = character(), patterns = character(), s3 = list())

  paths <- c(found$r, found$rmd)
  rel <- vapply(paths, relative_path, "", root = root, USE.NAMES = FALSE)
  ord <- callmap_order(rel, collate)
  defs <- new_defs()
  files <- list()
  skipped <- list()
  for (k in ord) {
    f <- scan_file(paths[[k]], rel[[k]], defs)
    if (!is.null(f$error)) skipped[[length(skipped) + 1L]] <- f else files[[length(files) + 1L]] <- f
    for (ce in f$chunk_errors) skipped[[length(skipped) + 1L]] <- list(rel = rel[[k]], error = ce)
  }

  w <- new.env()
  w$defs <- defs; w$pkg <- pkg; w$n <- 0L; w$edges <- list(); w$patterns <- new.env()
  w$names <- new.env()
  for (d in defs$rows) if (d$kind != "target") assign(d$name, TRUE, envir = w$names)
  # A walker failure costs that file's edges, never the whole map.
  for (k in seq_along(files)) {
    err <- tryCatch({ walk_file(files[[k]], w); NULL }, error = function(e) conditionMessage(e))
    if (!is.null(err)) skipped[[length(skipped) + 1L]] <- list(rel = files[[k]]$rel, error = paste("edges not read:", err))
  }

  map <- list(root = root, pkg = pkg, is_pkg = is_pkg, files = files, skipped = skipped,
              rmd = vapply(found$rmd, relative_path, "", root = root, USE.NAMES = FALSE),
              n_files = length(paths))
  map$defs <- finish_defs(defs$rows, ns)
  map$edges <- resolve_edges(w$edges, map$defs, files, root)
  map
}

# Exported flags, S3 roles and a table of definitions.
finish_defs <- function(rows, ns) {
  name <- vapply(rows, `[[`, "", "name")
  kind <- vapply(rows, `[[`, "", "kind")
  generic <- vapply(rows, function(d) d$kind == "function" && has_call(d$fn[[3L]], "UseMethod"), TRUE)
  generics <- unique(c(name[generic], callmap_base_generics, unlist(ns$s3, use.names = FALSE)))
  s3_of <- vapply(seq_along(rows), function(i) {
    if (kind[[i]] != "function") return(NA_character_)
    nm <- name[[i]]
    if (!is.null(ns$s3[[nm]])) return(ns$s3[[nm]])
    dots <- gregexpr(".", nm, fixed = TRUE)[[1L]]
    for (p in rev(dots[dots > 1L & dots < nchar(nm)])) {
      prefix <- substr(nm, 1L, p - 1L)
      if (prefix %in% generics && prefix != nm) return(prefix)
    }
    NA_character_
  }, "")
  kind[!is.na(s3_of)] <- "s3_method"
  exported <- name %in% ns$exports
  for (p in ns$patterns) exported <- exported | (kind %in% c("function", "s3_method") & grepl(p, name))
  exported[kind %in% c("r6_method", "target")] <- FALSE
  list(name = name, label = vapply(rows, `[[`, "", "label"), file = vapply(rows, `[[`, "", "file"),
       line = vapply(rows, `[[`, 1L, "line"), args = vapply(rows, `[[`, "", "args"), kind = kind,
       exported = exported, generic = generic, s3_of = s3_of)
}

# Edge names to definition ids. Which definitions a file can see: those in
# R/ and in files that only define things (they are sourced or loaded as a
# whole), plus those in the file itself and in the files it source()s,
# transitively. A function defined inside a script or a test file is private
# to it: a `make_data()` helper in one test file is not called by another
# test file that defines its own. Among the visible definitions of a name,
# the one in the same file wins, then a source()d one, then R/.
resolve_edges <- function(edges, defs, files, root) {
  empty <- data.frame(from_def = integer(), from = character(), file = character(), to_def = integer(),
                      kind = character(), line = integer(), stringsAsFactors = FALSE)
  if (!length(edges)) return(empty)
  sources <- list()
  for (f in files) sources[[f$rel]] <- unname(vapply(f$sources, resolve_source, "", file_rel = f$rel, root = root))
  closure <- function(rel) {
    seen <- character()
    todo <- sources[[rel]] %||% character()
    while (length(todo)) {
      nxt <- todo[[1L]]; todo <- todo[-1L]
      if (nxt %in% seen) next
      seen <- c(seen, nxt)
      todo <- c(todo, sources[[nxt]] %||% character())
    }
    seen
  }
  edge_files <- unique(vapply(edges, function(x) x$file, ""))
  closures <- lapply(stats::setNames(edge_files, edge_files), closure)
  top_level_callers <- unique(unlist(lapply(edges, function(x) if (is.na(x$from_def)) x$file)))
  script <- vapply(files, function(f) f$script || f$rel %in% top_level_callers, TRUE)
  names(script) <- vapply(files, function(f) f$rel, "")
  shared <- startsWith(defs$file, "R/") | !(script[defs$file] %in% TRUE)
  by_name <- split(seq_along(defs$label), defs$label)
  rows <- lapply(edges, function(x) {
    cand <- by_name[[x$to]]
    near <- defs$file[cand] == x$file | defs$file[cand] %in% closures[[x$file]]
    cand <- cand[shared[cand] | near]
    if (length(cand) > 1L) {
      tiers <- list(cand[defs$file[cand] == x$file], cand[defs$file[cand] %in% closures[[x$file]]],
                    cand[startsWith(defs$file[cand], "R/")])
      for (t in tiers) if (length(t)) { cand <- t; break }
    }
    if (!length(cand)) return(NULL)
    data.frame(from_def = x$from_def, from = x$from, file = x$file, to_def = cand, kind = x$kind,
               line = x$line, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(empty), rows))
  out[!duplicated(out), , drop = FALSE]
}

# --- text output ----------------------------------------------------------------

callmap_limits <- paste(
  "Static: misses calls built from strings at runtime (eval(parse()), names pasted together),",
  "S4 dispatch, R6 calls outside their class (obj$method()), inline `r` code in Rmd/qmd prose",
  "(chunks are read), and calls into packages (only project functions are listed).")

callmap_header <- function(map, what = NULL) {
  d <- map$defs
  nfun <- sum(d$kind != "target")
  ntar <- sum(d$kind == "target")
  lines <- sprintf("r-callmap: static call map of %s (%d functions%s in %d files%s)%s.",
                   map$root, nfun, if (ntar) sprintf(" and %d targets", ntar) else "", map$n_files,
                   if (length(map$rmd)) sprintf(", %d of them Rmd/qmd", length(map$rmd)) else "",
                   if (is.null(what)) "" else paste0(", ", what))
  lines <- c(lines, callmap_limits)
  for (s in map$skipped) lines <- c(lines, sprintf("Skipped: %s: %s", s$rel, s$error))
  lines
}

kind_tag <- function(kind) ifelse(kind == "value", " (as value)", ifelse(kind == "string", " (by string)", ""))

def_tags <- function(map, i) {
  d <- map$defs
  tags <- character()
  if (d$exported[[i]]) tags <- c(tags, "[exported]")
  if (!is.na(d$s3_of[[i]])) tags <- c(tags, sprintf("[S3 method of %s]", d$s3_of[[i]]))
  if (d$generic[[i]]) tags <- c(tags, "[S3 generic]")
  dup <- which(d$label == d$label[[i]] & seq_along(d$label) != i)
  if (length(dup)) tags <- c(tags, sprintf("[also defined at %s]", paste(sprintf("%s:%d", d$file[dup], d$line[dup]), collapse = ", ")))
  paste(tags, collapse = " ")
}

def_signature <- function(map, i) {
  d <- map$defs
  if (d$kind[[i]] == "target") d$label[[i]] else sprintf("%s(%s)", d$label[[i]], d$args[[i]])
}

def_loc <- function(map, i) sprintf("%s:%d", map$defs$file[[i]], map$defs$line[[i]])

# "a, b (as value), scripts/run.R (3)": one entry per caller and kind.
caller_list <- function(map, rows) {
  e <- map$edges[rows, , drop = FALSE]
  if (!nrow(e)) return("")
  key <- paste(e$from, e$kind)
  first <- !duplicated(key)
  counts <- table(key)[key[first]]
  is_file <- is.na(e$from_def[first])
  ord <- order(is_file, ifelse(is_file, Inf, e$from_def[first]), e$from[first], method = "radix")
  shown <- paste0(e$from[first], kind_tag(e$kind[first]), ifelse(counts > 1L, sprintf(" (%d)", counts), ""))
  paste(shown[ord], collapse = ", ")
}

callee_list <- function(map, rows) {
  e <- map$edges[rows, , drop = FALSE]
  if (!nrow(e)) return("")
  shown <- paste0(map$defs$label[e$to_def], kind_tag(e$kind))
  paste(unique(shown), collapse = ", ")
}

callmap_format <- function(map) {
  d <- map$defs
  e <- map$edges
  out <- c(callmap_header(map), "")
  for (f in map$files) {
    ids <- which(d$file == f$rel)
    top <- which(e$file == f$rel & is.na(e$from_def))
    if (!length(ids) && !length(top) && !length(f$sources)) next
    heading <- f$rel
    if (length(f$sources)) heading <- paste0(heading, "   sources: ", paste(f$sources, collapse = ", "))
    if (length(top)) heading <- paste0(heading, "   top-level code calls: ", callee_list(map, top))
    out <- c(out, heading)
    locs <- vapply(ids, function(i) def_loc(map, i), "")
    sigs <- vapply(ids, function(i) def_signature(map, i), "")
    wl <- max(nchar(locs), 0L)
    ws <- min(max(nchar(sigs), 0L), 40L)
    for (j in seq_along(ids)) {
      i <- ids[[j]]
      calls <- callee_list(map, which(e$from_def == i))
      by <- caller_list(map, which(e$to_def == i))
      parts <- c(formatC(locs[[j]], width = -wl), formatC(sigs[[j]], width = -ws), def_tags(map, i),
                 if (nzchar(calls)) paste("calls:", calls), if (nzchar(by)) paste("called by:", by))
      out <- c(out, sub(" +$", "", paste0("  ", paste(parts[nzchar(parts)], collapse = "   "))))
    }
    out <- c(out, "")
  }
  c(out, summary_sections(map))
}

summary_sections <- function(map) {
  d <- map$defs
  e <- map$edges
  plain <- which(d$kind == "function")
  out <- character()
  fn_callers <- unique(e$to_def[!is.na(e$from_def) & e$from_def != e$to_def])
  entry <- setdiff(plain, fn_callers)
  if (length(entry)) {
    out <- c(out, "Entry points (not called by any project function):")
    for (i in entry) {
      rows <- which(e$to_def == i & (is.na(e$from_def) | e$from_def != i))
      by <- caller_list(map, rows)
      tags <- def_tags(map, i)
      out <- c(out, paste0("  ", d$label[[i]], if (nzchar(tags)) paste0(" ", tags), "   ", def_loc(map, i), "   ",
                           if (nzchar(by)) paste("called from:", by) else "no callers"))
    }
    out <- c(out, "")
  }
  unref <- setdiff(plain, e$to_def[is.na(e$from_def) | e$from_def != e$to_def])
  if (length(unref)) {
    out <- c(out, "Unreferenced (no callers anywhere):",
             strwrap(paste(sprintf("%s%s", d$label[unref], ifelse(d$exported[unref], " [exported]", "")), collapse = ", "),
                     width = 100, indent = 2, exdent = 2), "")
  }
  if (any(d$kind %in% c("s3_method", "r6_method"))) {
    out <- c(out, "S3 and R6 methods are left out of these two lists: dispatch reaches them, not a call by name.", "")
  }
  cyc <- callmap_cycles(map)
  if (length(cyc)) out <- c(out, "Recursion/cycles:", paste0("  ", cyc), "")
  out
}

# Strongly connected components (Tarjan) of the function graph: each
# component of more than one function, and each function that calls itself.
callmap_cycles <- function(map) {
  e <- map$edges[!is.na(map$edges$from_def), , drop = FALSE]
  n <- length(map$defs$label)
  if (!nrow(e)) return(character())
  adj <- split(e$to_def, factor(e$from_def, levels = seq_len(n)))
  st <- new.env()
  st$index <- 0L; st$stack <- integer(); st$idx <- rep(NA_integer_, n); st$low <- integer(n)
  st$on <- logical(n); st$out <- list()
  strong <- function(v) {
    st$index <- st$index + 1L
    st$idx[v] <- st$index; st$low[v] <- st$index
    st$stack <- c(st$stack, v); st$on[v] <- TRUE
    for (u in unique(adj[[v]])) {
      if (is.na(st$idx[u])) { strong(u); st$low[v] <- min(st$low[v], st$low[u]) }
      else if (st$on[u]) st$low[v] <- min(st$low[v], st$idx[u])
    }
    if (st$low[v] == st$idx[v]) {
      comp <- integer()
      repeat {
        u <- st$stack[length(st$stack)]
        st$stack <- st$stack[-length(st$stack)]; st$on[u] <- FALSE
        comp <- c(comp, u)
        if (u == v) break
      }
      st$out[[length(st$out) + 1L]] <- rev(comp)
    }
  }
  for (v in seq_len(n)) if (is.na(st$idx[v])) strong(v)
  lab <- map$defs$label
  out <- character()
  for (comp in st$out) {
    if (length(comp) > 1L) out <- c(out, paste(paste(lab[comp], collapse = ", "), "call each other"))
    else if (comp %in% adj[[comp]]) out <- c(out, paste(lab[comp], "calls itself"))
  }
  out
}

# --- neighbourhood ------------------------------------------------------------

callmap_lookup <- function(map, name) {
  name <- sub("^[A-Za-z0-9.]+:::?", "", name)
  which(map$defs$label == name | (map$defs$kind == "target" & map$defs$name == name))
}

callmap_nearest <- function(map, name) {
  labels <- unique(map$defs$label)
  hit <- labels[grepl(tolower(name), tolower(labels), fixed = TRUE)]
  if (!length(hit)) hit <- labels[agrepl(name, labels, ignore.case = TRUE, max.distance = 0.2)]
  head(sort(hit, method = "radix"), 10L)
}

site_text <- function(map, rows) {
  e <- map$edges[rows, , drop = FALSE]
  by_file <- split(e$line, e$file)
  paste(vapply(names(by_file), function(f) sprintf("%s:%s", f, paste(sort(unique(by_file[[f]])), collapse = ", ")), ""),
        collapse = "; ")
}

callmap_neighbourhood <- function(map, names, depth = 2L) {
  d <- map$defs
  e <- map$edges
  out <- callmap_header(map, sprintf("around %s, depth %d", paste(names, collapse = ", "), depth))
  seen <- integer()
  for (name in names) {
    ids <- callmap_lookup(map, name)
    out <- c(out, "")
    if (!length(ids)) {
      near <- callmap_nearest(map, name)
      out <- c(out, sprintf("%s: not defined in the project.%s", name,
                            if (length(near)) paste0(" Nearest: ", paste(near, collapse = ", ")) else " No similar names."))
      next
    }
    for (i in ids) {
      seen <- c(seen, i)
      tags <- def_tags(map, i)
      out <- c(out, paste0(def_signature(map, i), "   ", def_loc(map, i), if (nzchar(tags)) paste0("   ", tags)))
      up <- character()
      expand_up <- function(v, level, path) {
        rows <- which(e$to_def == v & (is.na(e$from_def) | e$from_def != v))
        callers <- unique(e$from[rows])
        for (cl in callers) {
          r <- rows[e$from[rows] == cl]
          src <- e$from_def[r[[1L]]]
          kinds <- unique(e$kind[r])
          pad <- strrep("  ", level + 1L)
          if (is.na(src)) {
            up <<- c(up, sprintf("%s%s%s   (top level)", pad, site_text(map, r), paste(kind_tag(kinds), collapse = "")))
            next
          }
          seen <<- c(seen, src)
          cyc <- src %in% path
          up <<- c(up, sprintf("%s%s%s   %s   (call at %s)%s", pad, cl, paste(kind_tag(kinds), collapse = ""),
                               def_loc(map, src), site_text(map, r), if (cyc) "   [cycle, not expanded]" else ""))
          if (!cyc && level < depth) expand_up(src, level + 1L, c(path, src))
        }
        # A generic's callers reach its methods through dispatch.
        if (!is.na(d$s3_of[[v]])) {
          gen <- which(d$label == d$s3_of[[v]] & d$generic)
          pad <- strrep("  ", level + 1L)
          if (length(gen)) for (g in gen) {
            seen <<- c(seen, g)
            up <<- c(up, sprintf("%s%s (dispatch)   %s", pad, d$label[[g]], def_loc(map, g)))
            if (level < depth && !(g %in% path)) expand_up(g, level + 1L, c(path, g))
          } else {
            up <<- c(up, sprintf("%s(also reached through %s() dispatch; calls to %s() are not tracked)", pad, d$s3_of[[v]], d$s3_of[[v]]))
          }
        }
      }
      expand_up(i, 1L, i)
      out <- c(out, "  called by:", if (length(up)) up else "    (none in the project)")
      down <- character()
      expand_down <- function(v, level, path) {
        rows <- which(e$from_def == v & e$to_def != v)
        for (t in unique(e$to_def[rows])) {
          r <- rows[e$to_def[rows] == t]
          seen <<- c(seen, t)
          cyc <- t %in% path
          down <<- c(down, sprintf("%s%s%s   %s   (call at %s)%s", strrep("  ", level + 1L), d$label[[t]],
                                   paste(kind_tag(unique(e$kind[r])), collapse = ""), def_loc(map, t), site_text(map, r),
                                   if (cyc) "   [cycle, not expanded]" else ""))
          if (!cyc && level < depth) expand_down(t, level + 1L, c(path, t))
        }
      }
      expand_down(i, 1L, i)
      if (any(e$from_def == i & e$to_def == i, na.rm = TRUE)) down <- c(down, "    (itself: recursive)")
      out <- c(out, "  calls:", if (length(down)) down else "    (no project functions)")
      if (d$generic[[i]]) {
        m <- which(d$s3_of == d$label[[i]])
        if (length(m)) out <- c(out, paste0("  dispatches to: ", paste(sprintf("%s (%s)", d$label[m], vapply(m, function(k) def_loc(map, k), "")), collapse = ", ")))
      }
    }
  }
  structure(out, ids = unique(seen))
}

# --- JSON -----------------------------------------------------------------------

callmap_json <- function(map, ids = seq_along(map$defs$label)) {
  d <- map$defs
  e <- map$edges
  # args is the display string so a one-argument function is not unboxed to a
  # scalar while others are arrays; empty lists stay [] (list(), not NULL).
  lapply(ids, function(i) {
    out_rows <- which(e$from_def == i)
    in_rows <- which(e$to_def == i)
    calls <- lapply(out_rows, function(r) list(name = d$label[[e$to_def[[r]]]], kind = e$kind[[r]], line = e$line[[r]]))
    calls <- calls[!duplicated(vapply(calls, function(x) paste(x$name, x$kind, x$line), ""))]
    out <- list(name = d$label[[i]], file = d$file[[i]], line = d$line[[i]], args = d$args[[i]],
                exported = d$exported[[i]], kind = d$kind[[i]], calls = calls,
                called_by = lapply(in_rows, function(r) list(by = e$from[[r]], kind = e$kind[[r]], file = e$file[[r]], line = e$line[[r]])))
    if (!is.na(d$s3_of[[i]])) out$s3_generic <- d$s3_of[[i]]
    out
  })
}
