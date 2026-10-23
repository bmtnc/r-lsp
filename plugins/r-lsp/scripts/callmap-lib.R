# callmap: a static map of which project functions call which.
#
# Claude Code's LSP tool hands out the call graph one node per request
# (incomingCalls/outgoingCalls at a cursor position), so an agent planning a
# refactor greps a name and reads whole files instead. This prints the whole
# map at once from the parse trees, so the agent reads one listing and opens
# only the files it names. Nothing is evaluated.
#
# Definitions: top-level `name <- function(...)` (also `=`, `<<-`, `\(x)`,
# and a function literal handed to memoise()/Vectorize()/purrr::safely() and
# the like), R6 classes (`Class`) and their methods (`Class$method`) and, in
# _targets.R, `tar_target(name, ...)`. Nested functions are part of the
# function they sit in. Edges: direct calls (`f(x)`, `x %>% f`, `pkg::f()`
# when pkg is this package), functions passed as values (`lapply(x, f)`,
# `do.call(f, ...)`, any bare name of a project function used as a value),
# names passed as strings to do.call()/match.fun()/get() and the apply
# family (a literal, a local assigned one literal, paste0() of literals),
# plus Recall(). A call through an alias (`sq2 <- square_one; sq2(x)`, or
# `helpers <- list(sq = square_one); helpers$sq(x)`) is a call to the
# function. `Class$new()` is a call to `Class$initialize`; `self$m()`,
# `private$m()`, `super$m()` and `obj$m()` for an `obj <- Class$new()` in the
# same function resolve up the `inherit =` chain. A target's command names
# the targets it depends on. Only edges to functions defined in the project
# are kept: calls into packages are noise in a dependency map. Code outside
# any function (tests, scripts, top-level statements) is attributed to its
# file. R Markdown, Quarto and Sweave files are read chunk by chunk
# (parse_r_source() in colcheck-lib.R) and treated as scripts: knitr runs
# them top to bottom, so a function defined in a chunk is private to its
# document.
#
# Entry points: callmap_build(root) -> map; callmap_format(map),
#               callmap_neighbourhood(map, names, depth), callmap_json(map, ids).
# Helpers (call_name, call_args, quiet, local_names, relative_path, %||%) come
# from colcheck-lib.R, sourced by the caller.

# Same discovery as session-context.R, so the counts agree.
callmap_skip_dirs <- "/(renv|packrat|[.]git|node_modules)/"

callmap_files <- function(root) project_r_files(root)

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

# Calls that return a function made from the one they are given: `f2 <-
# memoise(f)` makes f2 a name for f, `f2 <- memoise(function(x) ...)`
# defines f2.
callmap_wrappers <- c("memoise", "Vectorize", "cmpfun", "Negate", "partial", "compose", "insistently", "safely",
                      "possibly", "quietly", "slowly", "as_function", "structure", "rate_limited")

# Classes a method may be written for without the project ever naming them
# in a `class =` argument: print.lm, format.data.frame, summary.default.
callmap_base_classes <- c(
  "default", "numeric", "integer", "double", "character", "logical", "complex", "raw", "list", "NULL", "function",
  "matrix", "array", "data.frame", "factor", "ordered", "Date", "POSIXct", "POSIXlt", "POSIXt", "difftime",
  "formula", "table", "ts", "lm", "glm", "nls", "htest", "environment", "name", "call", "expression", "AsIs",
  "noquote", "tbl_df", "tbl", "grouped_df", "rowwise_df", "data.table", "sf", "ggplot", "gg", "Rcpp_Module")

# targets factories whose first argument is the target name.
callmap_target_calls <- "^tar_(target|target_raw|file|files|files_input|file_read|age|change|force|format|rds|qs|fst|fst_tbl|fst_dt|parquet|feather|url|aws_.*)$"

# A name as written in NAMESPACE or an argument: symbol, string or pkg::name.
arg_name <- function(x) {
  if (is.symbol(x) || (is.character(x) && length(x) == 1L)) return(as.character(x))
  ns_name(x)
}

# A name as a value: symbol or pkg::name, never a string.
sym_name <- function(x) if (is.symbol(x)) as.character(x) else ns_name(x)

rx_escape <- function(s) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", s)

namespace_info <- function(root) {
  out <- list(exports = character(), patterns = character(), s3 = list(), imports = character())
  ex <- quiet(parse(file.path(root, "NAMESPACE"), keep.source = FALSE))
  for (e in ex) {
    fn <- call_name(e)
    if (is.na(fn)) next
    a <- vapply(call_args(e), arg_name, "")
    if (fn == "export") out$exports <- c(out$exports, a)
    if (fn == "exportPattern") out$patterns <- c(out$patterns, a)
    if (fn == "importFrom" && length(a) >= 2L) out$imports <- c(out$imports, a[-1L])
    if (fn == "S3method" && length(a) >= 2L) {
      method <- if (length(a) >= 3L && !is.na(a[[3L]])) a[[3L]] else paste0(a[[1L]], ".", a[[2L]])
      out$s3[[method]] <- a[[1L]]
    }
  }
  out
}

# Files whose code runs as a script, so a function defined there is private
# to it: a test file, or any file R CMD check runs from tests/.
is_test_script <- function(rel) grepl("(^|/)test[-_][^/]*[.][Rr]$", rel) | grepl("^tests?/[^/]+[.][Rr]$", rel)

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
fn_end <- function(fn, fallback) {
  sr <- if (length(fn) >= 4L) fn[[4L]] else NULL
  if (inherits(sr, "srcref")) sr[[3L]] else fallback
}

is_r6_class <- function(e) is.call(e) && identical(call_name(e), "R6Class")

# Methods of an `R6Class(...)` call: list of list(name, fn, active).
r6_methods <- function(rhs) {
  out <- list()
  args <- call_args(rhs)
  for (section in intersect(names(args), c("public", "private", "active"))) {
    lst <- args[[section]]
    if (!is.call(lst) || !identical(call_name(lst), "list")) next
    items <- call_args(lst)
    for (nm in names(items)) if (nzchar(nm) && is_function_expr(items[[nm]])) {
      out[[length(out) + 1L]] <- list(name = nm, fn = items[[nm]], active = section == "active")
    }
  }
  out
}

# The `inherit = Parent` of an `R6Class(...)` call, or NA.
r6_parent <- function(rhs) {
  p <- call_args(rhs)[["inherit"]]
  if (is.null(p)) NA_character_ else sym_name(p)
}

# The function literal a wrapper call defines: `memoise(function(x) ...)`.
wrapped_fn <- function(rhs) {
  if (!is.call(rhs) || !(call_name(rhs) %in% callmap_wrappers)) return(NULL)
  args <- call_args(rhs)
  if (length(args) && is_function_expr(args[[1L]])) args[[1L]] else NULL
}

# `a <- b`, `a <- memoise(b)` and `helpers <- list(sq = b)`: the names (`a`,
# `helpers$sq`) bound to another name, which callmap_build() keeps when that
# name is a project function.
alias_candidates <- function(name, rhs) {
  s <- sym_name(rhs)
  if (!is.na(s)) return(stats::setNames(s, name))
  if (!is.call(rhs)) return(character())
  fn <- call_name(rhs)
  args <- call_args(rhs)
  if (!is.na(fn) && fn %in% callmap_wrappers && length(args) && !is.na(s <- sym_name(args[[1L]]))) return(stats::setNames(s, name))
  out <- character()
  if (!is.na(fn) && fn == "list") {
    nms <- names(args) %||% rep("", length(args))
    for (k in seq_along(args)) if (nzchar(nms[[k]]) && !is.na(s <- sym_name(args[[k]]))) out[[paste0(name, "$", nms[[k]])]] <- s
  }
  out
}

# A string known when the code is read: a literal, or paste0()/paste() of
# literals.
literal_string <- function(e) {
  if (is.character(e) && length(e) == 1L) return(e)
  if (!is.call(e) || !(call_name(e) %in% c("paste0", "paste"))) return(NULL)
  args <- call_args(e)
  nms <- names(args) %||% rep("", length(args))
  sep <- if (call_name(e) == "paste") " " else ""
  if ("sep" %in% nms) { if (!is.character(args[["sep"]])) return(NULL); sep <- args[["sep"]][[1L]] }
  parts <- args[!(nms %in% c("sep", "collapse"))]
  if ("collapse" %in% nms || !length(parts) || !all(vapply(parts, function(p) is.character(p) && length(p) == 1L, TRUE))) return(NULL)
  paste(unlist(parts), collapse = sep)
}

# Names assigned one known string in a body (`fname <- "f"`, then
# `do.call(fname, ...)`): named character, NA when assigned more than once.
const_strings <- function(e, acc = character()) {
  if (!is.call(e)) return(acc)
  parts <- assignment_parts(e)
  if (!is.null(parts)) acc[[parts$name]] <- if (parts$name %in% names(acc)) NA_character_ else literal_string(parts$rhs) %||% NA_character_
  for (p in call_args(e)) acc <- const_strings(p, acc)
  acc
}

# The names tar_target() and the other factories declare, anywhere in an expression.
target_names_in <- function(e, acc = character()) {
  if (!is.call(e)) return(acc)
  fn <- call_name(e)
  if (!is.na(fn) && grepl(callmap_target_calls, fn) && length(e) >= 3L && !is.na(arg_name(e[[2L]]))) acc <- c(acc, arg_name(e[[2L]]))
  for (p in call_args(e)) acc <- target_names_in(p, acc)
  acc
}

# --- pass 1: parse every file and collect definitions -------------------------

new_defs <- function() {
  env <- new.env()
  env$n <- 0L
  env$rows <- list()
  env
}

add_def <- function(defs, name, label, file, line, args, kind, fn = NULL, class = NA_character_, end = line,
                    parent = NA_character_, active = FALSE) {
  defs$n <- defs$n + 1L
  defs$rows[[defs$n]] <- list(name = name, label = label, file = file, line = as.integer(line), end = as.integer(end),
                              args = args, kind = kind, fn = fn, class = class, parent = parent, active = active)
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
  # A literate file or a test file is a script (its definitions are private
  # to it, see resolve_edges()); a file whose top-level code calls project
  # functions is found to be one once the edges are walked.
  script <- is_literate_file(path) || is_test_script(rel)
  locals <- character()
  aliases <- character()
  for (i in seq_along(ex)) {
    line <- srcrefs[[i]][[1L]]
    end <- srcrefs[[i]][[3L]]
    parts <- assignment_parts(ex[[i]])
    if (is.null(parts)) {
      locals <- local_names(ex[[i]], locals)
      next
    }
    fn <- if (is_function_expr(parts$rhs)) parts$rhs else wrapped_fn(parts$rhs)
    if (!is.null(fn)) {
      units[[i]] <- add_def(defs, parts$name, parts$name, rel, line, fn_args(fn), "function", fn, end = end)
    } else if (is_r6_class(parts$rhs)) {
      cls <- add_def(defs, parts$name, parts$name, rel, line, "", "r6_class", NULL, parts$name, end = end, parent = r6_parent(parts$rhs))
      units[[i]] <- c(cls, vapply(r6_methods(parts$rhs), function(m) {
        add_def(defs, paste0(parts$name, "$", m$name), paste0(parts$name, "$", m$name), rel,
                fn_line(m$fn, line), fn_args(m$fn), "r6_method", m$fn, parts$name, end = fn_end(m$fn, end), active = m$active)
      }, 1L))
    } else {
      aliases <- c(aliases, alias_candidates(parts$name, parts$rhs))
      locals <- local_names(ex[[i]], locals)
    }
  }
  # source("x.R") with a literal path: an edge from this file to that file.
  sources <- unique(unlist(lapply(ex, source_calls)))
  targets <- if (basename(rel) == "_targets.R") unique(unlist(lapply(ex, target_names_in))) else character()
  code <- sub("#.*$", "", lines)
  list(rel = rel, path = path, exprs = ex, srcrefs = srcrefs, lines = lines, code = code, units = units, sources = sources,
       script = script, locals = unique(locals), aliases = aliases, targets = targets, chunk_errors = chunk_errors,
       classes = class_strings(code), s3reg = s3_registrations(code))
}

resolve_source <- function(s, file_rel, root) {
  cands <- c(file.path(root, dirname(file_rel), s), file.path(root, s), s)
  hit <- cands[file.exists(cands)]
  if (!length(hit)) return(s)
  relative_path(hit[[1L]], root)
}

# --- pass 2: walk each unit and collect edges ---------------------------------

# The line of the call: the first line of the statement that names it in
# code (comments are stripped, so a comment naming the callee does not win).
pin_line <- function(w, nm, l1, l2) {
  if (is.na(l2) || l2 <= l1) return(l1)
  pat <- w$patterns[[nm]]
  if (is.null(pat)) {
    pat <- paste0("(^|[^A-Za-z0-9._])", rx_escape(nm), "($|[^A-Za-z0-9._])")
    w$patterns[[nm]] <- pat
  }
  hit <- grep(pat, w$code[l1:l2])
  if (length(hit)) l1 + hit[[1L]] - 1L else l1
}

# `bare`: the edge is from a top-level statement that is not an assignment
# (nor inside a function literal): the mark of a script, see resolve_edges().
add_edge <- function(w, to, kind, l1, l2, pin = to) {
  w$n <- w$n + 1L
  w$edges[[w$n]] <- list(from_def = w$owner_def, from = w$owner, file = w$file, to = to, kind = kind,
                         line = pin_line(w, pin, l1, l2), bare = isTRUE(w$bare))
}

is_project_fn <- function(w, nm) !is.na(nm) && nzchar(nm) && !is.null(w$names[[nm]])

# A project function by its own name or through an alias (`sq2 <- square_one`), or NULL.
resolve_name <- function(w, nm) {
  if (is.na(nm) || !nzchar(nm)) return(NULL)
  if (!is.null(w$names[[nm]])) return(nm)
  w$aliases[[nm]]
}

# `Class$method` for `Class` or the nearest ancestor (`inherit =`) that defines it, or NULL.
r6_resolve <- function(w, cls, method) {
  seen <- character()
  while (!is.null(cls) && !is.na(cls) && !(cls %in% seen)) {
    target <- paste0(cls, "$", method)
    if (is_project_fn(w, target)) return(target)
    seen <- c(seen, cls)
    cls <- w$r6[[cls]]
  }
  NULL
}

# What `recv$field` names: `self$m`/`private$m`/`super$m` inside a class,
# `Class$new` (its initialize, else the class), `obj$m` for an obj the unit
# made with `Class$new()`, or an element of a list of functions
# (`helpers$sq`). NULL when it is none of these.
field_target <- function(w, recv, field) {
  if (is.na(field) || !nzchar(recv)) return(NULL)
  if (recv %in% c("self", "private") && !is.na(w$class)) return(r6_resolve(w, w$class, field))
  if (recv == "super" && !is.na(w$class)) return(r6_resolve(w, w$r6[[w$class]], field))
  if (recv %in% w$assigned) {
    cls <- unname(unit_receivers(w)[recv])
    return(if (!is.na(cls)) r6_resolve(w, cls, field))
  }
  # a parameter named like a class or a list of functions is neither
  if (recv %in% w$locals) return(NULL)
  if (field == "new" && !is.null(w$r6[[recv]])) return(r6_resolve(w, recv, "initialize") %||% recv)
  w$aliases[[paste0(recv, "$", field)]]
}

# What the unit (a function body, or a file's top-level code) assigns, read
# on first need: the objects it makes with `Class$new()` and the strings it
# binds to names. Most units need neither, so the walk does not pay for them.
unit_receivers <- function(w) {
  if (is.null(w$receivers)) {
    acc <- character()
    if (w$has_r6 && any(grepl("$new", w$lines[w$l1:w$l2], fixed = TRUE))) for (ex in w$unit) acc <- r6_receivers(ex, w, acc)
    w$receivers <- acc
  }
  w$receivers
}

unit_consts <- function(w) {
  if (is.null(w$consts)) {
    acc <- character()
    for (ex in w$unit) acc <- const_strings(ex, acc)
    w$consts <- acc
  }
  w$consts
}

# `x <- Class$new(...)` (also `Class$new(...)$clone()`) in an expression:
# named character, variable -> class.
r6_receivers <- function(e, w, acc = character()) {
  if (!is.call(e)) return(acc)
  parts <- assignment_parts(e)
  if (!is.null(parts) && is.call(parts$rhs) && is.call(parts$rhs[[1L]])) {
    h <- parts$rhs[[1L]]
    if (identical(call_name(h), "$") && length(h) == 3L && identical(arg_name(h[[3L]]), "clone") && is.call(h[[2L]]) && is.call(h[[2L]][[1L]])) h <- h[[2L]][[1L]]
    if (identical(call_name(h), "$") && length(h) == 3L && is.symbol(h[[2L]]) && identical(arg_name(h[[3L]]), "new") &&
        !is.null(w$r6[[as.character(h[[2L]])]])) acc[[parts$name]] <- as.character(h[[2L]])
  }
  for (p in call_args(e)) acc <- r6_receivers(p, w, acc)
  acc
}

# Strings the code uses as class names, for finish_defs(): `class = "x"`
# in any call, `class(x) <- "x"`, `inherits(x, "x")`, setClass()/
# setOldClass()/R6Class("x"). Read from the text (comments stripped), which
# is cheaper than looking at every call in the walk and serves a heuristic.
callmap_string_rx <- "(\"[^\"]*\"|'[^']*')"
callmap_class_rx <- paste0(
  "(class\\s*=|(class|oldClass)\\([^<]*\\)\\s*<-|inherits\\((?:[^,()]|\\([^()]*\\))*,|",
  "(setClass|setOldClass|setRefClass|R6Class|(?<![$@\\w.])new)\\()\\s*c?\\(?\\s*", callmap_string_rx, "(\\s*,\\s*", callmap_string_rx, ")*")
class_strings <- function(code) {
  code <- code[grepl("class|inherits\\(|new\\(", code)]
  hits <- unlist(regmatches(code, gregexpr(callmap_class_rx, code, perl = TRUE)))
  out <- unlist(regmatches(hits, gregexpr(callmap_string_rx, hits)))
  unique(substr(out, 2L, nchar(out) - 1L))
}

# S3 methods registered by hand: .S3method("print", "x"), registerS3method("print", "x", f).
s3_registrations <- function(code) {
  rx <- paste0("(\\.S3method|registerS3method)\\(\\s*", callmap_string_rx, "\\s*,\\s*", callmap_string_rx)
  code <- code[grepl("S3method", code, fixed = TRUE)]
  hits <- unlist(regmatches(code, gregexpr(rx, code, perl = TRUE)))
  out <- list()
  for (h in hits) {
    s <- unlist(regmatches(h, gregexpr(callmap_string_rx, h)))
    s <- substr(s, 2L, nchar(s) - 1L)
    out[[paste0(s[[1L]], ".", s[[2L]])]] <- s[[1L]]
  }
  out
}

walk_node <- function(e, l1, l2, w) {
  if (is.symbol(e)) {
    nm <- as.character(e)
    if (!nzchar(nm) || nm %in% w$locals) return(invisible())
    to <- resolve_name(w, nm)
    if (!is.null(to)) add_edge(w, to, "value", l1, l2, pin = nm)
    else if (isTRUE(w$in_target) && !is.null(w$target_names[[nm]])) add_edge(w, paste("target", nm), "direct", l1, l2, pin = nm)
    return(invisible())
  }
  if (!is.call(e)) return(invisible())
  head <- e[[1L]]
  fn <- NA_character_
  if (is.symbol(head)) {
    fn <- as.character(head)
    to <- if (fn %in% w$local_fns) NULL else resolve_name(w, fn)
    if (!is.null(to)) add_edge(w, to, "direct", l1, l2, pin = fn)
  } else if (is.call(head) && is.symbol(head[[1L]])) {
    hh <- as.character(head[[1L]])
    if (hh %in% c("::", ":::") && length(head) == 3L) {
      fn <- as.character(head[[3L]])
      if (!is.null(w$pkg) && identical(as.character(head[[2L]]), w$pkg) && is_project_fn(w, fn)) add_edge(w, fn, "direct", l1, l2)
      # box::use(./mod[f]): an import list, not a use of f.
      if (fn == "use" && identical(as.character(head[[2L]]), "box")) return(invisible())
    } else if (hh %in% c("$", "[[") && length(head) == 3L && is.symbol(head[[2L]])) {
      recv <- as.character(head[[2L]])
      field <- arg_name(head[[3L]])
      target <- field_target(w, recv, field)
      if (!is.null(target)) add_edge(w, target, "direct", l1, l2, pin = if (identical(field, "new")) recv else field)
      else walk_node(head, l1, l2, w)
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
    bare <- w$bare; w$bare <- FALSE
    for (k in seq_along(fm)) if (!identical(fm[[k]], quote(expr = ))) walk_node(fm[[k]], l1, l2, w)
    walk_node(e[[3L]], l1, l2, w)
    w$bare <- bare
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
    # A method or active binding as a value: `lapply(xs, self$process)`, `acct$pretty`.
    if (fn == "$" && is.symbol(e[[2L]]) && !is.null(target <- field_target(w, as.character(e[[2L]]), arg_name(e[[3L]])))) {
      add_edge(w, target, if (isTRUE(w$active[[target]])) "direct" else "value", l1, l2, pin = arg_name(e[[3L]]))
    }
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
    to <- if (nm %in% w$local_fns) NULL else resolve_name(w, nm)
    if (!is.null(to)) add_edge(w, to, "direct", l1, l2, pin = nm)
    return(invisible())
  }
  if (fn == "Recall" && !is.na(w$owner_def)) add_edge(w, w$owner, "string", l1, l2, pin = "Recall")
  if (fn %in% callmap_string_takers) {
    for (p in call_args(e)) {
      s <- if (is.symbol(p)) { if (as.character(p) %in% w$assigned) unname(unit_consts(w)[as.character(p)]) } else literal_string(p)
      if (!is.null(s) && !is.na(s) && is_project_fn(w, s)) add_edge(w, s, "string", l1, l2, pin = if (is.symbol(p)) as.character(p) else s)
    }
  }
  if (w$has_targets && fn %in% c("tar_read", "tar_load", "tar_read_raw", "tar_load_raw") && length(e) >= 2L &&
      !is.na(nm <- arg_name(e[[2L]])) && !is.null(w$target_names[[nm]])) add_edge(w, paste("target", nm), "direct", l1, l2, pin = nm)
  if (w$targets && grepl(callmap_target_calls, fn) && length(e) >= 3L && !is.na(arg_name(e[[2L]]))) {
    walk_target(e, fn, l1, l2, w)
    return(invisible())
  }
  for (p in call_args(e)) walk_node(p, l1, l2, w)
  invisible()
}

# tar_target(name, command): a pseudo-definition `target name` that owns the
# calls in its command, pinned to the line of its own `tar_target(name`.
walk_target <- function(e, fn, l1, l2, w) {
  name <- arg_name(e[[2L]])
  hit <- if (!is.na(l2) && l2 > l1) grep(paste0(rx_escape(fn), "\\(\\s*[\"']?", rx_escape(name), "[\"']?\\s*[,)]"), w$code[l1:l2]) else integer()
  line <- if (length(hit)) l1 + hit[[1L]] - 1L else pin_line(w, name, l1, l2)
  id <- add_def(w$defs, name, paste("target", name), w$file, line, "", "target", end = l2)
  saved <- list(w$owner, w$owner_def, w$in_target)
  w$owner <- paste("target", name)
  w$owner_def <- id
  w$in_target <- TRUE
  args <- call_args(e)
  for (p in args[-1L]) walk_node(p, l1, l2, w)
  w$owner <- saved[[1L]]
  w$owner_def <- saved[[2L]]
  w$in_target <- saved[[3L]]
}

walk_file <- function(f, w) {
  w$file <- f$rel
  w$lines <- f$lines
  w$code <- f$code
  w$targets <- basename(f$rel) == "_targets.R"
  w$in_target <- FALSE
  # The file's top-level code is one unit (its receivers and constant
  # strings, read on first need, carry across its statements).
  top <- list(unit = as.list(f$exprs), receivers = NULL, consts = NULL)
  for (i in seq_along(f$exprs)) {
    sr <- f$srcrefs[[i]]
    ids <- f$units[[i]]
    if (is.null(ids)) {
      w$owner <- f$rel; w$owner_def <- NA_integer_; w$class <- NA_character_
      # Variables of top-level code (`a <- result[...]` in a test) are not
      # the project function of the same name either.
      w$locals <- f$locals; w$local_fns <- character(); w$assigned <- f$locals
      w$unit <- top$unit; w$receivers <- top$receivers; w$consts <- top$consts; w$l1 <- 1L; w$l2 <- length(f$lines)
      w$bare <- !(is.call(f$exprs[[i]]) && call_name(f$exprs[[i]]) %in% c("<-", "=", "<<-"))
      walk_node(f$exprs[[i]], sr[[1L]], sr[[3L]], w)
      top$receivers <- w$receivers; top$consts <- w$consts
      next
    }
    w$bare <- FALSE
    for (id in ids) {
      d <- w$defs$rows[[id]]
      if (is.null(d$fn)) next
      w$owner <- d$label; w$owner_def <- id; w$class <- d$class
      # A parameter or local variable with the name of a project function is
      # not that function: `wrap <- function(x, f) lapply(x, f)`.
      formals_nm <- names(d$fn[[2L]]) %||% character()
      w$assigned <- local_names(d$fn[[3L]])
      w$locals <- c(formals_nm, w$assigned)
      w$local_fns <- c(formals_nm, local_function_names(d$fn[[3L]]))
      w$unit <- list(d$fn[[3L]]); w$receivers <- NULL; w$consts <- NULL; w$l1 <- sr[[1L]]; w$l2 <- sr[[3L]]
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
    # A scan failure on odd code costs that file, never the map.
    f <- tryCatch(scan_file(paths[[k]], rel[[k]], defs),
                  error = function(e) list(rel = rel[[k]], error = paste("definitions not read:", conditionMessage(e)), chunk_errors = character()))
    if (!is.null(f$error)) skipped[[length(skipped) + 1L]] <- f else files[[length(files) + 1L]] <- f
    for (ce in f$chunk_errors) skipped[[length(skipped) + 1L]] <- list(rel = rel[[k]], error = ce)
  }

  w <- new.env()
  w$defs <- defs; w$pkg <- pkg; w$n <- 0L; w$edges <- list(); w$patterns <- new.env()
  w$names <- new.env()
  for (d in defs$rows) if (d$kind != "target") assign(d$name, TRUE, envir = w$names)
  # R6: each class's parent, and which methods are active bindings.
  w$r6 <- new.env(); w$active <- new.env()
  for (d in defs$rows) {
    if (d$kind == "r6_class") assign(d$name, d$parent, envir = w$r6)
    if (isTRUE(d$active)) assign(d$name, TRUE, envir = w$active)
  }
  w$has_r6 <- length(ls(w$r6)) > 0L
  # Aliases of project functions (`sq2 <- square_one`, `helpers$sq`), chains
  # (`a <- b; b <- fn`) followed; a name that is itself a definition is not
  # an alias. The alias is then not a local of its file's top-level code.
  w$aliases <- new.env()
  cands <- unlist(lapply(files, function(f) f$aliases))
  repeat {
    before <- length(ls(w$aliases))
    for (k in seq_along(cands)) {
      nm <- names(cands)[[k]]
      to <- w$aliases[[cands[[k]]]] %||% cands[[k]]
      if (nm != to && is.null(w$names[[nm]]) && is_project_fn(w, to)) assign(nm, to, envir = w$aliases)
    }
    if (length(ls(w$aliases)) == before) break
  }
  roots <- unique(sub("[$].*$", "", ls(w$aliases)))
  for (k in seq_along(files)) files[[k]]$locals <- setdiff(files[[k]]$locals, roots)
  w$target_names <- new.env()
  for (f in files) for (t in f$targets) assign(t, TRUE, envir = w$target_names)
  w$has_targets <- length(ls(w$target_names)) > 0L
  # A walker failure costs that file's edges, never the whole map.
  for (k in seq_along(files)) {
    err <- tryCatch({ walk_file(files[[k]], w); NULL }, error = function(e) conditionMessage(e))
    if (!is.null(err)) skipped[[length(skipped) + 1L]] <- list(rel = files[[k]]$rel, error = paste("edges not read:", err))
  }
  # A file whose top-level statements run project code is a script.
  callers <- unique(unlist(lapply(w$edges, function(x) if (is.na(x$from_def) && x$bare) x$file)))
  for (k in seq_along(files)) files[[k]]$script <- files[[k]]$script || files[[k]]$rel %in% callers

  map <- list(root = root, pkg = pkg, is_pkg = is_pkg, files = files, skipped = skipped,
              rmd = vapply(found$rmd, relative_path, "", root = root, USE.NAMES = FALSE),
              n_files = length(paths))
  map$defs <- finish_defs(defs$rows, ns, unique(unlist(lapply(files, `[[`, "classes"))), do.call(c, c(list(), lapply(files, `[[`, "s3reg"))))
  script <- stats::setNames(vapply(files, `[[`, TRUE, "script"), vapply(files, `[[`, "", "rel"))
  map$defs$private <- !startsWith(map$defs$file, "R/") & script[map$defs$file] %in% TRUE
  map$edges <- resolve_edges(w$edges, map$defs, files, root)
  map
}

# Exported flags, S3 roles and a table of definitions. A dotted name is an S3
# method when NAMESPACE (or .S3method()) registers it, when its prefix is a
# generic the project defines, or when the prefix is a base generic and the
# suffix is a class the code names (`class = "money"`): `t.test_wrapper`
# and `summary.stats` are plain functions.
finish_defs <- function(rows, ns, classes = character(), registered = list()) {
  name <- vapply(rows, `[[`, "", "name")
  kind <- vapply(rows, `[[`, "", "kind")
  generic <- vapply(rows, function(d) d$kind == "function" && has_call(d$fn[[3L]], "UseMethod"), TRUE)
  project_generics <- name[generic]
  generics <- unique(c(project_generics, callmap_base_generics, ns$imports, unlist(ns$s3, use.names = FALSE)))
  classes <- c(classes, callmap_base_classes)
  hint <- rep(NA_character_, length(rows))
  s3_of <- vapply(seq_along(rows), function(i) {
    if (kind[[i]] != "function") return(NA_character_)
    nm <- name[[i]]
    if (!is.null(ns$s3[[nm]])) return(ns$s3[[nm]])
    if (!is.null(registered[[nm]])) return(registered[[nm]])
    dots <- gregexpr(".", nm, fixed = TRUE)[[1L]]
    for (p in rev(dots[dots > 1L & dots < nchar(nm)])) {
      prefix <- substr(nm, 1L, p - 1L)
      if (!(prefix %in% generics)) next
      if (prefix %in% project_generics || substr(nm, p + 1L, nchar(nm)) %in% classes) return(prefix)
      hint[i] <<- sprintf("[not an S3 method of %s: no class \"%s\" in the project]", prefix, substr(nm, p + 1L, nchar(nm)))
    }
    NA_character_
  }, "")
  kind[!is.na(s3_of)] <- "s3_method"
  exported <- name %in% ns$exports
  for (p in ns$patterns) exported <- exported | (kind %in% c("function", "s3_method") & grepl(p, name))
  exported[kind %in% c("r6_method", "target")] <- FALSE
  list(name = name, label = vapply(rows, `[[`, "", "label"), file = vapply(rows, `[[`, "", "file"),
       line = vapply(rows, `[[`, 1L, "line"), end = vapply(rows, `[[`, 1L, "end"),
       args = vapply(rows, `[[`, "", "args"), kind = kind,
       exported = exported, generic = generic, s3_of = s3_of, s3_hint = hint)
}

# Edge names to definition ids. Which definitions a file can see: those in
# R/ and in files that only define things (they are sourced or loaded as a
# whole: lib/, functions/, testthat helpers), plus those in the file itself
# and in the files it source()s, transitively. A function defined inside a
# script (a file whose top-level statements call project functions, a test
# file, a vignette) is private to it: a `make_data()` helper in one test
# file is not called by another test file that defines its own. Among the
# visible definitions of a name, the one in the same file wins, then a
# source()d one, then R/.
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
  shared <- !defs$private
  by_name <- split(seq_along(defs$label), defs$label)
  # the definition(s) each edge reaches; one row per pair
  to <- lapply(edges, function(x) {
    cand <- by_name[[x$to]]
    near <- defs$file[cand] == x$file | defs$file[cand] %in% closures[[x$file]]
    cand <- cand[shared[cand] | near]
    if (length(cand) > 1L) {
      tiers <- list(cand[defs$file[cand] == x$file], cand[defs$file[cand] %in% closures[[x$file]]],
                    cand[startsWith(defs$file[cand], "R/")])
      for (t in tiers) if (length(t)) { cand <- t; break }
    }
    cand
  })
  idx <- rep(seq_along(edges), lengths(to))
  if (!length(idx)) return(empty)
  chr <- function(nm) vapply(edges, `[[`, "", nm)[idx]
  int <- function(nm) as.integer(vapply(edges, `[[`, 1, nm))[idx]
  out <- data.frame(from_def = int("from_def"), from = chr("from"), file = chr("file"), to_def = unlist(to),
                    kind = chr("kind"), line = int("line"), stringsAsFactors = FALSE)
  out[!duplicated(out), , drop = FALSE]
}

# --- text output ----------------------------------------------------------------

callmap_limits <- paste(
  "Static: misses calls built from strings at runtime (eval(parse()), names computed at run time),",
  "S4 dispatch, R6 calls on an object not made by Class$new() in the same function (a parameter,",
  "a field), functions held in a list other than by name (list(f = function...)), inline `r` code",
  "in Rmd/qmd prose (chunks are read), and calls into packages (only project functions are listed).",
  "A function defined in a script or a test file is [private to this file] and the files that source() it.")

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
  if (!is.na(d$s3_hint[[i]])) tags <- c(tags, d$s3_hint[[i]])
  if (d$private[[i]] && d$kind[[i]] != "target" && map$n_files > 1L) tags <- c(tags, "[private to this file]")
  dup <- which(d$label == d$label[[i]] & seq_along(d$label) != i)
  if (length(dup)) tags <- c(tags, sprintf("[also defined at %s]", paste(sprintf("%s:%d", d$file[dup], d$line[dup]), collapse = ", ")))
  paste(tags, collapse = " ")
}

def_signature <- function(map, i) {
  d <- map$defs
  if (d$kind[[i]] == "target") d$label[[i]]
  else if (d$kind[[i]] == "r6_class") paste(d$label[[i]], "(R6 class)")
  else sprintf("%s(%s)", d$label[[i]], d$args[[i]])
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

callmap_neighbourhood <- function(map, names, depth = 2L, header = TRUE) {
  d <- map$defs
  e <- map$edges
  out <- if (header) callmap_header(map, sprintf("around %s, depth %d", paste(names, collapse = ", "), depth)) else character()
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
      if (d$kind[[i]] == "r6_class") {
        m <- which(d$kind == "r6_method" & startsWith(d$label, paste0(d$label[[i]], "$")))
        if (length(m)) out <- c(out, paste0("  methods: ", paste(sprintf("%s (%s)", d$label[m], vapply(m, function(k) def_loc(map, k), "")), collapse = ", ")))
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
    out <- list(name = d$label[[i]], file = d$file[[i]], line = d$line[[i]], end = d$end[[i]], args = d$args[[i]],
                exported = d$exported[[i]], kind = d$kind[[i]], calls = calls,
                called_by = lapply(in_rows, function(r) list(by = e$from[[r]], kind = e$kind[[r]], file = e$file[[r]], line = e$line[[r]])))
    if (!is.na(d$s3_of[[i]])) out$s3_generic <- d$s3_of[[i]]
    if (!is.na(d$s3_hint[[i]])) out$s3_hint <- d$s3_hint[[i]]
    if (d$private[[i]]) out$private <- TRUE
    out
  })
}


# --- one file ---------------------------------------------------------------------

# The definitions of one file, by its path relative to the root (or any path
# ending in it).
callmap_file_ids <- function(map, path) {
  rel <- tryCatch(relative_path(normalizePath(path, mustWork = FALSE), map$root), error = function(e) path)
  ids <- which(map$defs$file == rel)
  if (!length(ids)) ids <- which(endsWith(map$defs$file, paste0("/", path)) | map$defs$file == path)
  ids
}

# --- tests that reach a function --------------------------------------------------

is_test_file <- function(rel) grepl("^(tests?|inst/tests?)/", rel) | grepl("(^|/)test[-_][^/]*[.][Rr]$", rel)
# testthat's helper-*.R, setup-*.R and teardown-*.R, and tests/testthat.R,
# are run with the tests, not as tests: not something to list as the test to run.
is_test_runner_file <- function(rel) grepl("(^|/)(helper|setup|teardown)[^/]*[.][Rr]$", rel) | grepl("^tests/testthat[.][Rr]$", rel)

# For each definition, the test files whose code reaches it through callers
# (a test calling it directly, or a test calling a function that calls it,
# and so on), with the shortest chain for each. A caller inside a test file
# (a helper defined there) counts as that file's code.
callmap_tests <- function(map, ids) {
  d <- map$defs
  e <- map$edges
  reach <- list()
  for (i in ids) {
    # breadth-first up the callers; parent pointers give the chain
    parent <- list()
    queue <- i
    seen <- i
    found <- list()
    while (length(queue)) {
      v <- queue[[1L]]; queue <- queue[-1L]
      if (v != i && is_test_file(d$file[[v]]) && !is_test_runner_file(d$file[[v]])) {
        f <- d$file[[v]]
        if (is.null(found[[f]])) found[[f]] <- v
      }
      rows <- which(e$to_def == v & (is.na(e$from_def) | e$from_def != v))
      for (r in rows) {
        src <- e$from_def[[r]]
        if (is.na(src)) {
          f <- e$file[[r]]
          if (is_test_file(f) && !is_test_runner_file(f) && is.null(found[[f]])) found[[f]] <- v
          next
        }
        if (src %in% seen) next
        seen <- c(seen, src)
        parent[[as.character(src)]] <- v
        queue <- c(queue, src)
      }
      # callers of a method reach it through its generic
      if (!is.na(d$s3_of[[v]])) for (g in which(d$label == d$s3_of[[v]] & d$generic)) {
        if (g %in% seen) next
        seen <- c(seen, g)
        parent[[as.character(g)]] <- v
        queue <- c(queue, g)
      }
    }
    for (f in names(found)) {
      v <- found[[f]]
      chain <- character()
      while (v != i) { chain <- c(chain, d$label[[v]]); v <- parent[[as.character(v)]] }
      reach[[f]] <- c(reach[[f]], list(list(id = i, via = rev(chain))))
    }
  }
  reach
}

# devtools::test(filter = ) takes a regex, written inside an R string inside
# a shell single-quoted string: a regex character in the file's stem is
# escaped with a backslash, doubled for the R string.
test_command <- function(map, rel) {
  if (map$is_pkg && grepl("^tests/testthat/test[-_]", rel)) {
    stem <- sub("[.][Rr]$", "", sub("^tests/testthat/test[-_]", "", rel))
    stem <- gsub("([][{}()+*^$|?.\\\\])", "\\\\\\\\\\1", stem)
    sprintf("Rscript -e 'devtools::test(filter = \"^%s$\")'", gsub("'", "'\\\\''", stem))
  } else sprintf("Rscript -e 'testthat::test_file(\"%s\")'", gsub("'", "'\\\\''", rel))
}

tests_section <- function(map, ids, label = "the functions above") {
  d <- map$defs
  reach <- callmap_tests(map, ids)
  out <- character()
  if (length(reach)) {
    out <- c(out, sprintf("Tests that reach %s (run these):", label))
    for (f in names(reach)[order(names(reach), method = "radix")]) {
      items <- vapply(reach[[f]], function(x) paste0(d$label[[x$id]], if (length(x$via)) paste0(" (via ", paste(x$via, collapse = " <- "), ")") else " (directly)"), "")
      out <- c(out, sprintf("  %s   %s", f, paste(unique(items), collapse = ", ")), sprintf("    %s", test_command(map, f)))
    }
  }
  reached <- unique(unlist(lapply(reach, function(l) vapply(l, function(x) x$id, 1L))))
  none <- setdiff(ids, reached)
  none <- none[d$kind[none] != "target"]
  if (length(none)) out <- c(out, sprintf("No test reaches: %s", paste(d$label[none], collapse = ", ")))
  out
}

callmap_tests_view <- function(map, names) {
  out <- callmap_header(map, sprintf("tests reaching %s", paste(names, collapse = ", ")))
  ids <- integer()
  for (nm in names) {
    hit <- callmap_lookup(map, nm)
    if (!length(hit)) {
      near <- callmap_nearest(map, nm)
      out <- c(out, sprintf("%s: not defined in the project.%s", nm,
                            if (length(near)) paste0(" Nearest: ", paste(near, collapse = ", ")) else " No similar names."))
    }
    ids <- c(ids, hit)
  }
  out <- c(out, "")
  if (length(ids)) out <- c(out, tests_section(map, ids, if (length(ids) == 1L) map$defs$label[[ids]] else "them"))
  structure(out, ids = ids)
}

# --- what a git diff touches ------------------------------------------------------

# `git diff -U0 REF` for the R files under the root (HEAD when REF is NULL),
# plus untracked R files: per file, the new-side line ranges of the hunks
# and whether the file is new or deleted. NULL outside a git repository.
git_changes <- function(root, ref = NULL) {
  git <- function(...) {
    out <- suppressWarnings(tryCatch(system2("git", c("-C", shQuote(root), ...), stdout = TRUE, stderr = FALSE),
                                     error = function(e) NULL))
    if (is.null(out) || !is.null(attr(out, "status"))) return(NULL)
    out
  }
  top <- git("rev-parse", "--show-toplevel")
  if (is.null(top) || !length(top)) return(NULL)
  prefix <- git("rev-parse", "--show-prefix")
  prefix <- if (is.null(prefix) || !length(prefix)) "" else prefix[[1L]]
  pats <- c("*.R", "*.r", "*.Rmd", "*.rmd", "*.Rmarkdown", "*.qmd", "*.Rnw", "*.rnw")
  base <- ref %||% "HEAD"
  # the user's git config must not change the shape: prefixes a/ b/ always,
  # paths never quoted for non-ASCII
  d <- git("-c", "core.quotePath=false", "diff", "-U0", "--no-color", "--no-ext-diff", "--relative", "-M",
           "--src-prefix=a/", "--dst-prefix=b/", shQuote(base), "--", shQuote(pats))
  if (is.null(d)) return(structure(list(), error = sprintf("git diff %s failed (unknown revision, or no commit yet)", base)))
  files <- list()
  cur <- NULL
  for (ln in d) {
    m <- regmatches(ln, regexec("^diff --git a/(.*) b/(.*)$", ln))[[1L]]
    if (length(m) == 3L) {
      cur <- m[[3L]]
      files[[cur]] <- list(path = cur, old = m[[2L]], ranges = list(), new = FALSE, deleted = FALSE)
      next
    }
    if (is.null(cur)) next
    if (startsWith(ln, "--- /dev/null")) files[[cur]]$new <- TRUE
    if (startsWith(ln, "+++ /dev/null")) { files[[cur]]$deleted <- TRUE; files[[cur]]$path <- files[[cur]]$old }
    h <- regmatches(ln, regexec("^@@ -[0-9]+(,[0-9]+)? \\+([0-9]+)(,([0-9]+))? @@", ln))[[1L]]
    if (length(h)) {
      start <- as.integer(h[[3L]])
      n <- if (nzchar(h[[5L]])) as.integer(h[[5L]]) else 1L
      # a pure deletion sits after line `start` of the new file: it touches a
      # definition spanning that line, but names no top-level code (what was
      # removed is only in the old file, whose definitions are read apart)
      files[[cur]]$ranges[[length(files[[cur]]$ranges) + 1L]] <- c(max(start, 1L), max(start + n - 1L, start, 1L), as.integer(n == 0L))
    }
  }
  untracked <- git("-c", "core.quotePath=false", "ls-files", "--others", "--exclude-standard", "--", shQuote(pats))
  for (u in untracked %||% character()) {
    if (!is.null(files[[u]])) next
    files[[u]] <- list(path = u, old = NA_character_, ranges = list(c(1L, .Machine$integer.max, 0L)), new = TRUE, deleted = FALSE)
  }
  structure(files, prefix = prefix, base = base)
}

# The definitions of one file at REF (its old text through `git show`).
old_definitions <- function(root, prefix, base, old_path) {
  text <- suppressWarnings(tryCatch(system2("git", c("-C", shQuote(root), "show", shQuote(paste0(base, ":", prefix, old_path))),
                                            stdout = TRUE, stderr = FALSE), error = function(e) NULL))
  if (is.null(text) || !is.null(attr(text, "status"))) return(NULL)
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(old_path)))
  on.exit(unlink(tmp))
  writeLines(text, tmp)
  defs <- new_defs()
  f <- quiet(scan_file(tmp, old_path, defs))
  if (is.null(f) || !is.null(f$error)) return(NULL)
  defs$rows
}

# A definition spanning lines a..b is touched by a hunk's new-side range; a
# pure deletion sits between lines r1 and r1 + 1, so it touches the
# definition only when both lines are inside it.
in_ranges <- function(a, b, ranges) any(vapply(ranges, function(r) {
  if (length(r) >= 3L && r[[3L]] == 1L) a <= r[[1L]] && r[[1L]] < b else a <= r[[2L]] && b >= r[[1L]]
}, TRUE))

# Lines in a file (outside every definition) that mention a name.
mention_sites <- function(map, name, limit = 12L) {
  pat <- paste0("(^|[^A-Za-z0-9._])", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", name), "($|[^A-Za-z0-9._])")
  out <- character()
  for (f in map$files) {
    hit <- grep(pat, f$lines)
    if (length(hit)) out <- c(out, sprintf("%s:%d", f$rel, hit))
    if (length(out) >= limit) break
  }
  out
}

# What the working tree's diff against REF touches: the definitions whose
# lines changed (new, signature changed, body changed), the definitions that
# are gone and where they are still mentioned, the top-level code that
# changed, then the callers and callees of the changed definitions and the
# tests that reach them.
callmap_diff <- function(map, ref = NULL, depth = 2L) {
  d <- map$defs
  e <- map$edges
  changes <- git_changes(map$root, ref)
  base <- ref %||% "HEAD"
  out <- callmap_header(map, sprintf("changes in the working tree against %s", base))
  if (is.null(changes)) return(c(out, "", sprintf("%s is not inside a git repository, so there is no diff to read.", map$root)))
  if (!is.null(attr(changes, "error"))) return(c(out, "", attr(changes, "error")))
  if (!length(changes)) return(c(out, "", sprintf("No R file differs from %s (tracked or untracked).", base)))
  prefix <- attr(changes, "prefix")
  changed <- integer()
  how <- character()
  deleted <- list()
  top_level <- character()
  unmapped <- character()
  for (ch in changes) {
    old <- if (!ch$new && !is.na(ch$old)) old_definitions(map$root, prefix, base, ch$old) else list()
    old_by_name <- stats::setNames(old, vapply(old, `[[`, "", "label"))
    if (ch$deleted) {
      for (o in old) if (!(o$label %in% d$label)) deleted[[o$label]] <- o
      next
    }
    ids <- which(d$file == ch$path)
    known <- ch$path %in% vapply(map$files, `[[`, "", "rel")
    if (!known) { unmapped <- c(unmapped, ch$path); next }
    touched <- ids[vapply(ids, function(i) in_ranges(d$line[[i]], d$end[[i]], ch$ranges), TRUE)]
    # a hunk inside an R6 method is that method's change, not its class's
    for (i in touched[d$kind[touched] == "r6_class"]) {
      if (any(d$kind[touched] == "r6_method" & startsWith(d$label[touched], paste0(d$label[[i]], "$")))) touched <- setdiff(touched, i)
    }
    for (i in touched) {
      o <- old_by_name[[d$label[[i]]]]
      how[[as.character(i)]] <- if (is.null(o)) "new" else if (!identical(o$args, d$args[[i]])) sprintf("arguments were (%s)", o$args) else "body changed"
    }
    changed <- c(changed, touched)
    for (o in old) if (!(o$label %in% d$label)) deleted[[o$label]] <- o
    # lines of the hunks outside every definition: top-level code
    covered <- unlist(lapply(ids, function(i) seq.int(d$line[[i]], d$end[[i]])))
    text <- map$files[[match(ch$path, vapply(map$files, `[[`, "", "rel"))]]$lines
    free <- unlist(lapply(Filter(function(r) r[[3L]] == 0L, ch$ranges), function(r) seq.int(r[[1L]], min(r[[2L]], length(text)))))
    free <- setdiff(free, covered)
    # blank and comment lines are not code
    free <- free[grepl("[^[:space:]]", text[free]) & !grepl("^[[:space:]]*#", text[free])]
    if (length(free)) {
      runs <- split(free, cumsum(c(TRUE, diff(free) != 1L)))
      spans <- vapply(runs, function(r) if (length(r) == 1L) as.character(r[[1L]]) else sprintf("%d-%d", r[[1L]], r[[length(r)]]), "")
      rows <- which(e$file == ch$path & is.na(e$from_def) & e$line %in% free)
      calls <- callee_list(map, rows)
      top_level <- c(top_level, sprintf("  %s (lines %s)%s", ch$path, paste(spans, collapse = ", "),
                                        if (nzchar(calls)) paste0("   calls: ", calls) else ""))
    }
  }
  changed <- unique(changed)
  out <- c(out, "")
  if (length(changed)) {
    out <- c(out, sprintf("Changed definitions (%d):", length(changed)))
    for (i in changed) {
      tags <- def_tags(map, i)
      by <- caller_list(map, which(e$to_def == i & (is.na(e$from_def) | e$from_def != i)))
      out <- c(out, sprintf("  %s   %s   [%s]%s%s", def_signature(map, i), def_loc(map, i), how[[as.character(i)]],
                            if (nzchar(tags)) paste0("   ", tags) else "",
                            if (nzchar(by)) paste0("   called by: ", by) else "   no callers"))
    }
    sig <- changed[startsWith(how[as.character(changed)], "arguments were")]
    if (length(sig)) out <- c(out, "  A changed argument list: check every call site below passes arguments by the names and positions the new definition expects.")
  } else out <- c(out, "No definition changed (the hunks are outside every function).")
  if (length(deleted)) {
    out <- c(out, "", "Definitions no longer in the project:")
    for (o in deleted) {
      sites <- mention_sites(map, o$name)
      out <- c(out, sprintf("  %s(%s)   was %s:%d   %s", o$label, o$args, o$file, o$line,
                            if (length(sites)) paste0("still mentioned at: ", paste(sites, collapse = ", ")) else "no mention left"))
    }
  }
  if (length(top_level)) out <- c(out, "", "Top-level code changed:", top_level)
  if (length(unmapped)) out <- c(out, "", sprintf("Changed but not in the map (skipped or not parsed): %s", paste(unmapped, collapse = ", ")))
  if (length(changed)) {
    out <- c(out, "", sprintf("Callers and callees of the changed definitions (depth %d):", depth),
             callmap_neighbourhood(map, unique(d$label[changed]), depth, header = FALSE)[-1L],
             "", tests_section(map, changed, "the changed definitions"))
  }
  structure(out, ids = changed, changed = changed, how = how, deleted = deleted)
}

callmap_diff_json <- function(map, lines) {
  d <- map$defs
  changed <- attr(lines, "changed")
  how <- attr(lines, "how")
  reach <- callmap_tests(map, changed)
  list(
    changed = lapply(changed, function(i) list(name = d$label[[i]], file = d$file[[i]], line = d$line[[i]], end = d$end[[i]],
                                               args = d$args[[i]], change = how[[as.character(i)]])),
    deleted = lapply(unname(attr(lines, "deleted")), function(o) list(name = o$label, file = o$file, line = o$line, args = o$args,
                                                                      mentions = I(mention_sites(map, o$name)))),
    tests = lapply(names(reach), function(f) list(file = f, reaches = I(unique(vapply(reach[[f]], function(x) d$label[[x$id]], ""))),
                                                  command = test_command(map, f))),
    untested = I(d$label[setdiff(changed, unique(unlist(lapply(reach, function(l) vapply(l, function(x) x$id, 1L)))))]))
}
