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
# metadata for parquet/feather/excel/stata/spss/fst, an arrow dataset
# directory's schema. RDS files are loaded only when smaller than 20 MB. The
# reader's own arguments that change the names (col_select, col_names,
# header, id, skip, sheet, make.names repair, ...) are applied to the header
# (see "reader arguments"); one the checker cannot read stops tracking with
# a note. The moment a step makes the column set unknowable (across(),
# tidyselect helpers, pivot_wider(), clean_names(), joins to unknown frames,
# user functions, ...) the frame is marked unknown and nothing downstream is
# checked. Base-R access on a known frame (`df$x`, `df[["x"]]`, `df[, "x"]`)
# is checked like a verb, and `df$x <- v` adds the column. R Markdown, Quarto
# and Sweave files are checked chunk by chunk (parse_r_source()).
#
# Data the checker cannot open (S3 objects, databases, project loaders, a
# local file it cannot read) is covered by the schema cache,
# <root>/.colcheck/schemas.json, written by `r-colcheck learn KEY 'EXPR'`. A
# source call is matched to a cache entry by a string literal among its own
# arguments (the S3 key, URI or dataset name the code passes to its loader;
# a literal of a nested call belongs to that call), or by a `# cols: KEY`
# comment on the statement's line (any line of a multi-line call). A call
# whose literals name two cached entries is not guessed. A frame whose
# source stays unresolved and is then piped into verbs is reported as a
# note, with the learn command (its --source flags included), so the cache
# can be filled; nothing is reported for unresolved frames that are never
# used that way.
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

# --- literate files -----------------------------------------------------------
# R Markdown, Quarto and Sweave: the R code lives in chunks. knitr runs each
# chunk on its own, so each is parsed on its own, padded with blank lines so
# that every srcref carries the file's own line numbers; one chunk that does
# not parse costs that chunk, not the file.
is_literate_file <- function(path) grepl("[.](Rmd|rmd|Rmarkdown|qmd|Rnw|rnw)$", path)

# Body lines (1-based, inclusive) of every R chunk, as knitr delimits them:
# a fence of three or more backticks (or tildes) with {r ...} up to a fence
# of at least as many backticks on a line of its own, or, in Sweave, <<...>>=
# up to @. Leading blanks and `>` are allowed on fences as knitr allows them,
# and knitr strips that prefix from the chunk's lines (a chunk inside a block
# quote), so `prefix` says what to strip. A chunk of another engine
# ({python}, {sql}) is left out.
literate_r_chunks <- function(lines) {
  chunks <- list()
  i <- 1L
  n <- length(lines)
  while (i <= n) {
    m <- regmatches(lines[[i]], regexec("^[\t >]*(`{3,}|~{3,})[ \t]*\\{[ \t]*([A-Za-z0-9_]+)(?:[ \t,].*)?\\}[ \t]*$",
                                        lines[[i]], perl = TRUE))[[1L]]
    sweave <- !length(m) && grepl("^[ \t]*<<.*>>=", lines[[i]], perl = TRUE)
    if (!length(m) && !sweave) { i <- i + 1L; next }
    end_pat <- if (sweave) "^[ \t]*@[ \t]*(%.*)?$" else
      sprintf("^[\t >]*%s{%d,}[ \t]*$", substr(m[[2L]], 1L, 1L), nchar(m[[2L]]))
    j <- i + 1L
    while (j <= n && !grepl(end_pat, lines[[j]], perl = TRUE)) j <- j + 1L
    if (sweave || tolower(m[[3L]]) == "r") {
      chunks[[length(chunks) + 1L]] <- list(start = i + 1L, end = j - 1L, prefix = if (sweave) "" else "^[\t >]*")
    }
    i <- j + 1L
  }
  chunks
}

# parse() for a project file: an .R file as is; a literate file chunk by
# chunk. Returns list(exprs, lines, errors): exprs is an expression vector
# whose "srcref" attribute gives each top-level expression's lines in the
# file, lines the file's lines, errors a list of list(line, cond) for the
# chunks that do not parse (line is the chunk's first body line). An .R file
# that does not parse raises, as parse() does.
parse_r_source <- function(path) {
  if (!is_literate_file(path)) {
    ex <- parse(path, keep.source = TRUE, encoding = "UTF-8")
    return(list(exprs = ex, lines = attr(ex, "srcfile")$lines %||% readLines(path, warn = FALSE), errors = list()))
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  exprs <- list()
  srcrefs <- list()
  errors <- list()
  for (ch in literate_r_chunks(lines)) {
    if (ch$end < ch$start) next
    text <- rep("", ch$end)
    text[ch$start:ch$end] <- sub(ch$prefix, "", lines[ch$start:ch$end], perl = TRUE)
    ex <- tryCatch(parse(text = text, keep.source = TRUE, encoding = "UTF-8"), error = function(e) e)
    if (inherits(ex, "error")) {
      errors[[length(errors) + 1L]] <- list(line = ch$start, cond = ex)
      next
    }
    exprs <- c(exprs, as.list(ex))
    srcrefs <- c(srcrefs, attr(ex, "srcref"))
  }
  out <- as.expression(exprs)
  attr(out, "srcref") <- srcrefs
  list(exprs = out, lines = lines, errors = errors)
}

# The project a file belongs to: the nearest directory above it with a
# project marker, else the file's own directory. `within` (the hooks pass the
# session root) stops the walk there, so a file in the session with no nearer
# marker is checked from the session root, as before.
colcheck_root <- function(path, within = NULL) {
  dir <- normalizePath(dirname(path), mustWork = FALSE)
  markers <- c("DESCRIPTION", ".git", "renv.lock", "_targets.R", ".here")
  if (is.character(within) && length(within) == 1L && nzchar(within)) within <- normalizePath(within, mustWork = FALSE) else within <- NULL
  d <- dir
  while (nzchar(d) && d != dirname(d)) {
    if (any(file.exists(file.path(d, markers))) || length(list.files(d, pattern = "[.]Rproj$"))) return(d)
    if (identical(d, within)) return(d)
    d <- dirname(d)
  }
  dir
}

# --- schema cache: .colcheck/schemas.json -----------------------------------
# Column names observed from data the checker cannot open itself. Keyed by the
# string the code passes to its loader. Written by colcheck-learn.R.

schemas_path <- function(root) file.path(root, ".colcheck", "schemas.json")

# Project configuration, <root>/.colcheck.json, committed with the code
# (the cache directory is observations and stays local; this is policy):
#   {"assertions": [{"fn": "teamdata::require_columns", "frame": 1, "columns": 2, "exact": "exact"}],
#    "loaders": [{"fn": "s3_read", "key": 1, "learn": "arrow::open_dataset(sprintf('s3://bucket/%s/', KEY))",
#                 "source": ["R/loaders.R"], "load_all": false}],
#    "schema_ttl_days": 90}
# `assertions` names a function that stops unless its frame has the given
# columns; `frame` and `columns` are a positional index or an argument name,
# `exact` the name of the argument that makes the frame exactly those
# columns, or true. `loaders` describes the team's data loaders: `fn` (bare
# or pkg::fn), `key` the argument carrying the dataset key (a position
# among the unnamed arguments, or an argument name), `learn` an expression
# template that returns the table's schema cheaply, with KEY standing for
# the key as a quoted string (an arrow dataset on the same prefix reads the
# parquet footer only; absent, the loader call itself is evaluated), and
# `source`/`load_all` how `learn` evaluates it. A registered loader's cache
# key is that argument's literal (not any literal in the call), the
# unresolved-source note gives the derived learn command, and `r-colcheck
# discover` / `learn --all` cover the whole project. `schema_ttl_days` is
# the age after which a cached schema is called stale (90 by default).
load_config <- function(root) {
  p <- file.path(root, ".colcheck.json")
  if (!file.exists(p)) return(list())
  cfg <- quiet(jsonlite::fromJSON(p, simplifyVector = FALSE))
  if (!is.list(cfg)) list() else cfg
}

load_schemas <- function(root) {
  p <- schemas_path(root)
  if (!file.exists(p)) return(list())
  s <- quiet(jsonlite::fromJSON(p, simplifyVector = FALSE))
  if (!is.list(s) || is.null(names(s))) return(list())
  s
}

ttl_days <- function(cfg) {
  t <- suppressWarnings(as.integer(cfg$schema_ttl_days %||% 90L))
  if (length(t) != 1L || is.na(t) || t < 1L) 90L else t
}

# The registered loader a call is for, with the key literal its key argument
# carries: list(reg, key), or NULL.
loader_registry <- function(ctx) {
  regs <- ctx$config$loaders %||% list()
  Filter(function(r) is.list(r) && is.character(r$fn) && length(r$fn) == 1L && nzchar(r$fn), regs)
}

loader_key_arg <- function(e, which) {
  if (length(e) < 2L) return(NULL)
  # an empty argument (`f(x, )`) is the missing symbol, which no test below survives
  if (any(vapply(seq_along(e)[-1L], function(i) identical(e[[i]], quote(expr = )), TRUE))) return(NULL)
  nms <- names(e)
  nms <- if (is.null(nms)) rep("", length(e) - 1L) else nms[-1L]
  val <- NULL
  if (is.character(which) && length(which) == 1L && !grepl("^[0-9]+$", which)) {
    hit <- which(nms == which)
    if (length(hit)) val <- e[[hit[[1L]] + 1L]]
  } else {
    k <- suppressWarnings(as.integer(which))
    if (length(k) != 1L || is.na(k) || k < 1L) k <- 1L
    pos <- which(!nzchar(nms))
    if (length(pos) >= k) val <- e[[pos[[k]] + 1L]]
  }
  if (is.character(val) && length(val) == 1L) val else NULL
}

registered_loader <- function(e, ctx) {
  if (!is.call(e)) return(NULL)
  fn <- call_name(e)
  if (is.na(fn)) return(NULL)
  head <- e[[1L]]
  pkg <- if (is.call(head) && length(head) == 3L) as.character(head[[2L]]) else NULL
  for (r in loader_registry(ctx)) {
    rfn <- sub("^.*::", "", r$fn)
    rpkg <- if (grepl("::", r$fn, fixed = TRUE)) sub("::.*$", "", r$fn) else NULL
    if (rfn != fn) next
    if (!is.null(pkg) && !is.null(rpkg) && pkg != rpkg) next
    return(list(reg = r, key = loader_key_arg(e, r$key %||% 1L)))
  }
  NULL
}

# How `learn` evaluates a registered loader's key: the template with KEY
# replaced by the quoted key, and the flags the registry asks for.
registry_learn <- function(reg, key) {
  tmpl <- reg$learn
  expr <- if (is.character(tmpl) && length(tmpl) == 1L && nzchar(tmpl)) gsub("\\bKEY\\b", gsub("\\\\", "\\\\\\\\", deparse(key)), tmpl) else NULL
  sources <- as.character(unlist(reg$source))
  list(expr = expr, sources = sources, load_all = isTRUE(reg$load_all),
       extra = c(if (isTRUE(reg$load_all)) "--load-all", unlist(lapply(sources, function(s) c("--source", s)))))
}

# A command line to paste: the key in double quotes with its shell-special
# characters escaped, the expression in single quotes, paths quoted when they
# need it.
shell_dq <- function(x) paste0("\"", gsub("([\"$`\\\\])", "\\\\\\1", x), "\"")
learn_command <- function(key, expr, extra = character()) {
  extra <- vapply(extra, function(a) if (startsWith(a, "--") || grepl("^[A-Za-z0-9._/-]+$", a)) a else shell_dq(a), "")
  sprintf("r-colcheck learn %s '%s'%s", shell_dq(key), gsub("'", "'\\\\''", expr),
          if (length(extra)) paste0(" ", paste(extra, collapse = " ")) else "")
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
  inner <- list()
  for (cl in entry$columns) {
    if (is.list(cl) && is.list(cl$inner) && length(cl$inner)) {
      inn <- vapply(cl$inner, function(x) if (is.list(x)) as.character(x$name %||% "") else as.character(x), "")
      inn <- inn[nzchar(inn)]
      if (length(inn)) inner[[as.character(cl$name)]] <- inn
    }
  }
  if (length(inner)) attr(cols, "inner") <- inner
  attr(cols, "source") <- sprintf("cached schema \"%s\"%s", key,
                                  if (nzchar(observed)) paste0(", observed ", observed) else "")
  age <- quiet(as.integer(Sys.Date() - as.Date(observed)))
  if (!is.null(age) && !is.na(age) && age > ttl_days(ctx$config) && is.null(ctx$stale[[key]])) {
    ctx$stale[[key]] <- list(line = ctx$line, days = age, observed = observed)
  }
  cols
}

# A literal inside a lambda (`~ .x %>% filter(status == "open")`, a
# `function()` argument) belongs to that body, never to the call around it.
string_literals <- function(e) {
  if (is.character(e)) return(e)
  if (!is.call(e)) return(character())
  if (call_name(e) %in% c("~", "function")) return(character())
  unlist(lapply(call_args(e), string_literals))
}

# The literals a call can be keyed on: its own string arguments, and those
# of a path built in place (`file.path("s3://b", "k")`, `paste0(prefix,
# "k")`). A literal that belongs to a nested call is that call's key, never
# the wrapper's: `clean(s3_read("k"))` is matched when the walk reaches
# s3_read("k"), and clean() is then followed with that result.
path_builders <- c("file.path", "here", "paste0", "paste", "sprintf", "glue")
option_args <- c("na", "sep", "delim", "format", "mode", "encoding", "quote", "dec", "comment", "method",
                 "engine", "compression", "locale", "tz", "col_types", "colClasses")
key_literals <- function(e) {
  if (!is.call(e)) return(character())
  args <- call_args(e)
  args <- args[!((names(args) %||% rep("", length(args))) %in% option_args)]   # `mode = "p"` names no dataset
  unique(unlist(lapply(args, function(a) {
    if (is.character(a)) a
    else if (is.call(a) && call_name(a) %in% path_builders) c(builder_value(a), string_literals(a))
    else NULL
  })))
}

# The string a path builder of literal parts makes, `paste0("s3://b/", "k")`
# -> "s3://b/k": the key the learn command should carry. NULL when a part
# is not a literal.
builder_value <- function(a) {
  fn <- call_name(a)
  if (!(fn %in% c("file.path", "paste0", "paste", "sprintf")) || length(all.vars(a)) ||
      !all(all.names(a) %in% c(fn, "c"))) return(NULL)
  v <- quiet(eval(a, baseenv()))
  if (is.character(v) && length(v) == 1L && nzchar(v)) v else NULL
}

# Column set for a source call the checker could not open: a cache entry for
# a string literal of the call (key_literals), else the `# cols: KEY`
# annotation of the assignment being walked (consumed by the first
# unresolved source, which is the head of the pipeline). A call whose
# literals match two different entries (`join_keys("a", "b")`) is not
# guessed from the first: it falls through to the annotation and the
# call-shaped keys, and otherwise stays unresolved with a note.
cached_columns <- function(e, ctx) {
  if (length(ctx$schemas)) {
    # a registered loader is keyed on its key argument, not on any literal
    reg <- registered_loader(e, ctx)
    if (!is.null(reg) && !is.null(reg$key)) {
      cols <- schema_columns(reg$key, ctx)
      if (!is.null(cols)) return(cols)
    }
    hits <- list()
    for (s in key_literals(e)) {
      cols <- schema_columns(s, ctx)
      if (!is.null(cols)) hits[[s]] <- cols
    }
    if (length(hits) == 1L) return(hits[[1L]])
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
  # and no comment on any call line. A call that does pass a literal
  # (`s3_read("meta/funds")`, with "meta/funds" not learned) is keyed on
  # that literal or on a call-shaped key, never on the bare name: the bare
  # entry describes calls without a key, and the note below asks for the
  # literal's own entry.
  if (length(ctx$schemas)) {
    keyed <- length(key_literals(e)) > 0L
    for (k in function_keys(e, ctx, bare = !keyed)) {
      cols <- schema_columns(k, ctx)
      if (!is.null(cols)) return(cols)
    }
  }
  NULL
}

# The cache keys a call matches by its function: `pkg::fn` and `fn` for a
# namespaced call; for a bare call, `fn` and any `<pkg>::fn` entry, unless
# the project defines fn itself (its own default_rates() is not
# teamdata::default_rates()). A key shaped like a call with arguments,
# `fn(type = "monthly")`, comes first; `bare = FALSE` leaves those only.
function_keys <- function(e, ctx, bare = TRUE) {
  fn <- call_name(e)
  if (is.na(fn)) return(character())
  head <- e[[1L]]
  by_name <- if (!bare) character()
    else if (is.call(head)) c(paste0(as.character(head[[2L]]), "::", fn), fn)
    else if (!is.null(ctx$functions[[fn]])) fn
    else c(fn, grep(paste0("^[A-Za-z][A-Za-z0-9.]*::", gsub(".", "[.]", fn, fixed = TRUE), "$"), names(ctx$schemas), value = TRUE))
  c(call_keys(e, fn, ctx), by_name)
}

# Keys shaped like a call, `prepare(type = "monthly")`: one entry per argument
# value of a helper whose output depends on it. The key matches a call to
# the same function (the package, when both name one, must agree) when every
# argument the key carries appears in the call: a named one under the same
# name, a positional one anywhere among the call's arguments, since a piped
# call carries the data unseen. Keys with more arguments come first.
call_keys <- function(e, fn, ctx) {
  keys <- grep("(", names(ctx$schemas), fixed = TRUE, value = TRUE)
  if (!length(keys)) return(character())
  head <- e[[1L]]
  args <- call_args(e)
  nms <- names(args) %||% rep("", length(args))
  hits <- character()
  width <- integer()
  named <- integer()
  for (k in keys) {
    kc <- ctx$key_calls[[k]]
    if (is.null(kc)) {
      kc <- quiet(str2lang(k), NA)
      ctx$key_calls[[k]] <- kc
    }
    if (!is.call(kc) || !identical(call_name(kc), fn)) next
    kh <- kc[[1L]]
    if (is.call(kh) && is.call(head) && !identical(as.character(kh[[2L]]), as.character(head[[2L]]))) next
    ka <- call_args(kc)
    knms <- names(ka) %||% rep("", length(ka))
    ok <- TRUE
    for (i in seq_along(ka)) {
      found <- if (nzchar(knms[[i]])) knms[[i]] %in% nms && same_arg(args[[knms[[i]]]], ka[[i]])
        else any(vapply(args, same_arg, TRUE, ka[[i]]))
      if (!found) { ok <- FALSE; break }
    }
    if (ok) { hits <- c(hits, k); width <- c(width, length(ka)); named <- c(named, sum(nzchar(knms))) }
  }
  # the widest key first; among keys of one width, the one matched by name
  hits[order(-width, -named)]
}

# Argument values compared as the code means them: 2L and 2 are the same
# literal (the key is parsed from text, the code may carry the integer
# suffix), inside c() and unary minus too; anything else as identical().
same_arg <- function(a, b) {
  if (is.numeric(a) && is.numeric(b)) return(length(a) == length(b) && isTRUE(all(as.numeric(a) == as.numeric(b))))
  if (is.call(a) && is.call(b) && length(a) == length(b)) return(all(mapply(same_arg, as.list(a), as.list(b))))
  identical(a, b)
}

# `# cols: KEY` on the statement's first line, or, for a call broken over
# several lines, on any line up to its last; the tag in any case.
line_annotation <- function(ctx) {
  if (is.na(ctx$line) || ctx$line > length(ctx$src_lines)) return(NULL)
  to <- ctx$line_end
  if (is.null(to) || is.na(to) || to < ctx$line) to <- ctx$line
  for (ln in seq.int(ctx$line, min(to, length(ctx$src_lines)))) {
    m <- regmatches(ctx$src_lines[[ln]],
                    regexec("#\\s*(?i:cols?|colcheck|schema)\\s*:\\s*(\"[^\"]+\"|'[^']+'|\\S+)", ctx$src_lines[[ln]], perl = TRUE))[[1L]]
    if (length(m) >= 2L) return(gsub("^[\"']|[\"']$", "", m[[2L]]))
  }
  NULL
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

# The step at which a known frame became unknown: the first one in the
# statement, with why. The note for the statement names this step instead of
# walking back to the pipeline head and blaming a source that resolved.
nesting_verbs <- c("unnest", "unnest_wider", "unnest_longer", "hoist")
one_line <- function(e) paste(trimws(deparse(e, width.cutoff = 500L)), collapse = " ")
note_stop <- function(ctx, call, fn, def, reason = NULL) {
  if (!is.null(ctx$stopped) || !is.null(ctx$pkg_scope)) return(invisible())
  if (fn %in% c("ggplot", "+")) return(invisible())   # a plot is not a frame; nothing was lost
  if (is.null(reason)) reason <- if (!is.null(def) && !is.null(def$pkg)) "its code could not be read"
    else if (fn %in% nesting_verbs) "the columns inside the list-column it expands are not recorded; re-run r-colcheck learn for its source, since entries learned before 1.7.13 carry no inner columns"
    else "its output column names cannot be worked out statically"
  ctx$stopped <- list(fn = fn, expr = one_line(call), line = ctx$line, file = ctx$file, reason = reason,
                      key = if (is.null(def)) NULL else if (!is.null(def$pkg)) def$file else fn)
  invisible()
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
  # the source resolved and a later step lost the columns: say which step
  if (!is.null(ctx$stopped)) {
    # used already when the pipeline went on past the stop in this statement
    last_step <- if (is.call(rhs) && call_name(rhs) %in% c("%>%", "%<>%", "%T>%")) rhs[[3L]] else rhs
    if (is.symbol(last_step)) last_step <- as.call(list(last_step))
    went_on <- !identical(paste(deparse(last_step), collapse = " "), ctx$stopped$expr)
    ctx$unresolved[[target]] <- list(line = ctx$line, keys = character(), stopped = ctx$stopped, used = went_on)
    return(invisible())
  }
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
    # a function from a public package (CRAN, Bioconductor, base) builds its
    # result at run time and no cache key the user could learn describes it;
    # the note is for a team's own package, where a fixed output is likely
    if (!is.null(head_def) && !is.null(head_def$pkg) && !public_package(head_def$pkg, ctx)) {
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
  literals <- key_literals(head)
  learn <- NULL
  why <- NULL
  if (fn %in% readers) {
    # a reader with a literal path that does not exist locally (an S3 URI);
    # read.csv(text = x) or read_csv(path_variable) are not learnable sources
    path_arg <- if (length(call_args(head))) call_args(head)[[1L]] else NULL
    if (!is.character(path_arg) && !(is.call(path_arg) && call_name(path_arg) %in% c("here", "file.path"))) return(invisible())
    why <- unreadable_reason(resolve_path(path_arg, ctx), fn)
    # parquet, feather or an arrow dataset on a bucket: the schema is in the
    # file footer, which arrow::open_dataset() reads without the rows
    if (is.character(path_arg) && grepl("^(s3|gs|az|abfs|hdfs|https?)://", path_arg) &&
        (fn %in% c("read_parquet", "read_feather", "open_dataset") || grepl("[.](parquet|feather|arrow)$", path_arg) || !grepl("[.][A-Za-z0-9]+$", path_arg))) {
      learn <- list(expr = sprintf("arrow::open_dataset(%s)", deparse(path_arg)), extra = character(), why = "that expression reads the schema without the rows")
    }
  } else {
    reg <- registered_loader(head, ctx)
    if (!is.null(reg)) {
      if (!is.null(reg$key)) key <- c(reg$key, key)
      learn <- registry_learn(reg$reg, reg$key %||% "")
      if (is.null(reg$key) && !is.null(learn$expr)) learn$expr <- NULL
      learn$why <- "from the loader registry in .colcheck.json"
    } else if (!is.null(head_def) && is.null(head_def$pkg)) {
      # a project function: `learn` evaluates the expression in a fresh
      # session, so the command must bring the loader's definition along
      learn <- list(extra = loader_flags(head_def, ctx))
    }
    if (is.null(key) && !length(literals)) return(package_note())
  }
  ctx$unresolved[[target]] <- list(
    line = ctx$line, keys = unique(c(key, literals)), expr = paste(deparse(head), collapse = " "),
    used = !identical(head, rhs), learn = learn, why = why)
  invisible()
}

# Why a path that is there could not be read: said in the note, so that the
# remedy is not a guess. NULL when the path is not there (nothing to add).
unreadable_reason <- function(path, fn) {
  if (is.null(path)) return(NULL)
  if (dir.exists(path)) return(sprintf("%s is a directory%s", basename(path),
                                       if (fn == "open_dataset") ", which needs the arrow package" else ""))
  ext <- tolower(tools::file_ext(path))
  if (ext == "rds" || fn %in% c("readRDS", "read_rds")) {
    mb <- file.size(path) / 1024^2
    if (!is.na(mb) && mb > 20) return(sprintf("%s is an RDS file of %.0f MB, over the 20 MB the checker loads", basename(path), mb))
  }
  pkg <- switch(ext, xlsx = "readxl", xls = "readxl", dta = "haven", sav = "haven", fst = "fst",
                parquet = "arrow", feather = "arrow", arrow = "arrow", NULL)
  if (!is.null(pkg) && !requireNamespace(pkg, quietly = TRUE)) return(sprintf("reading %s needs the %s package, which is not installed", basename(path), pkg))
  sprintf("%s is there but its column names could not be read", basename(path))
}

# The flags that make a project loader's definition available to `learn`:
# --load-all in a package when the loader lives under R/, else --source for
# the file that defines it, the files defining the project functions its
# body calls, and the files those source().
loader_flags <- function(def, ctx) {
  files <- def$file
  for (nm in intersect(unique(all.names(def$fn)), names(ctx$functions))) files <- c(files, ctx$functions[[nm]]$file)
  files <- unique(files[vapply(files, function(f) is.character(f) && length(f) == 1L && file.exists(f), TRUE)])
  if (!length(files)) return(character())
  root <- normalizePath(ctx$root, mustWork = FALSE)
  if (file.exists(file.path(root, "DESCRIPTION")) &&
      any(startsWith(normalizePath(files, mustWork = FALSE), paste0(file.path(root, "R"), "/")))) return("--load-all")
  for (f in files) {
    ex <- quiet(parse(f, keep.source = FALSE))
    if (!is.null(ex)) for (e in ex) for (p in source_calls(e)) {
      src <- resolve_path(p, ctx)
      if (!is.null(src)) files <- c(files, src)
    }
  }
  unlist(lapply(unique(files), function(f) c("--source", relative_path(f, ctx$root))))
}

# Calls that combine or wrap frames rather than load them.
# Row binding: the result has the union of its inputs' columns, in order of
# first appearance, with the first input's grouping and source. bind_rows()
# and rbind() take frames; list_rbind() and rbindlist() a list, which is
# followed when it is a literal `list(a, b)` (a list held in a variable is
# not a frame and stays unknown). An unknown input makes the result unknown,
# since it may carry columns the others lack. bind_cols()/cbind() are not
# here: they concatenate columns and rename duplicates.
combining_calls <- c("bind_rows", "rbind", "list_rbind", "rbindlist")

# Control arguments of the dplyr/tidyr verbs, which never name a column.
control_args <- c(".by", ".keep", ".before", ".after", ".groups", ".add", ".drop", ".preserve", ".unpack",
                  ".locale", ".by_group", ".keep_all", ".name_repair", ".id", ".default", ".fn", ".cols",
                  ".names", ".fns", ".ptype", ".transform", ".remove", ".simplify", ".strict", ".sep",
                  ".direction", ".data", ".env", ".order", ".method", ".fill", ".value", ".seed", ".weight",
                  ".col", ".key", ".names_sep", ".names_repair", ".fill_value", ".funs", ".vars", ".predicate")
id_args <- c(bind_rows = ".id", list_rbind = "names_to", rbindlist = "idcol")

# `partial`: the first input was already found unknown (the piped frame).
bound_rows <- function(call, first, ctx, partial = FALSE) {
  fn <- call_name(call)
  args <- call_args(call)
  nms <- names(args) %||% rep("", length(args))
  id_arg <- id_args[fn]
  id <- NULL
  if (!is.na(id_arg) && id_arg %in% nms) {
    v <- args[[id_arg]]
    id <- if (is.character(v) && length(v) == 1L) v else if (isTRUE(v) && fn == "rbindlist") ".id" else NULL
    if (is.null(id)) return(NULL)   # a computed id column name
  }
  inputs <- args[!(nms %in% id_args)]
  parts <- if (is.null(first)) list() else list(first)
  # an unknown input leaves the result partly known: at least the union of
  # the inputs that are known
  for (a in inputs) {
    cols <- input_frame(a, ctx)
    if (is.null(cols)) { partial <- TRUE; next }
    parts <- c(parts, list(cols))
  }
  if (!length(parts)) return(NULL)
  out <- unique(c(id, unlist(lapply(parts, as.character))))
  out <- keep_source(out, parts[[1L]])
  attr(out, "groups") <- attr(parts[[1L]], "groups") %||% character()
  if (partial || any(vapply(parts, is_open, TRUE))) attr(out, "open") <- TRUE
  out
}

# One input of a row binding: a frame expression, or a literal list of them.
input_frame <- function(a, ctx) {
  if (is.call(a) && identical(call_name(a), "list")) {
    els <- call_args(a)
    if (!length(els)) return(NULL)
    parts <- lapply(els, function(el) as_frame(frame_columns(el, ctx, used = TRUE)))
    if (any(vapply(parts, is.null, TRUE))) return(NULL)
    out <- keep_source(unique(unlist(lapply(parts, as.character))), parts[[1L]])
    attr(out, "groups") <- attr(parts[[1L]], "groups") %||% character()
    if (any(vapply(parts, is_open, TRUE))) attr(out, "open") <- TRUE
    return(out)
  }
  cols <- as_frame(frame_columns(a, ctx, used = TRUE))
  if (is.null(cols)) NULL else unlisted(cols)
}

# --- lists of frames: split() and the mappers -------------------------------
# split(df, df$g) / group_split() turn a frame into a list of frames with the
# same columns (a column set carrying a `listed` attribute); map(), imap(),
# lapply() over it walk the lambda once with its parameter bound to that
# frame, so the body is checked at its own lines, and give a list of the
# body's result columns; map_dfr()/map_df() give that frame, and bind_rows()
# or list_rbind() of the list (input_frame) does the same. A listed set is
# not a frame anywhere a frame is consumed: verbs and joins give up on it.
listed_calls <- c("split", "group_split")
mapping_calls <- c("map", "imap", "lapply", "map_dfr", "map_df")
is_listed <- function(cols) isTRUE(attr(cols, "listed"))
listed <- function(cols) { attr(cols, "listed") <- TRUE; cols }
unlisted <- function(cols) { attr(cols, "listed") <- NULL; cols }

split_frames <- function(call, cols, ctx, piped) {
  fn <- call_name(call)
  args <- call_args(call)
  data_expr <- if (piped) as.name(".") else if (length(args)) args[[1L]] else NULL
  if (!piped && length(args)) args <- args[-1L]
  nms <- names(args) %||% rep("", length(args))
  cols <- unlisted(cols)
  if (fn == "split") {
    f <- if ("f" %in% nms) args[["f"]] else if (any(!nzchar(nms))) args[!nzchar(nms)][[1L]] else NULL
    check_symbols(dollar_columns(f, data_expr), cols, ctx)
    return(listed(keep_source(structure(as.character(cols), groups = attr(cols, "groups") %||% character()), cols)))
  }
  pos <- args[!nzchar(nms)]
  g <- if (length(pos)) grouping_names(pos, ctx, cols) else attr(cols, "groups") %||% character()
  if (is.null(g) || anyNA(g)) return(NULL)
  if (length(pos)) check_symbols(g, cols, ctx)
  keep <- !identical(args[[".keep"]], FALSE)
  out <- if (keep) as.character(cols) else setdiff(as.character(cols), g)
  listed(keep_source(structure(out, groups = character()), cols))
}

# The column names a split() factor names on the data itself: `.$g`,
# `df$g`, `list(df$a, df$b)`, `~ g`.
dollar_columns <- function(f, data_expr) {
  if (!is.call(f)) return(character())
  fn <- call_name(f)
  if (identical(fn, "$") && length(f) == 3L && is.symbol(f[[3L]])) {
    base <- f[[2L]]
    own <- is.symbol(base) && (identical(as.character(base), ".") || (!is.null(data_expr) && identical(base, data_expr)))
    return(if (own) as.character(f[[3L]]) else character())
  }
  if (identical(fn, "~") && length(f) == 2L) return(all.vars(f))
  if (fn %in% c("list", "c", "interaction")) return(unlist(lapply(call_args(f), dollar_columns, data_expr = data_expr)))
  character()
}

map_over <- function(call, cols, ctx, piped) {
  fn <- call_name(call)
  args <- call_args(call)
  if (!piped && length(args)) args <- args[-1L]
  nms <- names(args) %||% rep("", length(args))
  f <- if (".f" %in% nms) args[[".f"]] else if ("FUN" %in% nms) args[["FUN"]] else if (any(!nzchar(nms))) args[!nzchar(nms)][[1L]] else NULL
  if (is.null(f)) return(NULL)
  extra <- args[!(nms %in% c(".f", "FUN", ".x", "X", ".l"))]
  if (any(!nzchar(nms))) extra <- extra[-match(TRUE, !nzchar(names(extra) %||% rep("", length(extra))))]
  result <- lambda_columns(f, unlisted(cols), ctx, with_index = fn == "imap", extra = extra, shown = call)
  if (is.null(result)) return(NULL)
  if (fn %in% c("map_dfr", "map_df")) unlisted(result) else listed(result)
}

# The columns a lambda returns for one element frame: a formula (`.x`, `.`,
# `..1` bound), a function literal (its first formal, the second for imap),
# or a named function followed like any project or package function.
lambda_columns <- function(f, frame, ctx, with_index = FALSE, extra = list(), shown = f) {
  if (is.symbol(f) || (is.call(f) && !is.na(ns_name(f)))) {
    ctx$frames[[".colcheck_el"]] <- frame
    on.exit(ctx$frames[[".colcheck_el"]] <- NULL, add = TRUE)
    call <- as.call(c(list(f, as.name(".colcheck_el")), extra))
    fn <- call_name(call)
    def <- lookup_function(call, fn, ctx)
    if (is.null(def)) return(NULL)
    return(as_frame(call_user_function(call, fn, ctx, def = def, shown = f)))
  }
  if (!is.call(f)) return(NULL)
  fn <- call_name(f)
  params <- character()
  body <- NULL
  if (identical(fn, "~") && length(f) == 2L) {
    params <- c(".x", ".", "..1")
    body <- f[[2L]]
  } else if (identical(fn, "function") && length(f) >= 3L) {
    formals_ <- names(f[[2L]]) %||% character()
    if (!length(formals_)) return(NULL)
    params <- formals_[[1L]]
    body <- f[[3L]]
  } else return(NULL)
  saved <- list(frames = ctx$frames, variables = ctx$variables, unknown = ctx$unknown_frames,
                strings = ctx$strings, in_function = ctx$in_function, stopped = ctx$stopped,
                annotation = ctx$annotation, annotation_head = ctx$annotation_head)
  for (p in params) ctx$frames[[p]] <- frame
  ctx$variables <- union(ctx$variables, c(params, ".y", ".i"))
  if (with_index && identical(fn, "function") && length(names(f[[2L]])) >= 2L) ctx$variables <- union(ctx$variables, names(f[[2L]])[[2L]])
  ctx$in_function <- TRUE
  result <- as_frame(body_columns(body, ctx))
  inner <- if (is.null(saved$stopped)) ctx$stopped else NULL
  ctx$frames <- saved$frames
  ctx$variables <- saved$variables
  ctx$unknown_frames <- saved$unknown
  ctx$strings <- saved$strings
  ctx$in_function <- saved$in_function
  ctx$stopped <- saved$stopped
  ctx$annotation <- saved$annotation          # the statement's `# cols:` survives the body walk
  ctx$annotation_head <- saved$annotation_head
  if (is.null(result) && !is.null(inner)) {
    note_stop(ctx, shown, call_name(shown) %||% "lambda", NULL, reason = sprintf("inside its lambda at %s, %s", inner$expr, inner$reason))
  }
  result
}

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

# Installed from CRAN or Bioconductor, or shipped with R: the DESCRIPTION
# carries Repository: CRAN (written at publication), biocViews, or
# Priority: base. A team's own package, built from source or an internal
# repository, has none of these.
public_package <- function(pkg, ctx) {
  memo <- ctx$public_pkgs[[pkg]]
  if (!is.null(memo)) return(memo)
  d <- tryCatch(suppressWarnings(utils::packageDescription(pkg)), error = function(e) NULL)
  res <- is.list(d) && (identical(d$Repository, "CRAN") || !is.null(d$biocViews) || identical(d$Priority, "base"))
  ctx$public_pkgs[[pkg]] <- res
  res
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

call_user_function <- function(e, name, ctx, def = lookup_function(e, name, ctx), shown = e) {
  if (is.null(def) || def$file %in% ctx$call_stack || length(ctx$call_stack) >= 4L) return(NULL)
  if (isTRUE(ctx$in_negative_test)) return(NULL)
  saved <- list(frames = ctx$frames, variables = ctx$variables, unknown = ctx$unknown_frames,
                line = ctx$line, line_end = ctx$line_end, file = ctx$file, call_stack = ctx$call_stack,
                call_sites = ctx$call_sites, in_function = ctx$in_function, pkg_scope = ctx$pkg_scope,
                stopped = ctx$stopped, strings = ctx$strings,
                annotation = ctx$annotation, annotation_head = ctx$annotation_head)
  errors_before <- length(ctx$errors)
  result <- guarded(ctx, def$file, follow_function(e, def, ctx))
  # the step inside the body that lost the columns, if the body recorded one
  inner <- if (is.null(saved$stopped)) ctx$stopped else NULL
  known_in <- isTRUE(ctx$follow_known[length(ctx$follow_known)])
  ctx$follow_known <- ctx$follow_known[-length(ctx$follow_known)]
  ctx$stopped <- saved$stopped
  ctx$strings <- saved$strings
  ctx$annotation <- saved$annotation          # the call line's `# cols:` survives the body walk
  ctx$annotation_head <- saved$annotation_head
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
  # A project function that gave no column set for a frame that went in: a
  # stop at the call site, naming the step inside the body where the columns
  # were lost. Direct calls had no note at all, and the piped note named the
  # helper without saying where inside it tracking ended. A package function
  # keeps its own note (the learn command for a fixed output).
  if (is.null(result) && is.null(def$pkg) && length(ctx$errors) == errors_before && (!is.null(inner) || known_in)) {
    reason <- if (is.null(inner)) "following its code gave no column set" else sprintf(
      "inside %s() at its %s%s, %s", name, inner$expr,
      # a body without braces never sets a line of its own: show none
      if (identical(inner$line, saved$line)) "" else sprintf(" (%s:%s)", relative_path(inner$file, ctx$root), inner$line),
      inner$reason)
    note_stop(ctx, shown, name, def, reason = reason)
  }
  result
}

follow_function <- function(e, def, ctx) {
  ctx$follow_known <- c(ctx$follow_known, FALSE)   # did a known frame go in? (popped by the caller)
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
  arg_strings <- list()
  for (p in names(supplied)) {
    a <- supplied[[p]]
    if (is.character(a)) { arg_strings[[p]] <- a; next }
    if (!(is.symbol(a) || is.call(a))) next
    arg_strings[[p]] <- string_vector(a, ctx)
    arg_cols[[p]] <- as_frame(frame_columns(a, ctx, used = param_used_as_frame(fdef[[3L]], p)))
  }
  # `if ("x" %in% names(df))`: the body handles frames with and without x,
  # so x counts as present for the parameter rather than reported
  guarded <- guarded_columns(fdef[[3L]])
  ctx$follow_known[length(ctx$follow_known)] <- any(!vapply(arg_cols, is.null, TRUE))
  for (p in params) ctx$strings[[p]] <- arg_strings[[p]]
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

# The header line of a delimited file: `sep` NULL to guess it, "" for
# whitespace (read.table's default); `skip` lines and `comment` lines
# before it are passed over as the readers do.
header_delim <- function(path, sep = NULL, skip = 0L, comment = "") {
  if (!identical(sep, "") && requireNamespace("readr", quietly = TRUE)) {
    df <- quiet(if (is.null(sep)) readr::read_csv(path, n_max = 0, skip = skip, comment = comment, show_col_types = FALSE, progress = FALSE)
                else readr::read_delim(path, delim = sep, n_max = 0, skip = skip, comment = comment, show_col_types = FALSE, progress = FALSE))
    if (!is.null(df)) return(names(df))
  }
  lines <- quiet(readLines(path, n = skip + 200L, warn = FALSE))
  if (is.null(lines) || length(lines) <= skip) return(NULL)
  if (skip > 0L) lines <- lines[-seq_len(skip)]
  if (nzchar(comment)) lines <- lines[!startsWith(trimws(lines), comment)]
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(NULL)
  line <- lines[[1L]]
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

# `opts`: the reader arguments that decide where the header is (sep, skip,
# comment; sheet, range for a workbook), as reader_columns() read them.
columns_of_file <- function(path, reader, opts = list()) {
  if (!file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  if (dir.exists(path)) {
    # a directory of parquet/feather files, as arrow::open_dataset() reads
    # it: the schema comes from the footers, hive partition columns included
    if (reader != "open_dataset" || !requireNamespace("arrow", quietly = TRUE)) return(NULL)
    sch <- quiet(arrow::open_dataset(path)$schema)
    return(if (is.null(sch)) NULL else names(sch))
  }
  delim_readers <- c("read_csv", "read.csv", "fread", "vroom", "read_delim", "read.delim", "read_tsv",
                     "read_csv2", "read.csv2", "read.table")
  if (reader %in% delim_readers || ext %in% c("csv", "tsv", "txt", "dat")) {
    sep <- opts$sep %||% switch(reader, read_tsv = "\t", read.delim = "\t", read_csv2 = ";", read.csv2 = ";", read.table = "", NULL)
    return(header_delim(path, sep, skip = opts$skip %||% 0L, comment = opts$comment %||% ""))
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
    df <- quiet(do.call(readxl::read_excel, c(list(path, n_max = 0), opts[intersect(names(opts), c("sheet", "skip", "range"))])))
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

# --- reader arguments --------------------------------------------------------
# The arguments of a reader call that change the names it gives, applied to
# the file's header: col_select/select/drop/cols_only() keep or drop
# columns, col_names/header/col.names replace the header (FALSE: generated
# names, X1..Xn for readr and vroom, V1..Vn for read.csv and fread, ...1..n
# for readxl; a literal vector: those names), id= adds a column in front,
# read.csv() and friends repair names with make.names() unless check.names
# = FALSE, colClasses = "NULL" and row.names = 1 drop columns, and skip=,
# comment=, sep=, sheet= and range= decide which line is the header. A
# name-changing argument the checker cannot read (a variable, a computed
# selection) gives NULL with a note at the line, so the steps after it
# count as not checked rather than checked against names the reader does
# not produce. Type specifications (col_types, colClasses) only matter when
# they are literal; a variable holding one is taken to keep the names.
reader_pkgs <- c(read_csv = "readr", read_csv2 = "readr", read_tsv = "readr", read_delim = "readr", read_rds = "readr",
                 vroom = "vroom", read.csv = "utils", read.csv2 = "utils", read.delim = "utils", read.table = "utils",
                 fread = "data.table", read_parquet = "arrow", read_feather = "arrow", open_dataset = "arrow",
                 read_excel = "readxl", read_xlsx = "readxl", read_dta = "haven", read_sav = "haven", readRDS = "base")
reader_families <- list(readr = c("read_csv", "read_csv2", "read_tsv", "read_delim", "vroom"),
                        base = c("read.csv", "read.csv2", "read.delim", "read.table"), fread = "fread",
                        arrow = c("read_parquet", "read_feather"), excel = c("read_excel", "read_xlsx"),
                        haven = c("read_dta", "read_sav"))
reader_family <- function(fn) {
  for (f in names(reader_families)) if (fn %in% reader_families[[f]]) return(f)
  NA_character_
}

# The call's arguments under the reader's own argument names (positional and
# partially matched ones included) when the reader's package is installed;
# the named ones as written otherwise.
reader_args <- function(e, fn) {
  args <- call_args(e)
  args <- args[!vapply(args, function(a) is.symbol(a) && identical(as.character(a), "..."), TRUE)]
  nms <- names(args) %||% rep("", length(args))
  pkg <- reader_pkgs[fn]
  if (!is.na(pkg) && quiet(requireNamespace(pkg, quietly = TRUE), FALSE)) {
    def <- quiet(get0(fn, envir = asNamespace(pkg), inherits = FALSE))
    m <- if (is.function(def)) quiet(match.call(def, as.call(c(e[[1L]], args)))) else NULL
    if (!is.null(m)) {
      out <- as.list(m)[-1L]
      return(out[nzchar(names(out) %||% rep("", length(out)))])
    }
  }
  args[nzchar(nms)]
}

# A numeric vector written literally: 2, 1:3, c(1, 3), -1.
numeric_literal <- function(v) {
  if (is.numeric(v)) return(v)
  if (is.call(v) && !length(all.vars(v)) && all(all.names(v) %in% c("c", ":", "-", "+", "("))) {
    val <- quiet(eval(v, baseenv()))
    return(if (is.numeric(val)) val else NULL)
  }
  NULL
}

# One tidyselect expression (a reader's col_select) applied to the header:
# the names kept, in the reader's order; NULL when it cannot be read. A
# name that is not a column is reported at the line, as in select().
reader_selection <- function(sel, cols, ctx) {
  parts <- if (is.call(sel) && identical(call_name(sel), "c")) call_args(sel) else list(sel)
  nm <- selection_names(parts, ctx, cols)
  if (is.null(nm)) return(NULL)
  check_symbols(nm, cols, ctx)
  neg <- any(vapply(parts, function(p) is.call(p) && call_name(p) %in% c("-", "!"), TRUE))
  if (neg) setdiff(as.character(cols), nm) else unique(nm)
}

# fread's select=/drop=: names or positions, written literally.
fread_picks <- function(v, cols, ctx) {
  idx <- numeric_literal(v)
  if (!is.null(idx)) return(as.character(cols)[idx[idx >= 1 & idx <= length(cols)]])
  nm <- string_vector(v, ctx)
  if (is.null(nm)) return(NULL)
  check_symbols(nm, cols, ctx)
  nm
}

# The column names a reader call gives for the file at `path`, its
# arguments applied (see above); NULL when the file cannot be read or an
# argument cannot be.
reader_columns <- function(path, e, fn, ctx) {
  args <- reader_args(e, fn)
  fam <- reader_family(fn)
  arg <- function(nm) if (nm %in% names(args)) args[[nm]] else NULL
  give_up <- function(nm) {
    note_stop(ctx, e, fn, NULL, reason = sprintf("its %s argument cannot be read statically", nm))
    structure(character(), gave_up = TRUE)   # not NULL: the cache is no answer to an unreadable argument
  }
  literal_flag <- function(v, default) if (is.null(v)) default else if (isTRUE(v) || isFALSE(v)) v else NA
  # where the header is: forwarded to the file reader
  opts <- list()
  skip <- arg("skip")
  if (!is.null(skip)) {
    n <- numeric_literal(skip)
    if (length(n) != 1L || is.na(n)) return(give_up("skip"))
    opts$skip <- as.integer(n)
  }
  for (nm in c("sep", "delim", "comment", "comment.char")) {
    v <- arg(nm)
    if (is.character(v) && length(v) == 1L) opts[[if (nm %in% c("sep", "delim")) "sep" else "comment"]] <- v
  }
  if (identical(fam, "excel")) {
    for (nm in c("sheet", "range")) {
      v <- arg(nm)
      if (is.null(v)) next
      if ((is.character(v) || (nm == "sheet" && is.numeric(v))) && length(v) == 1L) opts[[nm]] <- v else return(give_up(nm))
    }
  }
  cols <- columns_of_file(path, fn, opts)
  if (is.null(cols)) return(NULL)
  attr(cols, "source") <- basename(path)
  if (is.na(fam) || !length(args)) return(cols)
  out <- as.character(cols)
  generated <- function(n) paste0(c(readr = "X", base = "V", fread = "V", excel = "...")[[fam]], seq_len(n))
  # 1. the header: generated names, or a literal vector in its place (the
  # first line is then data, so the file's field count is kept)
  if (fam %in% c("readr", "excel")) {
    v <- arg("col_names")
    if (isFALSE(v)) out <- generated(length(out))
    else if (!is.null(v) && !isTRUE(v)) {
      lit <- string_vector(v, ctx)
      if (is.null(lit)) return(give_up("col_names"))
      out <- if (length(lit) >= length(out)) lit else c(lit, generated(length(out))[seq.int(length(lit) + 1L, length(out))])
    }
  }
  if (fam %in% c("base", "fread")) {
    header <- literal_flag(arg("header"), fn != "read.table")
    if (identical(arg("header"), "auto")) header <- TRUE
    if (is.na(header)) return(give_up("header"))
    if (!header) out <- generated(length(out))
    v <- arg("col.names")
    if (!is.null(v)) {
      lit <- string_vector(v, ctx)
      if (is.null(lit)) return(give_up("col.names"))
      out <- lit
    }
  }
  cols <- keep_source(out, cols)
  # 2. which columns are kept
  if (fam %in% c("readr", "arrow", "haven")) {
    sel <- arg("col_select")
    if (!is.null(sel)) {
      out <- reader_selection(sel, cols, ctx)
      if (is.null(out)) return(give_up("col_select"))
    }
  }
  if (fam == "readr") {
    ct <- arg("col_types")
    if (is.call(ct) && identical(call_name(ct), "cols_only")) {
      nm <- names(call_args(ct)) %||% character()
      if (length(nm) != length(call_args(ct)) || !all(nzchar(nm))) return(give_up("col_types"))
      check_symbols(nm, cols, ctx)
      out <- out[out %in% nm]
    } else if (is.call(ct) && identical(call_name(ct), "cols")) {
      ca <- call_args(ct)
      skipped <- vapply(ca, function(a) (is.call(a) && identical(call_name(a), "col_skip")) || identical(a, "_") || identical(a, "-"), TRUE)
      out <- setdiff(out, (names(ca) %||% character())[skipped])
    } else if (is.character(ct) && length(ct) == 1L && grepl("[_-]", ct)) {
      drop <- which(strsplit(ct, "")[[1L]] %in% c("_", "-"))
      out <- out[!(seq_along(out) %in% drop)]
    }
    id <- arg("id")
    if (!is.null(id)) {
      if (!(is.character(id) && length(id) == 1L)) return(give_up("id"))
      out <- c(id, out)
    }
  }
  if (fam == "fread") {
    for (nm in c("select", "drop")) {
      v <- arg(nm)
      if (is.null(v)) next
      picked <- fread_picks(v, cols, ctx)
      if (is.null(picked)) return(give_up(nm))
      out <- if (nm == "select") picked else setdiff(out, picked)
    }
    repair <- literal_flag(arg("check.names"), FALSE)
    if (is.na(repair)) return(give_up("check.names"))
    if (repair) out <- make.names(out, unique = TRUE)
  }
  if (fam == "base") {
    repair <- literal_flag(arg("check.names"), TRUE)
    if (is.na(repair)) return(give_up("check.names"))
    if (repair) out <- make.names(out, unique = TRUE)
    cc <- arg("colClasses")
    classes <- if (is.character(cc)) cc
      else if (is.call(cc) && identical(call_name(cc), "c") && all(vapply(call_args(cc), is.character, TRUE))) unlist(call_args(cc))
      else NULL
    if (!is.null(classes) && any(classes == "NULL")) {
      nms <- names(classes) %||% rep("", length(classes))
      out <- if (all(nzchar(nms))) setdiff(out, nms[classes == "NULL"])
        else if (length(classes) == length(out)) out[classes != "NULL"] else out
    }
    rn <- arg("row.names")
    if (is.numeric(rn) && length(rn) == 1L && !is.na(rn) && rn >= 1 && rn <= length(out)) out <- out[-rn]
    else if (is.character(rn) && length(rn) == 1L) out <- setdiff(out, rn)
  }
  keep_source(unique(out), cols)
}

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
  attr(cols, "inner") <- attr(template, "inner")
  attr(cols, "open") <- attr(template, "open")
  cols
}

# An open column set: the frame has at least these columns (a column
# assertion on an unknown frame). Nothing is reported against it, since any
# name may be among the rest; it closes at the first verb that keeps only
# named columns (select, transmute, summarise, distinct, count), after which
# the set is known and checked.
is_open <- function(cols) isTRUE(attr(cols, "open"))
closed <- function(cols) { attr(cols, "open") <- NULL; cols }

# A third outcome beside known columns and NULL (unknown): the expression is
# not a data frame at all, because a terminal verb turned it into a vector
# or a scalar (pull(), nrow(), names()). It flows through pipes and wrappers
# untouched, binds its target as a plain variable rather than an unresolved
# frame (so no "source the checker cannot open" note blames the loader the
# pipeline started from), and reads as unknown wherever a frame is required.
not_frame <- structure(character(), not_frame = TRUE)
is_not_frame <- function(x) isTRUE(attr(x, "not_frame"))
as_frame <- function(x) if (is_not_frame(x)) NULL else x

# Verbs whose result is not a frame. pull() names a column and is checked;
# the rest take no column names, so a frame that only reaches them is not
# "used" in the sense that earns an unresolved-source note.
terminal_calls <- c("pull", "nrow", "ncol", "dim", "names", "colnames", "deframe")

# Calls that consume a frame without naming its columns: the frame is written
# out, and that is where it went. Recorded as a "written" fate for the data
# map; the frame keeps its columns (readr's writers return their input).
sink_calls <- c("saveRDS", "save", "write_rds", "write_csv", "write_csv2", "write_tsv", "write_delim", "write_excel_csv",
                "write.csv", "write.csv2", "write.table", "fwrite", "write_parquet", "write_feather", "write_ipc_file",
                "write_dataset", "write_xlsx", "write.xlsx", "writeData", "write_fst", "write_dta", "write_sav", "write_sas",
                "write_xpt", "write_json", "qsave", "dbWriteTable", "dbAppendTable", "copy_to", "s3write_using", "s3saveRDS",
                "s3save", "write_sf", "st_write", "write_ods")
# where the frame sits in a sink call: these names, else the first positional
# argument that resolves to a frame (`dbWriteTable(con, "t", df)`)
sink_frame_args <- c("x", "object", "value", "data", ".data", "df", "obj", "dataset", "table")

record_sink <- function(e, fn, ctx, cols = NULL) {
  if (is.null(cols)) {
    args <- call_args(e)
    nms <- names(args) %||% rep("", length(args))
    for (a in c(args[nms %in% sink_frame_args], args[!nzchar(nms)])) {
      if (!(is.symbol(a) || is.call(a))) next
      if (is.call(a) && identical(call_name(a), "function")) next
      cols <- as_frame(frame_columns(a, ctx))
      if (!is.null(cols)) break
    }
  }
  if (is.null(cols) || is_listed(cols)) return(invisible())
  record_step(ctx, e, cols, before = cols, fate = "written", fate_fn = fn)
  invisible()
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
    # a cache entry for a piped helper, `df %>% clean()`, beats following its
    # code, as it does for the direct call `clean(df)`; verbs never match
    cached <- if (!is.na(rhs_fn) && !(rhs_fn %in% c(verb_names, combining_calls, listed_calls, mapping_calls))) cached_columns(rhs, ctx) else NULL
    if (!is.na(rhs_fn) && rhs_fn %in% c(listed_calls, mapping_calls) && is.null(rhs_def)) {
      lhs <- frame_columns(e[[2L]], ctx, used = TRUE)
      if (is.null(lhs) || is_not_frame(lhs)) return(if (is.null(lhs)) NULL else lhs)
      if (rhs_fn %in% mapping_calls && !is_listed(lhs)) return(NULL)
      result <- if (rhs_fn %in% listed_calls) split_frames(rhs, lhs, ctx, piped = TRUE)
        else map_over(rhs, lhs, ctx, piped = TRUE)
      record_step(ctx, rhs, result)
      if (is.null(result) && (rhs_fn %in% listed_calls || is_listed(lhs)))
        note_stop(ctx, rhs, rhs_fn, NULL, reason = "the columns its lambda returns cannot be worked out")
      return(result)
    }
    if (!is.na(rhs_fn) && rhs_fn %in% combining_calls && is.null(rhs_def)) {
      first <- input_frame(e[[2L]], ctx)
      result <- bound_rows(rhs, first, ctx, partial = is.null(first))
      record_step(ctx, rhs, result)
      # a binding whose inputs were all unknown was never known: no stop to name
      if (!is.null(first) && is.null(result)) note_stop(ctx, rhs, rhs_fn, NULL, reason = "its id column is computed")
      return(result)
    }
    if (!is.na(rhs_fn) && rhs_fn %in% sink_calls && is.null(rhs_def)) {
      lhs <- frame_columns(e[[2L]], ctx, used = used)
      if (!is.null(lhs) && !is_not_frame(lhs)) record_sink(rhs, rhs_fn, ctx, cols = lhs)
      # readr's writers return their input; saveRDS() and its kind return nothing
      return(if (rhs_fn %in% c("saveRDS", "save", "qsave", "s3saveRDS", "s3save")) not_frame else lhs)
    }
    lhs <- frame_columns(e[[2L]], ctx, used = used || rhs_fn %in% column_verbs || !is.null(rhs_def) || !is.null(cached))
    if (!is.null(cached)) return(cached)
    if (is.null(lhs)) {
      if (!is.na(rhs_fn)) count_step(ctx, "skipped", rhs_fn)
      return(NULL)
    }
    if (is_not_frame(lhs)) return(lhs)
    result <- if (!is.null(rhs_def)) {
      ctx$frames[[".colcheck_lhs"]] <- lhs
      call_user_function(pipe_into(rhs, as.name(".colcheck_lhs")), rhs_fn, ctx, def = rhs_def, shown = rhs)
    } else apply_verb(rhs, lhs, ctx, piped = TRUE)
    if (is.null(result)) note_stop(ctx, rhs, rhs_fn, rhs_def)
    return(result)
  }
  if (fn %in% readers) {
    path_arg <- if (length(e) >= 2L) e[[2L]] else NULL
    nms <- names(e)
    if (!is.null(nms)) {
      for (k in c("file", "path", "input", "sources")) if (k %in% nms) path_arg <- e[[k]]
    }
    path <- resolve_path(path_arg, ctx)
    if (is.null(path)) return(cached_columns(e, ctx))
    # a file that is there but cannot be read (a dataset directory, a big
    # RDS, a format whose package is missing) falls back to the cache, as a
    # path that is not there does, so the learn command the note gives works
    cols <- reader_columns(path, e, fn, ctx)
    if (is.null(cols)) return(cached_columns(e, ctx))
    return(if (isTRUE(attr(cols, "gave_up"))) NULL else cols)
  }
  if (fn %in% c("tibble", "data.frame", "data_frame")) {
    nms <- names(e)[-1L]
    if (is.null(nms) || any(!nzchar(nms))) return(NULL)
    cols <- nms
    attr(cols, "source") <- paste0(fn, "() literal")
    attr(cols, "source_line") <- ctx$line
    # data = list(tibble(d = 1, r = 2)): the inner columns of a list-column
    inner <- list()
    for (i in seq_along(nms)) {
      a <- e[[i + 1L]]
      if (is.call(a) && identical(call_name(a), "list") && length(a) >= 2L) {
        first <- a[[2L]]
        if (is.call(first) && call_name(first) %in% c("tibble", "data.frame", "data_frame")) {
          inn <- names(first)[-1L]
          if (!is.null(inn) && all(nzchar(inn))) inner[[nms[[i]]]] <- inn
        }
      }
    }
    if (length(inner)) attr(cols, "inner") <- inner
    return(cols)
  }
  if (fn == "tribble") {
    hdr <- Filter(function(a) is.call(a) && identical(a[[1L]], as.name("~")), call_args(e))
    cols <- vapply(hdr, function(a) as.character(a[[2L]]), "")
    if (!length(cols)) return(NULL)
    attr(cols, "source") <- "tribble() literal"
    attr(cols, "source_line") <- ctx$line
    return(cols)
  }
  # native pipe is parsed into nested calls: verb(frame_expr, ...); a ggplot
  # chain is `+` calls nested the same way
  if (fn %in% verb_names || fn == "+") {
    if (length(e) < 2L) return(NULL)
    inner <- frame_columns(e[[2L]], ctx, used = used || fn %in% column_verbs)
    if (is_not_frame(inner)) return(inner)
    if (!is.null(inner)) {
      result <- apply_verb(e, inner, ctx, piped = FALSE)
      if (is.null(result)) note_stop(ctx, e, fn, NULL)
      return(result)
    }
    count_step(ctx, "skipped", fn)
    return(NULL)
  }
  if (fn %in% c("{", "(")) return(if (length(e) >= 2L) frame_columns(e[[length(e)]], ctx, used = used) else NULL)
  if (fn %in% c("function", "if", "for", "while", "repeat", "$", "@", "[", "[[")) return(NULL)
  if (fn %in% c(listed_calls, mapping_calls) && is.null(lookup_function(e, fn, ctx))) {
    args <- call_args(e)
    nms <- names(args) %||% rep("", length(args))
    data <- if (fn %in% mapping_calls && any(c(".x", "X") %in% nms)) args[[intersect(c(".x", "X"), nms)[[1L]]]]
      else if (fn == "group_split" && "x" %in% nms) args[["x"]] else if (length(args)) args[[1L]] else NULL
    if (is.null(data)) return(NULL)
    first <- frame_columns(data, ctx, used = TRUE)
    if (is.null(first) || is_not_frame(first)) return(if (is.null(first)) NULL else first)
    if (fn %in% mapping_calls && !is_listed(first)) return(NULL)
    result <- if (fn %in% listed_calls) split_frames(e, first, ctx, piped = FALSE)
      else map_over(e, first, ctx, piped = FALSE)
    shown <- e
    if (length(e) >= 2L && is.call(e[[2L]])) shown[[2L]] <- as.name(".")
    record_step(ctx, shown, result)
    if (is.null(result) && (fn %in% listed_calls || is_listed(first)))
      note_stop(ctx, e, fn, NULL, reason = "the columns its lambda returns cannot be worked out")
    return(result)
  }
  if (fn %in% combining_calls && is.null(lookup_function(e, fn, ctx))) {
    args <- call_args(e)
    nms <- names(args) %||% rep("", length(args))
    inputs <- args[!(nms %in% id_args)]
    if (!length(inputs)) return(NULL)
    first <- input_frame(inputs[[1L]], ctx)
    rest <- e
    rest[[1L + which(!(nms %in% id_args))[[1L]]]] <- NULL   # the first input is resolved already
    result <- bound_rows(rest, first, ctx, partial = is.null(first))
    record_step(ctx, e, result)
    if (!is.null(first) && is.null(result)) note_stop(ctx, e, fn, NULL, reason = "its id column is computed")
    return(result)
  }
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
    # a known frame went into a call whose result the checker cannot
    # describe (`df |> clean_names()` parses to clean_names(df)): the same
    # stop the %>% branch records, so the statement gets its note
    if (!is.null(inner) && !is_not_frame(inner) && !(fn %in% not_loaders)) note_stop(ctx, e, fn, NULL)
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

# Bare symbols that are direct arguments of a plain call inside the
# expression, `sd` in `slide_dbl(x, sd, .before = 11)` or `sapply(x, nchar)`:
# the one place where a name that is not a column is more likely a function
# passed as a value than a typo. An operand of `+`, `==` or `[`, or the
# value expression itself, is never in this set.
fn_arg_symbols <- function(e, acc = character()) {
  if (!is.call(e)) return(acc)
  fn <- call_name(e)
  if (!is.na(fn) && fn %in% c("$", "@", "::", ":::")) return(acc)
  plain <- !is.na(fn) && grepl("^[A-Za-z.][A-Za-z0-9._]*$", fn) &&
    !(fn %in% c("if", "for", "while", "repeat", "function", "return"))
  for (p in call_args(e)) {
    if (plain && is.symbol(p)) acc <- c(acc, as.character(p))
    acc <- fn_arg_symbols(p, acc)
  }
  acc
}

# Whether `s` names a function the code can see: a project function, one of
# R's default packages, or an export of a package the file attached with
# library() or the package's DESCRIPTION imports. Looked up only for a name
# about to be reported, and once per name.
is_function_name <- function(s, ctx) {
  memo <- ctx$fn_memo[[s]]
  if (!is.null(memo)) return(memo)
  hit <- !is.null(ctx$functions[[s]]) ||
    any(vapply(c("stats", "utils", "methods"), function(pkg) exists(s, envir = asNamespace(pkg), inherits = FALSE), TRUE))
  if (!hit) for (pkg in unique(c(ctx$packages, ctx$imports))) {
    if (pkg %in% c("base", "stats", "utils", "methods")) next
    if (quiet(requireNamespace(pkg, quietly = TRUE), FALSE) && s %in% quiet(getNamespaceExports(pkg), character())) { hit <- TRUE; break }
  }
  ctx$fn_memo[[s]] <- hit
  hit
}

collect_symbols <- function(e, acc = character()) {
  if (is.symbol(e)) return(c(acc, as.character(e)))
  if (!is.call(e)) return(acc)
  fn <- call_name(e)
  # `.data$x`, `.data[["x"]]`, get("x"), sym("x"), !!sym("x"): x names a
  # column of the frame as surely as a bare x (the package idiom); `.env$x`
  # and `.data[[variable]]` are left alone
  if (!is.na(fn) && fn %in% c("$", "[[") && length(e) == 3L && identical(e[[2L]], as.name(".data"))) {
    x <- e[[3L]]
    return(if (is.character(x) && length(x) == 1L) c(acc, x) else if (fn == "$" && is.symbol(x)) c(acc, as.character(x)) else acc)
  }
  if (!is.na(fn) && fn %in% c("!!") && length(e) == 2L && is.call(e[[2L]])) e <- e[[2L]]
  if (is.call(e) && call_name(e) %in% c("get", "sym") && length(e) == 2L && is.character(e[[2L]]) && length(e[[2L]]) == 1L) return(c(acc, e[[2L]]))
  if (!is.na(fn) && fn %in% c("!!") && length(e) == 2L) return(acc)
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
island_selection <- function(e, ctx = NULL, cols = NULL) {
  fn <- call_name(e)
  if (is.na(fn)) return(NULL)
  args <- call_args(e)
  nms <- names(args) %||% rep("", length(args))
  if (fn == "all_of") return(selection_names(list(e), ctx, cols))
  if (fn == "pick") return(selection_names(args, ctx, cols))
  if (fn %in% c("across", "if_any", "if_all", "c_across")) {
    slot <- if (fn == "c_across") "cols" else ".cols"
    sel <- if (slot %in% nms) args[[slot]] else if (any(!nzchar(nms))) args[!nzchar(nms)][[1L]] else NULL
    if (is.null(sel)) return(NULL)
    return(selection_names(list(sel), ctx, cols))
  }
  NULL
}

# The columns an across()/pick() island adds to the frame it sits in.
# character() when it rewrites the selected columns in place: a single
# function and no `.names`, which needs no reading of the selection (a
# `where(is.Date)` or `all_of(variable)` selection keeps the column set just
# the same). The expanded names when `.names` is a literal template of
# `{.col}` and `{.fn}` or `.fns` is a list() of functions (default template
# "{.col}_{.fn}", the list's names or positions as `.fn`), which needs the
# selection. NULL when the names cannot be worked out. A bare symbol as
# `.fns` counts as one function: `across(cols, mean)` is the common shape.
across_names <- function(e, ctx = NULL, cols = NULL) {
  fn <- call_name(e)
  if (identical(fn, "pick")) return(character())
  if (!identical(fn, "across")) return(NULL)
  args <- call_args(e)
  nms <- names(args) %||% rep("", length(args))
  pos <- args[!nzchar(nms)]
  fns <- if (".fns" %in% nms) args[[".fns"]] else if (length(pos) >= 2L) pos[[2L]] else NULL
  tpl <- if (".names" %in% nms) args[[".names"]] else if (length(pos) >= 3L) pos[[3L]] else NULL
  many <- is.call(fns) && call_name(fns) %in% c("list", "c")
  if (is.null(tpl)) {
    if (!many) return(character())
    tpl <- "{.col}_{.fn}"
  }
  if (!is.character(tpl) || length(tpl) != 1L) return(NULL)
  fn_names <- if (many) {
    fa <- call_args(fns)
    fnm <- names(fa) %||% rep("", length(fa))
    ifelse(nzchar(fnm), fnm, as.character(seq_along(fa)))
  } else "1"
  sel <- island_selection(e, ctx, cols)
  if (is.null(sel)) return(NULL)
  holes <- regmatches(tpl, gregexpr("\\{[^}]*\\}", tpl))[[1L]]
  if (!all(holes %in% c("{.col}", "{col}", "{.fn}", "{fn}"))) return(NULL)
  out <- character()
  for (f in fn_names) for (cl in sel) out <- c(out, gsub("\\{\\.?col\\}", cl, gsub("\\{\\.?fn\\}", f, tpl)))
  unique(out)
}

# A character vector the code states literally: `c("a", "b")`, a variable
# bound to one earlier in the same scope, `switch("kind", a = c(...))` on a
# literal, or a project/package function whose body returns one of these
# (`columns_for("monthly")`). NULL when it cannot be worked out. Used for
# the column vectors that assertions name.
string_vector <- function(e, ctx, depth = 0L) {
  if (is.character(e)) return(e)
  if (is.symbol(e)) return(ctx$strings[[as.character(e)]])
  if (!is.call(e)) return(NULL)
  fn <- call_name(e)
  if (is.na(fn)) return(NULL)
  if (fn == "c") {
    parts <- lapply(call_args(e), string_vector, ctx = ctx, depth = depth)
    if (any(vapply(parts, is.null, TRUE))) return(NULL)
    return(unlist(parts))
  }
  if (fn %in% c("(", "{")) return(if (length(e) >= 2L) string_vector(e[[length(e)]], ctx, depth) else NULL)
  if (fn %in% c("return", "invisible") && length(e) == 2L) return(string_vector(e[[2L]], ctx, depth))
  if (fn == "switch" && length(e) >= 3L) {
    on <- string_vector(e[[2L]], ctx, depth)
    if (is.null(on) || length(on) != 1L) return(NULL)
    branches <- as.list(e)[-(1:2)]
    nms <- names(branches) %||% rep("", length(branches))
    hit <- which(nms == on)
    if (!length(hit)) hit <- which(!nzchar(nms))   # the default branch
    if (!length(hit)) return(NULL)
    return(string_vector(branches[[hit[[1L]]]], ctx, depth))
  }
  if (depth >= 3L) return(NULL)
  # a function is followed only when every argument is itself a string
  # (`columns_for("monthly")`): a frame or anything else going in means the
  # result is not a column vector, and looking the function up costs time
  args <- call_args(e)
  if (length(args) && !all(vapply(args, function(a) is.character(a) || (is.symbol(a) && !is.null(ctx$strings[[as.character(a)]])), TRUE))) return(NULL)
  tryCatch(string_vector_from(e, fn, ctx, depth), error = function(err) NULL)
}

string_vector_from <- function(e, fn, ctx, depth) {
  def <- lookup_function(e, fn, ctx)
  if (is.null(def) || !is.call(def$fn) || length(def$fn) < 3L) return(NULL)
  proto <- quiet(eval(call("function", def$fn[[2L]], NULL)))
  if (!is.function(proto)) return(NULL)
  matched <- quiet(match.call(proto, e, expand.dots = FALSE))
  if (is.null(matched)) return(NULL)
  saved <- ctx$strings
  on.exit(ctx$strings <- saved)
  formals_ <- def$fn[[2L]]
  for (p in names(formals_) %||% character()) {
    v <- if (p %in% names(matched)) string_vector(matched[[p]], ctx, depth + 1L)
      else if (!is.symbol(formals_[[p]]) || nzchar(as.character(formals_[[p]]))) string_vector(formals_[[p]], ctx, depth + 1L)
      else NULL
    ctx$strings[[p]] <- v
  }
  body <- def$fn[[3L]]
  if (is.call(body) && identical(call_name(body), "{")) {
    stmts <- as.list(body)[-1L]
    for (st in stmts[-length(stmts)]) {
      if (is.call(st) && call_name(st) %in% c("<-", "=") && is.symbol(st[[2L]])) {
        ctx$strings[[as.character(st[[2L]])]] <- string_vector(st[[3L]], ctx, depth + 1L)
      }
    }
    if (!length(stmts)) return(NULL)
    body <- stmts[[length(stmts)]]
  }
  string_vector(body, ctx, depth + 1L)
}

# --- column assertions -------------------------------------------------------
# A call that stops unless a frame has certain columns states, for the code
# after it, what the frame has: a configured function (.colcheck.json),
# `stopifnot(all(cols %in% names(df)))`, `if (!all(cols %in% names(df)))
# stop(...)`, `checkmate::assert_names(names(df), must.include = cols)`.
# On a known frame the names are checked; on an unknown one they become its
# (open, or exact) column set.
assertion_of <- function(e, ctx) {
  fn <- call_name(e)
  if (is.na(fn)) return(NULL)
  head <- e[[1L]]
  args <- call_args(e)
  nms <- names(args) %||% rep("", length(args))
  pos <- args[!nzchar(nms)]
  pick <- function(spec) {
    if (is.character(spec) && length(spec) == 1L) { if (spec %in% nms) args[[spec]] else NULL }
    else if (is.numeric(spec) && length(spec) == 1L && spec >= 1 && spec <= length(pos)) pos[[spec]]
    else NULL
  }
  frame_of <- function(x) {
    if (is.call(x) && call_name(x) %in% c("names", "colnames") && length(x) == 2L) x <- x[[2L]]
    if (is.symbol(x)) as.character(x) else NULL
  }
  result <- function(frame, cols, exact, what) {
    if (is.null(frame) || is.null(cols) || !length(cols)) return(NULL)
    list(frame = frame, cols = unique(cols), exact = isTRUE(exact), fn = what)
  }
  for (cfg in ctx$config$assertions %||% list()) {
    want <- cfg$fn
    if (!is.character(want) || length(want) != 1L) next
    bare <- sub("^.*::", "", want)
    pkg <- if (grepl("::", want, fixed = TRUE)) sub("::.*$", "", want) else NULL
    if (!identical(bare, fn)) next
    if (!is.null(pkg) && is.call(head) && !identical(as.character(head[[2L]]), pkg)) next
    exact <- if (isTRUE(cfg$exact)) TRUE
      else if (is.character(cfg$exact) && cfg$exact %in% nms) isTRUE(args[[cfg$exact]])
      else FALSE
    return(result(frame_of(pick(cfg$frame)), string_vector(pick(cfg$columns), ctx), exact, want))
  }
  membership <- function(x) names_membership(x, ctx)
  if (fn %in% c("stopifnot", "assert_that")) {   # stopifnot("message" = cond) names its conditions
    for (a in args) { m <- membership(a); if (!is.null(m)) { m$fn <- fn; return(m) } }
    return(NULL)
  }
  if (fn == "if" && length(e) >= 3L) {
    cond <- e[[2L]]
    if (is.call(cond) && identical(call_name(cond), "(") && length(cond) == 2L) cond <- cond[[2L]]
    inner <- if (is.call(cond) && identical(call_name(cond), "!") && length(cond) == 2L) cond[[2L]]
      else if (is.call(cond) && identical(call_name(cond), "any") && length(cond) == 2L &&
               is.call(cond[[2L]]) && identical(call_name(cond[[2L]]), "!") && length(cond[[2L]]) == 2L) cond[[2L]][[2L]]
      else NULL
    if (is.null(inner)) return(NULL)
    stops <- any(all.names(e[[3L]]) %in% c("stop", "abort", "cli_abort", "stopifnot", "rlang_abort"))
    if (!stops) return(NULL)
    m <- membership(inner)
    if (!is.null(m)) m$fn <- "if"
    return(m)
  }
  if (fn == "assert_names") {
    x <- if ("x" %in% nms) args[["x"]] else if (length(pos)) pos[[1L]] else NULL
    f <- if (is.call(x)) frame_of(x) else NULL
    if (is.null(f)) return(NULL)
    for (k in c("must.include", "permutation.of", "identical.to")) if (k %in% nms) {
      return(result(f, string_vector(args[[k]], ctx), k != "must.include", "checkmate::assert_names"))
    }
    return(NULL)
  }
  if (fn == "assert_subset" && length(pos) >= 2L) {
    f <- if (is.call(pos[[2L]])) frame_of(pos[[2L]]) else NULL
    return(result(f, string_vector(pos[[1L]], ctx), FALSE, "checkmate::assert_subset"))
  }
  NULL
}

# `all(cols %in% names(df))` or `cols %in% names(df)`: the frame and the
# names, or NULL.
names_membership <- function(x, ctx) {
  if (is.call(x) && identical(call_name(x), "(") && length(x) == 2L) x <- x[[2L]]
  if (is.call(x) && identical(call_name(x), "all") && length(x) == 2L) x <- x[[2L]]
  if (is.call(x) && identical(call_name(x), "%in%") && length(x) == 3L && is.call(x[[3L]])) {
    nm <- x[[3L]]
    if (call_name(nm) %in% c("names", "colnames") && length(nm) == 2L && is.symbol(nm[[2L]])) {
      cols <- string_vector(x[[2L]], ctx)
      if (!is.null(cols) && length(cols)) return(list(frame = as.character(nm[[2L]]), cols = unique(cols), exact = FALSE, fn = "if"))
    }
  }
  NULL
}

# `if (all(cols %in% names(df))) { ... }`: inside the branch the frame has
# those columns (added to a known frame, an open set for an unknown one);
# outside it nothing changes. The else branch is walked as before.
walk_guarded_if <- function(e, ctx) {
  m <- names_membership(e[[2L]], ctx)
  if (is.null(m)) return(FALSE)
  nm <- m$frame
  saved <- ctx$frames[[nm]]
  saved_unknown <- ctx$unknown_frames
  if (!is.null(saved) && !is_listed(saved)) {
    ctx$frames[[nm]] <- keep_source(structure(unique(c(as.character(saved), m$cols)), groups = attr(saved, "groups") %||% character()), saved)
  } else if (is.null(saved)) {
    apply_assertion(m, ctx)
  }
  walk_expr(e[[3L]], ctx)
  ctx$frames[[nm]] <- saved
  ctx$unknown_frames <- saved_unknown
  if (length(e) >= 4L) walk_expr(e[[4L]], ctx)
  TRUE
}

apply_assertion <- function(a, ctx) {
  nm <- a$frame
  cur <- ctx$frames[[nm]]
  if (!is.null(cur) && !is_open(cur)) {
    # a known frame: the columns the code requires must be there
    check_symbols(a$cols, cur, ctx)
    if (a$exact) ctx$frames[[nm]] <- closed(keep_source(structure(a$cols, groups = attr(cur, "groups")), cur))
    return(invisible())
  }
  cols <- structure(unique(c(if (!is.null(cur)) as.character(cur), a$cols)),
                    source = sprintf("the columns %s() requires on line %s", a$fn %||% "the assertion", ctx$line),
                    groups = attr(cur, "groups") %||% character())
  if (a$exact) cols <- structure(a$cols, source = attr(cols, "source"), groups = attr(cols, "groups"))
  else attr(cols, "open") <- TRUE
  ctx$frames[[nm]] <- cols
  ctx$unknown_frames <- setdiff(ctx$unknown_frames, nm)
  ctx$variables <- union(ctx$variables, nm)
  invisible()
}

# The grouping columns a group_by() names: a selection, an island, or a
# computed group (`group_by(yr = year(date))`, which also adds the column).
# NULL when any of them cannot be resolved.
grouping_names <- function(pos, ctx = NULL, cols = NULL) {
  out <- character()
  for (a in pos) {
    g <- if (is.call(a) && call_name(a) %in% c("across", "pick")) island_selection(a, ctx, cols) else selection_names(list(a), ctx, cols)
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

# The names a selection picks: bare names, strings, `-`/`!` negations,
# `c()`, `(...)`, `all_of()`, the range `a:b` (every known column from a to
# b, in either direction) and the tidyselect helpers expanded against the
# frame's known columns (`cols`): `starts_with()`, `ends_with()`,
# `contains()`, `matches()`, `num_range()`, `everything()`, `last_col()`,
# `any_of()`. NULL when a name cannot be worked out. On a partly known frame
# a positive helper or range could match columns the checker has not seen,
# so it stays unresolved; a negated one drops the known matches and the set
# stays open.
selection_names <- function(args, ctx = NULL, cols = NULL, negated = FALSE) {
  out <- character()
  for (a in args) {
    if (is.symbol(a)) out <- c(out, as.character(a))
    else if (is.character(a)) out <- c(out, a)
    else if (is.call(a) && call_name(a) %in% c("-", "!") && length(a) == 2L) {
      inner <- selection_names(list(a[[2L]]), ctx, cols, !negated)
      if (is.null(inner)) return(NULL)
      out <- c(out, inner)
    } else if (is.call(a) && call_name(a) == ":" && length(a) == 3L) {
      # with no frame to span (an island checked for its names only) the
      # endpoints are what there is to check; a missing endpoint is passed
      # on for the caller's check_symbols() to report
      ends <- selection_names(as.list(a)[-1L], ctx, cols, negated)
      if (is.null(ends) || length(ends) != 2L) return(NULL)
      if (is.null(cols)) { out <- c(out, ends); next }
      if (is_open(cols) && !negated) return(NULL)
      known <- as.character(cols)
      i <- match(ends, known)
      out <- c(out, if (anyNA(i)) ends else known[seq(i[[1L]], i[[2L]])])
    } else if (is.call(a) && call_name(a) %in% c("c", "(")) {
      inner <- selection_names(as.list(a)[-1L], ctx, cols, negated)
      if (is.null(inner)) return(NULL)
      out <- c(out, inner)
    } else if (is.call(a) && call_name(a) %in% select_helpers) {
      inner <- helper_names(a, ctx, cols, negated)
      if (is.null(inner)) return(NULL)
      out <- c(out, inner)
    } else if (is.call(a) && call_name(a) == "all_of" && length(a) == 2L) {
      # all_of("a"), all_of(c("a", "b")), or all_of(keys) with a vector the
      # code states (a literal bound earlier, a switch(), a helper returning
      # one): the names must exist, so they are checked; any_of() (missing
      # names allowed) stays unresolved
      inner <- if (!is.null(ctx)) string_vector(a[[2L]], ctx)
        else if (is.character(a[[2L]])) a[[2L]]
        else if (is.call(a[[2L]]) && identical(call_name(a[[2L]]), "c") &&
            all(vapply(as.list(a[[2L]])[-1L], is.character, TRUE))) unlist(as.list(a[[2L]])[-1L]) else NULL
      if (is.null(inner)) return(NULL)
      out <- c(out, inner)
    } else return(NULL)
  }
  out
}

select_helpers <- c("starts_with", "ends_with", "contains", "matches", "num_range", "everything",
                    "last_col", "any_of")

# One tidyselect helper expanded against the known columns, as tidyselect
# would: `ignore.case = TRUE` by default, a vector of prefixes matching any,
# zero matches a legal empty selection. NULL when the frame or the helper's
# argument is unknown, or when a positive helper meets a partly known frame.
helper_names <- function(e, ctx, cols, negated) {
  if (is.null(cols)) return(NULL)
  if (is_open(cols) && !negated) return(NULL)
  known <- as.character(cols)
  fn <- call_name(e)
  args <- call_args(e)
  nms <- names(args) %||% rep("", length(args))
  pos <- args[!nzchar(nms)]
  arg <- function(name, i) if (name %in% nms) args[[name]] else if (length(pos) >= i) pos[[i]] else NULL
  strings <- function(x) {
    if (is.null(x)) NULL else if (is.character(x)) x else if (!is.null(ctx)) string_vector(x, ctx) else NULL
  }
  if (fn == "everything") return(known)
  if (fn == "last_col") {
    off <- arg("offset", 1L) %||% 0
    if (!is.numeric(off) || length(off) != 1L) return(NULL)
    i <- length(known) - off
    return(if (i >= 1L && i <= length(known)) known[[i]] else NULL)
  }
  if (fn == "any_of") {
    v <- strings(arg("x", 1L))
    return(if (is.null(v)) NULL else known[known %in% v])
  }
  if (fn == "num_range") {
    prefix <- strings(arg("prefix", 1L))
    range <- arg("range", 2L)
    rng <- if (is.numeric(range)) range
      else if (is.call(range) && call_name(range) %in% c(":", "c") && all(vapply(call_args(range), is.numeric, TRUE)))
        quiet(eval(range, baseenv())) else NULL
    suffix <- arg("suffix", 3L) %||% ""
    width <- arg("width", 4L)
    if (is.null(prefix) || is.null(rng) || !is.character(suffix) || !(is.null(width) || is.numeric(width))) return(NULL)
    nums <- if (is.null(width)) as.character(rng) else formatC(rng, width = width, flag = "0", format = "d")
    wanted <- as.vector(outer(prefix, paste0(nums, suffix), paste0))
    return(known[known %in% wanted])
  }
  pat <- strings(arg("match", 1L))
  if (is.null(pat) || !length(pat)) return(NULL)
  ic <- arg("ignore.case", 2L) %||% TRUE
  if (!is.logical(ic) || length(ic) != 1L || is.na(ic)) return(NULL)
  k <- if (ic) tolower(known) else known
  p <- if (ic) tolower(pat) else pat
  hit <- switch(fn,
    starts_with = Reduce(`|`, lapply(p, function(x) startsWith(k, x))),
    ends_with = Reduce(`|`, lapply(p, function(x) endsWith(k, x))),
    contains = Reduce(`|`, lapply(p, function(x) grepl(x, k, fixed = TRUE))),
    matches = {
      perl <- arg("perl", 3L) %||% FALSE
      if (!is.logical(perl) || length(perl) != 1L) return(NULL)
      quiet(Reduce(`|`, lapply(pat, function(x) grepl(x, known, ignore.case = ic, perl = perl))))
    })
  if (is.null(hit)) return(NULL)
  known[hit]
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
    columns = as.character(cols), via = via, step = ctx$step_call, statement = ctx$line,
    message = sprintf("'%s' is not a column at this point (columns: %s; data from %s%s)", name,
                      paste(head(cols, 12), collapse = ", "), src,
                      if (is.null(via)) "" else sprintf(", called from %s", via)))
}

# `fn_args`: the symbols among `syms` that sit as a direct argument of a
# call (fn_arg_symbols); one of those that names a function in scope is a
# function passed as a value, not a column.
check_symbols <- function(syms, cols, ctx, fn_args = character()) {
  syms <- unique(syms)
  bad <- syms[!(syms %in% cols) & !(syms %in% ctx$variables) & !(syms %in% ctx$known_names) &
              !vapply(syms, function(s) exists(s, envir = baseenv()), TRUE)]
  bad <- bad[!(bad %in% fn_args) | !vapply(bad, is_function_name, TRUE, ctx = ctx)]
  if (is_open(cols)) {
    # a partly known frame ("at least these"): nothing is reported. For
    # r-colmap, the known names read here are uses; on a function parameter
    # every name that is not a variable is one too, since the body states
    # what it expects of the frame (on a data frame bound with an unknown one
    # the name may belong to the other, so only the known ones count)
    if (!is.null(ctx$used_now)) {
      expected <- if (startsWith(attr(cols, "source") %||% "", "parameter ")) bad else character()
      ctx$used_now <- union(ctx$used_now, c(syms[syms %in% cols], expected))
    }
    return(invisible(character()))
  }
  for (b in bad) report(ctx, b, cols)
  # the names that are columns: what this step reads (for r-colmap)
  if (!is.null(ctx$used_now)) ctx$used_now <- union(ctx$used_now, syms[syms %in% cols])
  invisible(bad)
}

# Apply one verb call to a known column set; returns the new set or NULL (unknown).
apply_verb <- function(call, cols, ctx, piped) {
  # a `|>` chain parses to nested calls: show the step as `verb(., ...)`
  shown <- call
  if (!piped && length(call) >= 2L && is.call(call[[2L]])) shown[[2L]] <- as.name(".")
  saved_used <- ctx$used_now
  saved_step <- ctx$step_call
  saved_joined <- ctx$joined_now
  # restored on every exit, an error inside the verb included
  on.exit({ ctx$used_now <- saved_used; ctx$step_call <- saved_step; ctx$joined_now <- saved_joined }, add = TRUE)
  ctx$used_now <- character()
  ctx$joined_now <- NULL
  ctx$step_call <- one_line(shown)
  result <- apply_verb_impl(call, cols, ctx, piped)
  used <- ctx$used_now
  record_step(ctx, shown, result, before = cols, used = used, joined = ctx$joined_now)
  result
}

# The column set after a verb, for `r-colcheck --steps`: a per-step listing
# of what the checker knows, the lineage a review can read. With the set
# before the step and the columns the step read, r-colmap's raw material.
# `fate`: a step that is not a verb but says where the frame went: "written"
# (a sink such as saveRDS(); `fate_fn` names it) or "returned" (the last
# expression of a named function; `fate_fn` is the function). The third fate,
# tracking ended, is a verb step whose `columns` are NULL with a known
# `before`, and needs no field. `joined`: for a join, the other frame (its
# source, columns, the keys read from it and the columns it brought into the
# result), so the data map can attribute those columns to their own source.
record_step <- function(ctx, call, cols, before = NULL, used = character(), fate = NULL, fate_fn = NULL,
                        joined = NULL, line = ctx$line) {
  if (!is.null(ctx$pkg_scope)) return(invisible())
  groups <- attr(cols, "groups") %||% character()
  groups_before <- attr(before, "groups") %||% character()
  ctx$steps[[length(ctx$steps) + 1L]] <- list(
    file = ctx$file, line = line, call = if (is.character(call)) call else one_line(call),
    fate = fate, fate_fn = fate_fn, joined = joined,
    columns = if (is.null(cols)) NULL else as.character(cols),
    before = if (is.null(before)) NULL else as.character(before),
    groups_before = if (anyNA(groups_before)) NULL else as.character(groups_before),
    open_before = is_open(before),
    used = as.character(used),
    open = is_open(cols),
    listed = is_listed(cols),
    groups = if (anyNA(groups)) NULL else as.character(groups),
    source = attr(cols, "source") %||% attr(before, "source") %||% NA_character_,
    source_line = attr(cols, "source_line") %||% attr(before, "source_line") %||% NA_integer_)
  invisible()
}

# The key columns a join's `by` names, as list(x = , y = ): a character
# vector the code states ("a", c("a", "b"), c(a = "b"), a variable bound to
# one), or join_by() of bare names, `a == b` and `x$a == y$b`. NULL when it
# cannot be read, or when join_by() holds anything else (an inequality,
# rolling or overlap join keeps both keys).
join_keys <- function(by, ctx) {
  if (is.call(by) && identical(call_name(by), "join_by")) {
    x <- y <- character()
    side <- function(s) {
      if (is.call(s) && identical(call_name(s), "$") && length(s) == 3L && is.symbol(s[[2L]]) &&
          as.character(s[[2L]]) %in% c("x", "y") && (is.symbol(s[[3L]]) || is.character(s[[3L]])))
        return(c(as.character(s[[2L]]), as.character(s[[3L]])))
      if (is.symbol(s) || (is.character(s) && length(s) == 1L)) return(c("", as.character(s)))
      NULL
    }
    for (a in call_args(by)) {
      if (is.symbol(a) || is.character(a)) { x <- c(x, as.character(a)); y <- c(y, as.character(a)); next }
      if (!is.call(a) || !identical(call_name(a), "==") || length(a) != 3L) return(NULL)
      l <- side(a[[2L]])
      r <- side(a[[3L]])
      if (is.null(l) || is.null(r) || (nzchar(l[[1L]]) && identical(l[[1L]], r[[1L]]))) return(NULL)
      if (l[[1L]] == "y" || r[[1L]] == "x") { t <- l; l <- r; r <- t }
      x <- c(x, l[[2L]])
      y <- c(y, r[[2L]])
    }
    return(if (length(x)) list(x = x, y = y) else NULL)
  }
  v <- string_vector(by, ctx)
  if (is.null(v) || !length(v) || anyNA(v)) return(NULL)
  n <- names(v) %||% rep("", length(v))
  list(x = unname(ifelse(nzchar(n), n, v)), y = unname(v))
}

# The columns a mutating join yields, named as dplyr names them: x's
# columns, each non-key that collides with a non-key of y suffixed, then y's
# non-key columns, suffixed where they collide with a column of x; y's keys
# are dropped, or with keep = TRUE kept and suffixed like the rest.
join_names <- function(x, y, keys, suffix, keep) {
  suffixed <- function(a, taken, sfx) {
    if (identical(sfx, "")) return(a)
    all <- c(taken, a)
    dup <- duplicated(all)
    while (any(dup)) { all[dup] <- paste0(all[dup], sfx); dup <- duplicated(all) }
    all[length(taken) + seq_along(a)]
  }
  if (keep) return(c(suffixed(x, y, suffix[[1L]]), suffixed(y, x, suffix[[2L]])))
  x_out <- x
  free <- !(x %in% keys$x)
  x_out[free] <- suffixed(x[free], c(keys$x, setdiff(y, c(keys$x, keys$y))), suffix[[1L]])
  c(x_out, suffixed(y, x, suffix[[2L]])[!(y %in% keys$y)])
}

# Verbs that keep the column set: the superseded scoped verbs with one
# function (a list() of functions adds columns named by type and function,
# which stays unknown), row samplers, tidyr's fillers and expanders.
kept_set_verbs <- c("replace_na", "na.omit", "separate_rows", "sample_n", "sample_frac", "top_n", "top_frac",
                    "complete", "add_row", "filter_at", "filter_if", "filter_all", "arrange_at", "arrange_if",
                    "arrange_all", "mutate_at", "mutate_if", "mutate_all")
# Verbs whose result is read off their arguments: a row-name column added or
# removed, columns added by name, united, renamed by toupper()/tolower().
computed_verbs <- c("rownames_to_column", "rowid_to_column", "column_to_rownames", "add_column", "unite",
                    "setNames", "rename_with", "rename_all")

apply_verb_impl <- function(call, cols, ctx, piped) {
  fn <- call_name(call)
  if (is.na(fn)) return(NULL)
  if (is_listed(cols)) { count_step(ctx, "skipped", fn); return(NULL) }
  count_step(ctx, if (is_open(cols)) "partial" else "checked", fn)
  args <- call_args(call)
  if (!piped && length(args)) args <- args[-1L]
  nms <- names(args) %||% rep("", length(args))
  # the verbs' own control arguments; any other dotted name is a column
  # (`mutate(.src = "a")` adds .src)
  dot_args <- nms %in% control_args
  pos <- args[!nzchar(nms)]
  named <- args[nzchar(nms) & !dot_args]
  group_cols <- attr(cols, "groups") %||% character()
  groups_unknown <- anyNA(group_cols)
  with_groups <- function(x, g = group_cols) { attr(x, "groups") <- g; keep_source(x, cols) }
  check_all <- function() check_symbols(unlist(lapply(c(pos, named), masked_symbols)), cols, ctx,
                                        unlist(lapply(c(pos, named), fn_arg_symbols)))
  # mutate(a = ..., b = a + 1): each argument sees the columns made before it
  check_sequential <- function() {
    cur <- cols
    for (i in seq_along(args)) {
      if (dot_args[[i]]) next
      check_symbols(masked_symbols(args[[i]]), cur, ctx, fn_arg_symbols(args[[i]]))
      if (nzchar(nms[[i]])) cur <- unique(c(cur, nms[[i]]))
    }
  }
  # across()/pick() as an unnamed argument: the columns it adds to a frame
  # that keeps the rest (mutate), or the columns it keeps when nothing else
  # survives (transmute, summarise); NULL when either cannot be worked out
  island_added <- function() {
    added <- character()
    for (p in pos) {
      if (is.symbol(p)) next
      a <- if (is.call(p)) across_names(p, ctx, cols) else NULL
      if (is.null(a)) return(NULL)
      added <- c(added, a)
    }
    added
  }
  island_kept <- function() {
    kept <- character()
    for (p in pos) {
      if (is.symbol(p)) { kept <- c(kept, as.character(p)); next }
      sel <- if (is.call(p)) island_selection(p, ctx, cols) else NULL
      a <- if (is.call(p)) across_names(p, ctx, cols) else NULL
      if (is.null(sel) || is.null(a)) return(NULL)
      kept <- c(kept, if (length(a)) a else sel)
    }
    kept
  }
  if (fn %in% nesting_verbs) {
    inner <- attr(cols, "inner") %||% list()
    col_arg <- if (fn == "hoist") (if (".col" %in% nms) args[[".col"]] else if (length(pos)) pos[[1L]] else NULL)
      else if ("cols" %in% nms) args[["cols"]] else if ("col" %in% nms) args[["col"]] else if (length(pos)) pos[[1L]] else NULL
    sel <- if (is.null(col_arg)) NULL else selection_names(list(col_arg), ctx, cols)
    if (is.null(sel)) return(NULL)
    bad <- check_symbols(sel, cols, ctx)
    if (length(bad)) return(NULL)
    if (fn == "unnest_longer") return(with_groups(cols))
    if (fn == "hoist") {
      # hoist(series, first = "d", second = list("x", 1)): each field names a
      # column inside the list-column; a string field is checked against the
      # recorded inner columns when they are known
      fields <- named[!(names(named) %in% c(".remove", ".simplify", ".ptype", ".transform"))]
      inn <- inner[[sel[[1L]]]]
      if (!is.null(inn)) {
        strings <- unlist(lapply(fields, function(f) if (is.character(f) && length(f) == 1L) f else
          if (is.call(f) && identical(call_name(f), "list") && length(f) >= 2L && is.character(f[[2L]])) f[[2L]] else NULL))
        if (length(strings)) check_symbols(strings, structure(inn, source = attr(cols, "source")), ctx)
      }
      return(with_groups(unique(c(cols, names(fields)))))
    }
    # unnest(), unnest_wider(): the list-columns are replaced by their inner columns
    if (!all(sel %in% names(inner))) return(NULL)
    res <- with_groups(unique(c(setdiff(cols, sel), unlist(inner[sel], use.names = FALSE))))
    attr(res, "inner") <- inner[setdiff(names(inner), sel)]
    return(res)
  }
  if (fn %in% kept_set_verbs) {
    if (fn %in% c("mutate_at", "mutate_if", "mutate_all")) {
      # a list() of functions adds a column per function and matching
      # column, named by both, which depends on the types: unknown
      slot <- if (fn == "mutate_all") 1L else 2L
      funs <- if (".funs" %in% nms) args[[".funs"]] else if (length(pos) >= slot) pos[[slot]] else NULL
      if (is.call(funs) && call_name(funs) %in% c("list", "funs", "c")) return(NULL)
    }
    if (grepl("_at$", fn)) {
      # the scoped verbs take functions, not columns; vars() is the one place
      # they name columns
      v <- if (".vars" %in% nms) args[[".vars"]] else if (length(pos)) pos[[1L]] else NULL
      sel <- if (is.call(v) && identical(call_name(v), "vars")) selection_names(call_args(v), ctx, cols) else NULL
      if (!is.null(sel)) check_symbols(sel, cols, ctx)
    } else if (!grepl("_(if|all)$", fn)) {
      check_all()
      if (fn == "add_row") check_symbols(names(named), cols, ctx)   # a new row cannot add columns
    }
    return(with_groups(cols))
  }
  if (fn %in% c("rownames_to_column", "rowid_to_column")) {
    var <- if (!is.null(args[["var"]])) string_vector(args[["var"]], ctx) else if (length(pos)) string_vector(pos[[1L]], ctx)
      else if (fn == "rowid_to_column") "rowid" else "rowname"
    if (!is.character(var) || length(var) != 1L) return(NULL)
    return(with_groups(unique(c(var, as.character(cols)))))
  }
  if (fn == "column_to_rownames") {
    var <- string_vector(if (!is.null(args[["var"]])) args[["var"]] else if (length(pos)) pos[[1L]] else "rowname", ctx)
    if (is.null(var) || length(var) != 1L || length(check_symbols(var, cols, ctx))) return(NULL)
    return(with_groups(setdiff(as.character(cols), var)))
  }
  if (fn == "add_column") {
    # its values are evaluated outside the frame, so no bare column names
    if (length(pos)) return(NULL)
    return(with_groups(unique(c(as.character(cols), names(named)))))
  }
  if (fn == "unite") {
    col <- if (!is.null(args[["col"]])) args[["col"]] else if (length(pos)) pos[[1L]] else NULL
    nm <- if (is.symbol(col) || is.character(col)) as.character(col) else NULL
    rest <- if (!is.null(args[["col"]])) pos else pos[-1L]
    if (is.null(nm) || (!length(rest) && is_open(cols))) return(NULL)
    sel <- if (length(rest)) selection_names(rest, ctx, cols) else as.character(cols)
    if (is.null(sel) || length(check_symbols(sel, cols, ctx))) return(NULL)
    remove <- args[["remove"]] %||% TRUE
    if (!isTRUE(remove) && !isFALSE(remove)) return(NULL)
    known <- as.character(cols)
    kept <- if (remove) setdiff(known, sel) else known
    # the new column sits where the first united one was
    return(with_groups(append(kept, nm, after = sum(match(kept, known) < min(match(sel, known))))))
  }
  if (fn == "setNames") {
    v <- string_vector(if (!is.null(args[["nm"]])) args[["nm"]] else if (length(pos)) pos[[1L]] else NULL, ctx)
    if (is.null(v) || is_open(cols) || length(v) != length(cols)) return(NULL)
    return(with_groups(v, if (groups_unknown) group_cols else v[match(group_cols, as.character(cols))]))
  }
  if (fn %in% c("rename_with", "rename_all")) {
    # the renaming function must be one the checker can run: toupper, tolower
    f <- if (!is.null(args[[".fn"]])) args[[".fn"]] else if (!is.null(args[[".funs"]])) args[[".funs"]] else if (length(pos)) pos[[1L]] else NULL
    f <- if (is.symbol(f) || (is.character(f) && length(f) == 1L)) as.character(f) else NULL
    if (is.null(f) || !(f %in% c("toupper", "tolower"))) return(NULL)
    sel_arg <- if (!is.null(args[[".cols"]])) args[[".cols"]] else if (fn == "rename_with" && length(pos) >= 2L) pos[[2L]] else NULL
    sel <- if (!is.null(sel_arg)) selection_names(list(sel_arg), ctx, cols) else if (!is_open(cols)) as.character(cols) else NULL
    if (is.null(sel) || length(check_symbols(sel, cols, ctx))) return(NULL)
    out <- as.character(cols)
    out[match(sel, out)] <- if (f == "toupper") toupper(sel) else tolower(sel)
    return(with_groups(out, if (groups_unknown) group_cols else out[match(group_cols, as.character(cols))]))
  }
  if (fn %in% terminal_calls) {
    # deframe() reads its first two columns (names, values) without naming them
    if (fn == "deframe" && !is.null(ctx$used_now)) ctx$used_now <- union(ctx$used_now, head(as.character(cols), 2L))
    if (fn == "pull") {
      var <- if ("var" %in% nms) args[["var"]] else if (length(pos)) pos[[1L]] else NULL
      sel <- if (is.null(var) || is.numeric(var)) NULL else selection_names(list(var), ctx, cols)
      if (!is.null(sel)) check_symbols(sel, cols, ctx)
    }
    return(not_frame)
  }
  # `.by =` is the one dot-argument that names columns of the incoming frame
  # (bare, `c(a, b)`, a helper); check it like group_by()'s arguments. When a
  # name is wrong the result is unknown: the pipeline fails there at run time,
  # and carrying the misspelling into the output would only flag the correct
  # name downstream.
  by_sel <- if (".by" %in% nms) selection_names(list(args[[".by"]]), ctx, cols) else NULL
  if (!is.null(by_sel) && length(check_symbols(by_sel, cols, ctx))) return(NULL)

  if (fn %in% c("filter", "arrange", "slice", "slice_head", "slice_tail", "slice_min", "slice_max",
                "slice_sample", "drop_na", "fill", "relocate", "ungroup", "rowwise", "glimpse", "print",
                "head", "tail", "collect", "compute", "as_tibble", "as.data.frame")) {
    check_all()
    return(with_groups(cols, if (fn == "ungroup") character() else group_cols))
  }
  if (fn == "group_by") {
    check_all()
    g <- grouping_names(pos, ctx, cols)
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
    if (fn == "mutate") {
      added <- island_added()
      if (is.null(added)) return(NULL)
      return(with_groups(unique(c(cols, names(named), added))))
    }
    kept <- island_kept()
    if (is.null(kept) || groups_unknown) return(NULL)
    return(closed(with_groups(unique(c(group_cols, kept, names(named))))))
  }
  if (fn %in% c("summarise", "summarize", "reframe")) {
    check_sequential()
    kept <- island_kept()
    if (is.null(kept)) return(NULL)
    by <- if (".by" %in% nms) by_sel else group_cols
    if (is.null(by) || anyNA(by)) return(NULL)
    # the result's groups as dplyr leaves them: all but the last grouping
    # column by default (.groups = "drop_last"), none for "drop", `.by` and
    # reframe(), all of them for "keep" and "rowwise"; a .groups the code
    # does not state literally leaves them unknown
    groups <- if (fn == "reframe" || ".by" %in% nms) character() else {
      how <- if (".groups" %in% nms) string_vector(args[[".groups"]], ctx) else "drop_last"
      if (!is.character(how) || length(how) != 1L || is.na(how)) how <- NA_character_
      switch(how, drop_last = head(group_cols, -1L), drop = character(), keep = , rowwise = group_cols, NA_character_)
    }
    return(closed(with_groups(unique(c(by, kept, names(named))), groups)))
  }
  if (fn == "select") {
    sel <- selection_names(c(pos, unname(named)), ctx, cols)
    if (is.null(sel)) {
      check_symbols(unlist(lapply(pos, masked_symbols)), cols, ctx, unlist(lapply(pos, fn_arg_symbols)))
      return(NULL)
    }
    check_symbols(sel, cols, ctx)
    neg <- any(vapply(pos, function(p) is.call(p) && call_name(p) %in% c("-", "!"), TRUE))
    renamed_from <- unlist(lapply(named, function(a) if (is.symbol(a) || is.character(a)) as.character(a) else NULL))
    kept <- if (neg) setdiff(cols, sel) else c(names(named), sel[!(sel %in% renamed_from)])
    return(if (neg) with_groups(unique(kept)) else closed(with_groups(unique(kept))))
  }
  if (fn == "rename") {
    old <- vapply(named, function(a) if (is.symbol(a) || is.character(a)) as.character(a) else NA_character_, "")
    if (anyNA(old) || !length(old)) return(NULL)
    check_symbols(old, cols, ctx)
    return(with_groups(unique(c(setdiff(cols, old), names(named)))))
  }
  if (fn %in% c("count", "tally", "add_count")) {
    check_all()
    g <- selection_names(pos, ctx, cols)
    if (is.null(g)) return(NULL)
    nm <- if (!is.null(args[["name"]])) string_arg(args[["name"]]) %||% "n" else "n"
    if (fn != "add_count" && groups_unknown) return(NULL)
    # count() and add_count() keep the frame's groups; tally() is a
    # summarise(), which drops the last one
    return(if (fn == "add_count") with_groups(unique(c(cols, nm)))
           else closed(with_groups(unique(c(group_cols, g, nm)), if (fn == "tally") head(group_cols, -1L) else group_cols)))
  }
  if (fn == "distinct") {
    check_all()
    g <- selection_names(pos, ctx, cols)
    if (is.null(g)) return(NULL)
    if (!length(g) || isTRUE(args[[".keep_all"]] %||% FALSE)) return(with_groups(cols))
    if (groups_unknown) return(NULL)
    return(closed(with_groups(unique(c(group_cols, g)))))
  }
  # nest(): the nested columns leave the frame and a list-column takes their
  # place. `nest(data = -id)`, `nest(data = c(a, b))`, several named
  # selections, `nest(.by = id)` (the rest nested under `.key`, "data" by
  # default), `nest()` on a grouped frame (the groups stay, the rest is
  # nested). `.by` together with a named selection, a negated selection on a
  # partly known frame, and `nest()` on an ungrouped frame stay unknown.
  if (fn == "nest") {
    by <- if (".by" %in% nms) selection_names(list(args[[".by"]]), ctx, cols) else NULL
    if (".by" %in% nms && (is.null(by) || length(check_symbols(by, cols, ctx)))) return(NULL)
    key <- if (".key" %in% nms) string_vector(args[[".key"]], ctx) else "data"
    if (is.null(key) || length(key) != 1L) return(NULL)
    if (length(named)) {
      if (!is.null(by)) return(NULL)
      nested <- character()
      for (a in named) {
        sel <- selection_names(list(a), ctx, cols)
        if (is.null(sel) || length(check_symbols(sel, cols, ctx))) return(NULL)
        neg <- is.call(a) && call_name(a) %in% c("-", "!")
        if (neg && is_open(cols)) return(NULL)
        nested <- c(nested, if (neg) setdiff(as.character(cols), sel) else sel)
      }
      # the nested columns are consumed into the list-column: a use, for r-colmap
      if (!is.null(ctx$used_now)) ctx$used_now <- union(ctx$used_now, nested)
      return(with_groups(unique(c(setdiff(as.character(cols), nested), names(named)))))
    }
    if (!is.null(ctx$used_now) && !is_open(cols)) ctx$used_now <- union(ctx$used_now, setdiff(as.character(cols), c(by, group_cols)))
    if (!is.null(by)) return(closed(with_groups(unique(c(by, key)), character())))
    if (!length(group_cols) || groups_unknown) return(NULL)
    return(closed(with_groups(unique(c(group_cols, key)))))
  }
  if (fn == "pivot_longer") {
    spec <- if (length(pos)) pos[[1L]] else args[["cols"]]
    if (is.null(spec)) return(NULL)
    sel <- selection_names(list(spec), ctx, cols)
    if (is.null(sel)) return(NULL)
    check_symbols(sel, cols, ctx)
    neg <- is.call(spec) && call_name(spec) %in% c("-", "!")
    gone <- if (neg) setdiff(cols, sel) else sel
    nt <- if ("names_to" %in% nms) string_vector(args[["names_to"]], ctx) else "name"
    vt <- if ("values_to" %in% nms) string_vector(args[["values_to"]], ctx) else "value"
    if (is.null(nt) || is.null(vt) || length(vt) != 1L) return(NULL)
    nt <- nt[!is.na(nt)]
    if (".value" %in% nt) {
      # names_to = c(".value", "set"), names_sep = "_": the pivoted columns'
      # names are split and the pieces at .value become the value columns;
      # known when the separator is a literal and every pivoted column is seen
      sep <- string_vector(args[["names_sep"]], ctx)
      if (!is.character(sep) || length(sep) != 1L || is_open(cols) || "names_pattern" %in% nms) return(NULL)
      pieces <- quiet(regmatches(gone, gregexpr(sep, gone, perl = TRUE), invert = TRUE))
      if (!length(pieces) || any(lengths(pieces) != length(nt))) return(NULL)
      vals <- unique(vapply(pieces, `[[`, "", which(nt == ".value")[[1L]]))
      return(with_groups(unique(c(setdiff(cols, gone), setdiff(nt, ".value"), vals))))
    }
    return(with_groups(unique(c(setdiff(cols, gone), nt, vt))))
  }
  if (fn %in% c("left_join", "right_join", "inner_join", "full_join", "semi_join", "anti_join")) {
    # a key that is not a column stops the pipeline at run time: reported
    # here, and nothing downstream
    by_arg <- if (!is.null(args[["by"]])) args[["by"]] else if (length(pos) >= 2L) pos[[2L]] else NULL
    if (length(pos) >= 3L) return(NULL)   # copy, suffix given by position: not read
    keys <- if (!is.null(by_arg)) join_keys(by_arg, ctx) else NULL
    if (!is.null(keys) && length(check_symbols(keys$x, cols, ctx))) return(NULL)
    if (fn %in% c("semi_join", "anti_join")) return(with_groups(cols))
    other <- as_frame(frame_columns(if (length(pos)) pos[[1L]] else args[["y"]], ctx, used = TRUE))
    if (is.null(other) || is_listed(other)) return(NULL)
    if (!is.null(keys) && !is_open(other)) {
      saved_used <- ctx$used_now   # y's keys are read from y, not from this frame
      bad <- check_symbols(keys$y, other, ctx)
      ctx$used_now <- saved_used
      if (length(bad)) return(NULL)
    }
    # a `by` the checker cannot read may rename anything: unknown. With no
    # `by` the keys are the common columns, so nothing is renamed; on a
    # partly known frame they cannot be told, and the union stays open
    if (is.null(keys) && !is.null(by_arg)) return(NULL)
    open <- is_open(cols) || is_open(other)
    if (is.null(keys) && !open) { k <- intersect(as.character(cols), as.character(other)); keys <- list(x = k, y = k) }
    joined <- if (is.null(keys)) unique(c(as.character(cols), as.character(other))) else {
      suffix <- if (!is.null(args[["suffix"]])) string_vector(args[["suffix"]], ctx) else c(".x", ".y")
      keep <- args[["keep"]]
      if (is.null(suffix) || length(suffix) != 2L || !(is.null(keep) || isTRUE(keep) || isFALSE(keep))) return(NULL)
      join_names(as.character(cols), as.character(other), keys, suffix, isTRUE(keep))
    }
    # a grouping column in the overlap is renamed with the rest
    groups <- if (groups_unknown || is.null(keys)) group_cols else joined[match(group_cols, as.character(cols))]
    joined <- with_groups(joined, groups)
    if (open) attr(joined, "open") <- TRUE
    # for the data map: the other frame, and the columns it brought into the
    # result as they are named there (an x column renamed with the x suffix is
    # not one of them)
    sfx <- if (!is.null(args[["suffix"]])) string_vector(args[["suffix"]], ctx) else c(".x", ".y")
    x_suffixed <- if (!is.null(keys) && length(sfx) == 2L) paste0(setdiff(as.character(cols), keys$x), sfx[[1L]]) else character()
    ctx$joined_now <- list(source = attr(other, "source"), source_line = attr(other, "source_line"),
                           columns = as.character(other), keys = keys$y %||% character(),
                           brought = setdiff(setdiff(as.character(joined), as.character(cols)), x_suffixed))
    return(joined)
  }
  if (fn %in% c("pivot_wider")) {
    for (k in c("names_from", "values_from", "id_cols")) if (!is.null(args[[k]])) {
      s <- selection_names(list(args[[k]]), ctx, cols)
      if (!is.null(s)) check_symbols(s, cols, ctx)
    }
    return(NULL)
  }
  if (fn == "ggplot") {
    for (a in c(pos, named)) if (is.call(a) && identical(call_name(a), "aes")) check_symbols(masked_symbols(a), cols, ctx, fn_arg_symbols(a))
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
  cols <- as_frame(frame_columns(base[[2L]], ctx, used = TRUE))
  if (is.null(cols)) return(invisible())
  layers <- list()
  node <- e
  while (is.call(node) && identical(call_name(node), "+")) {
    layers <- c(layers, list(node[[3L]]))
    node <- node[[2L]]
  }
  # the plot is a step that reads columns and keeps the frame as it is
  shown <- base
  if (length(shown) >= 2L && is.call(shown[[2L]])) shown[[2L]] <- as.name(".")
  saved_used <- ctx$used_now
  saved_step <- ctx$step_call
  ctx$used_now <- character()
  ctx$step_call <- one_line(shown)
  for (l in c(list(base), layers)) {
    if (!is.call(l)) next
    for (a in as.list(l)[-1L]) if (is.call(a) && identical(call_name(a), "aes")) check_symbols(masked_symbols(a), cols, ctx, fn_arg_symbols(a))
  }
  record_step(ctx, shown, cols, before = cols, used = ctx$used_now)
  ctx$used_now <- saved_used
  ctx$step_call <- saved_step
  invisible()
}

# Verb calls the walker checks when they appear on their own.
verbs <- c("mutate", "filter", "summarise", "summarize", "select", "arrange", "group_by", "rename",
           "transmute", "distinct", "count", "pivot_longer", "pivot_wider", "ggplot", "slice", "reframe",
           "left_join", "inner_join", "right_join", "full_join", "semi_join", "anti_join", "pull",
           "unnest", "unnest_wider", "unnest_longer", "hoist", "nest")

# Everything apply_verb() understands: a call to one of these is a pipeline
# step, never a data source.
verb_names <- unique(c(verbs, "slice_head", "slice_tail", "slice_min", "slice_max", "slice_sample",
                       "drop_na", "fill", "relocate", "ungroup", "rowwise", "glimpse", "print", "head",
                       "tail", "collect", "compute", "as_tibble", "as.data.frame", "tally", "add_count",
                       kept_set_verbs, computed_verbs, terminal_calls))
column_verbs <- setdiff(verb_names, setdiff(terminal_calls, "pull"))

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
  if (!is.na(fn) && fn %in% ctx$assert_fns) {
    assertion <- assertion_of(e, ctx)
    if (!is.null(assertion)) apply_assertion(assertion, ctx)
  }
  # df$x <- v, df[["x"]] <- v, df[, "x"] <- v, names(df) <- v: the frame's
  # columns change in place (a new one, or the whole set); a name the checker
  # cannot read makes the frame unknown from here on
  if (!is.na(fn) && fn %in% c("<-", "=", "<<-") && length(e) == 3L && is.call(e[[2L]])) {
    if (is.call(e[[3L]])) walk_expr(e[[3L]], ctx)
    # the target's row index may read columns (`df[df$y > 1, "x"] <- 0`); the
    # target itself is not a read
    for (p in call_args(e[[2L]])[-1L]) if (is.call(p)) walk_expr(p, ctx)
    assign_columns(e[[2L]], e[[3L]], ctx)
    return(invisible())
  }
  if (!is.na(fn) && fn %in% c("<-", "=", "<<-") && length(e) == 3L && is.symbol(e[[2L]])) {
    target <- as.character(e[[2L]])
    ctx$strings[[target]] <- if (is.character(e[[3L]]) || is.call(e[[3L]])) string_vector(e[[3L]], ctx) else NULL
    if (is.call(e[[3L]]) && identical(call_name(e[[3L]]), "function")) {
      # a named function: check its body now (its own loaders and globals
      # resolve; a parameter used as a frame is an open set); calls with known
      # frames check it again
      ctx$variables <- union(ctx$variables, target)
      ctx$def_name <- target
      walk_expr(e[[3L]], ctx)
      ctx$def_name <- NULL   # also when the body walk failed before the function branch cleared it
      return(invisible())
    }
    key <- line_annotation(ctx)
    ctx$annotation <- key
    ctx$annotation_head <- pipeline_head(e[[3L]])
    ctx$stopped <- NULL
    cols <- frame_columns(e[[3L]], ctx)
    # an annotation no source consumed (the right-hand side is a symbol, a
    # user function, a split/map statement) names the columns of the result:
    # the user's statement of what the line assigns beats what was inferred
    if (!is.null(ctx$annotation) && !is_not_frame(cols)) cols <- schema_columns(ctx$annotation, ctx) %||% cols
    ctx$annotation <- NULL
    ctx$annotation_head <- NULL
    if (is_not_frame(cols)) {
      ctx$frames[[target]] <- NULL
      ctx$unresolved[[target]] <- NULL
      ctx$unknown_frames <- setdiff(ctx$unknown_frames, target)
      ctx$variables <- union(ctx$variables, target)
      check_ggplot(e[[3L]], ctx)
      if (is.call(e[[3L]]) && !identical(call_name(e[[3L]]), "function")) walk_expr(e[[3L]], ctx)
      return(invisible())
    }
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
    name <- ctx$def_name
    ctx$def_name <- NULL
    params <- names(e[[2L]]) %||% character()
    body <- e[[3L]]
    saved_vars <- ctx$variables
    saved_unknown <- ctx$unknown_frames
    saved_frames <- ctx$frames
    saved_strings <- ctx$strings
    ctx$variables <- union(ctx$variables, params)
    ctx$unknown_frames <- union(ctx$unknown_frames, params)
    for (p in params) ctx$frames[[p]] <- NULL   # a parameter shadows a frame of the same name
    # In a named function, a parameter the body pipes into a verb is a frame
    # whose columns are not known: an open set ("at least these"), so the
    # steps on it are followed, the names the body reads and creates are
    # recorded, and nothing is reported against it. The data map lists it as
    # a source of its own, "parameter df of f()".
    if (!is.null(name)) for (p in params) if (param_used_as_frame(body, p)) {
      ctx$frames[[p]] <- structure(character(), open = TRUE, source = sprintf("parameter %s of %s()", p, name))
      ctx$unknown_frames <- setdiff(ctx$unknown_frames, p)
    }
    saved_in <- ctx$in_function
    ctx$in_function <- TRUE
    if (is.null(name)) walk_expr(body, ctx) else {
      # the frame a named function returns went to its callers: recorded as a
      # fate, which the data map reports when no caller is followed from a
      # known frame
      result <- as_frame(body_columns(body, ctx))
      if (!is.null(result) && !is_listed(result) && !is.null(attr(result, "source"))) {
        srcs <- attr(body, "srcref")
        last_line <- if (is.call(body) && identical(call_name(body), "{") && !is.null(srcs) && length(srcs) > 1L)
          as.integer(srcs[[length(srcs)]][[1L]]) else ctx$line
        record_step(ctx, sprintf("return from %s()", name), result, before = result, fate = "returned",
                    fate_fn = name, line = last_line)
      }
    }
    ctx$in_function <- saved_in
    ctx$variables <- saved_vars
    ctx$unknown_frames <- saved_unknown
    ctx$frames <- saved_frames
    ctx$strings <- saved_strings
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
  # `x %<>% f()` is `x <- x %>% f()`
  if (!is.na(fn) && fn == "%<>%" && length(e) == 3L && is.symbol(e[[2L]])) {
    return(walk_expr(call("<-", e[[2L]], call("%>%", e[[2L]], e[[3L]])), ctx))
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
  if (!is.na(fn) && fn == "if" && length(e) >= 3L && walk_guarded_if(e, ctx)) return(invisible())
  if (!is.na(fn) && fn %in% c("if", "for", "while", "repeat")) return(walk_branches(e, fn, ctx))
  if (!is.na(fn) && fn %in% c("$", "[[", "[", "setnames")) check_access(e, ctx)
  if (!is.na(fn) && !is.null(ctx$functions[[fn]])) check_call_args(e, fn, ctx)
  if (!is.na(fn) && fn %in% sink_calls && is.null(ctx$functions[[fn]])) record_sink(e, fn, ctx)
  if (!is.na(fn) && (fn %in% verbs || !is.null(ctx$functions[[fn]]))) frame_columns(e, ctx)
  for (p in call_args(e)) if (is.call(p)) walk_expr(p, ctx)
  invisible()
}

# A frame reassigned inside a branch or a loop body may or may not have been
# reassigned once the block is over (`if (drop) df <- select(df, -x)`): after
# it the frame has the columns of either outcome, so a name is reported only
# when neither has them. The else branch starts from the frames the `if`
# started from, not from the then branch's. A frame one outcome left unknown
# is unknown; one assigned in a branch only is taken as assigned.
walk_branches <- function(e, fn, ctx) {
  parts <- as.list(e)[-1L]
  heads <- switch(fn, "if" = parts[1L], "for" = parts[2L], "while" = parts[1L], list())   # condition, sequence
  bodies <- switch(fn, "if" = parts[-1L], "for" = parts[3L], "while" = parts[2L], parts)
  for (h in heads) if (is.call(h)) walk_expr(h, ctx)
  if (fn == "for" && is.symbol(parts[[1L]])) ctx$variables <- union(ctx$variables, as.character(parts[[1L]]))
  before <- ctx$frames
  before_unknown <- ctx$unknown_frames
  outcomes <- list()
  for (b in bodies) {
    ctx$frames <- before
    ctx$unknown_frames <- before_unknown
    if (is.call(b)) walk_expr(b, ctx)
    outcomes[[length(outcomes) + 1L]] <- list(frames = ctx$frames, unknown = ctx$unknown_frames)
  }
  # an if without else, a loop body: the block may have done nothing
  if (fn != "if" || length(bodies) < 2L) outcomes <- c(list(list(frames = before, unknown = before_unknown)), outcomes)
  a <- outcomes[[1L]]$frames
  b <- outcomes[[2L]]$frames
  merged <- list()
  for (nm in union(names(a), names(b))) {
    fa <- a[[nm]]
    fb <- b[[nm]]
    if (is.null(fa) || is.null(fb)) {
      if (is.null(before[[nm]])) merged[[nm]] <- fa %||% fb
      next
    }
    if (identical(fa, fb)) { merged[[nm]] <- fa; next }
    if (is_listed(fa) || is_listed(fb)) next
    m <- keep_source(unique(c(as.character(fa), as.character(fb))), fa)
    if (is_open(fb)) attr(m, "open") <- TRUE
    ga <- attr(fa, "groups") %||% character()
    attr(m, "groups") <- if (identical(ga, attr(fb, "groups") %||% character())) ga else NA_character_
    merged[[nm]] <- m
  }
  ctx$frames <- merged
  ctx$unknown_frames <- union(union(outcomes[[1L]]$unknown, outcomes[[2L]]$unknown), setdiff(union(names(a), names(b)), names(merged)))
  invisible()
}


# --- base-R column access ----------------------------------------------------
# `df$x`, `df[["x"]]`, `df[, "x"]`, `df[c("a", "b")]` on a frame with known
# columns name its columns as surely as a verb does, and a wrong name there
# is quieter at run time (NULL, not an error), so it is reported the same
# way. Only literal names are read (a variable bound to a literal counts);
# `df[[i]]` with a number or an expression is left alone, as is a list of
# frames (split()), where the name picks a group.
frame_of <- function(e, ctx) {
  if (length(e) < 2L || !is.symbol(e[[2L]])) return(NULL)
  cols <- ctx$frames[[as.character(e[[2L]])]]
  if (is.null(cols) || is_listed(cols) || is_not_frame(cols)) NULL else cols
}

# The column names a `$`/`[[`/`[` call names literally, or NULL.
access_names <- function(e, ctx) {
  fn <- call_name(e)
  if (fn == "$" && length(e) == 3L) {
    x <- e[[3L]]
    return(if (is.symbol(x)) as.character(x) else if (is.character(x) && length(x) == 1L) x else NULL)
  }
  if (fn == "[[" && length(e) == 3L) return(string_vector(e[[3L]], ctx))
  if (fn == "[") {
    # the column slot: `df["x"]`, `df[rows, "x"]`; an empty one (`df[rows, ]`)
    # is never bound to a variable, which would make that variable "missing"
    k <- if (length(e) == 3L) 3L else if (length(e) >= 4L) 4L else return(NULL)
    if (identical(e[[k]], quote(expr = ))) return(NULL)
    return(string_vector(e[[k]], ctx))
  }
  NULL
}

check_access <- function(e, ctx) {
  fn <- call_name(e)
  cols <- frame_of(e, ctx)
  if (is.null(cols)) return(invisible())
  nm <- as.character(e[[2L]])
  # data.table: `df[, x := ...]` adds x in place, setnames() renames
  if (fn == "[") {
    walrus <- Filter(function(a) is.call(a) && identical(call_name(a), ":="), call_args(e))
    if (length(walrus)) {
      new <- if (length(walrus[[1L]]) == 3L) { lhs <- walrus[[1L]][[2L]]; if (is.symbol(lhs)) as.character(lhs) else string_vector(lhs, ctx) } else NULL
      set_frame(nm, if (is.null(new)) NULL else unique(c(as.character(cols), new)), cols, ctx)
      return(invisible())
    }
  }
  if (fn == "setnames") {
    args <- call_args(e)
    old <- if (length(args) >= 3L) string_vector(args[[2L]], ctx) else NULL
    new <- if (length(args) >= 3L) string_vector(args[[3L]], ctx) else NULL
    ok <- !is.null(old) && !is.null(new) && length(old) == length(new) && all(old %in% cols)
    set_frame(nm, if (ok) replace(as.character(cols), match(old, cols), new) else NULL, cols, ctx)
    return(invisible())
  }
  names <- access_names(e, ctx)
  if (is.character(names) && length(names)) check_symbols(names, cols, ctx)
  invisible()
}

set_frame <- function(nm, new, cols, ctx) {
  if (is.null(new)) {
    ctx$frames[[nm]] <- NULL
    ctx$unknown_frames <- union(ctx$unknown_frames, nm)
  } else {
    groups <- attr(cols, "groups") %||% character()
    ctx$frames[[nm]] <- keep_source(structure(new, groups = groups[groups %in% new]), cols)
  }
  invisible()
}

# `df$x <- v` and friends: the columns of df after the assignment.
assign_columns <- function(target, value, ctx) {
  fn <- call_name(target)
  if (is.na(fn)) return(invisible())
  if (fn %in% c("names", "colnames") && length(target) == 2L) {
    cols <- frame_of(target, ctx)
    if (is.null(cols)) return(invisible())
    new <- string_vector(value, ctx)
    set_frame(as.character(target[[2L]]), if (is.null(new)) NULL else new, cols, ctx)
    return(invisible())
  }
  if (!(fn %in% c("$", "[[", "["))) return(invisible())
  # `names(df)[2] <- "x"`: one name changes, which one is not followed
  if (length(target) >= 2L && is.call(target[[2L]]) && call_name(target[[2L]]) %in% c("names", "colnames") && length(target[[2L]]) == 2L) {
    cols <- frame_of(target[[2L]], ctx)
    if (!is.null(cols)) set_frame(as.character(target[[2L]][[2L]]), NULL, cols, ctx)
    return(invisible())
  }
  cols <- frame_of(target, ctx)
  if (is.null(cols)) return(invisible())
  names <- access_names(target, ctx)
  nm <- as.character(target[[2L]])
  if (is.null(names)) {
    # `df[rows, ] <- v` keeps the columns; `df[[expr]] <- v` names one the checker cannot read
    row_only <- fn == "[" && length(target) == 4L && identical(target[[4L]], quote(expr = ))
    if (!row_only) set_frame(nm, NULL, cols, ctx)
    return(invisible())
  }
  removed <- is.null(value)
  set_frame(nm, if (removed) setdiff(as.character(cols), names) else unique(c(as.character(cols), names)), cols, ctx)
  invisible()
}

# The packages a DESCRIPTION depends on or imports, by name.
description_imports <- function(root) {
  p <- file.path(root, "DESCRIPTION")
  if (!file.exists(p)) return(character())
  d <- quiet(read.dcf(p, fields = c("Depends", "Imports")))
  if (is.null(d)) return(character())
  deps <- unlist(strsplit(paste(d[1L, ], collapse = ","), ","))
  deps <- trimws(sub("\\(.*$", "", deps))
  deps[nzchar(deps) & deps != "R" & !is.na(deps)]
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
  ctx$config <- load_config(ctx$root)
  ctx$assert_fns <- c("stopifnot", "assert_that", "if", "assert_names", "assert_subset",
                      vapply(ctx$config$assertions %||% list(), function(a) sub("^.*::", "", a$fn %||% ""), ""))
  ctx$strings <- list()
  ctx$unresolved <- list()
  ctx$stale <- list()
  ctx$annotation <- NULL
  ctx$src_lines <- quiet(readLines(file, warn = FALSE), character())
  ctx$line_cache <- list()
  ctx$line_end <- NA_integer_
  ctx$stats <- list(checked = character(), partial = character(), skipped = character())
  ctx$steps <- list()
  ctx$follow_known <- logical()
  ctx$pkg_name <- if (file.exists(file.path(ctx$root, "DESCRIPTION")))
    quiet(unname(read.dcf(file.path(ctx$root, "DESCRIPTION"), fields = "Package")[1L, 1L])) else NULL
  ctx$imports <- description_imports(ctx$root)
  ctx$fn_memo <- list()
  ctx$known_names <- c("TRUE", "FALSE", "NA", "NULL", "T", "F", "Inf", "NaN", ".", ".x", ".y", ".data", ".env",
                       "desc", "n", "everything", "where", "across", "pi", "letters", "LETTERS",
                       "month.name", "month.abb", "self", "private")
  parsed <- tryCatch(suppressWarnings(parse_r_source(file)), error = function(err) err)
  if (inherits(parsed, "error")) return(syntax_finding(file, parsed))
  exprs <- parsed$exprs
  src <- attr(exprs, "srcref")
  # A chunk of a literate file that does not parse: said in its own finding;
  # the other chunks are checked.
  for (e in parsed$errors) ctx$findings[[length(ctx$findings) + 1L]] <- syntax_finding(file, e$cond, chunk = e$line)[[1L]]
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
  # the walker can visit a pipeline twice (assignment and its right-hand side)
  # (the same step reached from a parameter's open frame and from a caller's
  # known one is two steps: the source tells them apart)
  step_keys <- vapply(ctx$steps, function(s) paste(s$file, s$line, s$call, s$source, isTRUE(s$open_before)), "")
  structure(c(ctx$findings, source_notes(ctx), error_findings(ctx)),
            stats = list(checked = length(ctx$stats$checked), partial = length(ctx$stats$partial),
                         skipped = length(ctx$stats$skipped)),
            steps = ctx$steps[!duplicated(step_keys)])
}

# A file that does not parse: said in its own finding, on the line the parser
# names, so that an agent that has just broken the file does not read the
# checker's silence as "clean". The parser's message already carries
# file:line:col, the offending line and a caret.
syntax_finding <- function(file, err, chunk = NULL) {
  msg <- conditionMessage(err)
  m <- regmatches(msg, regexec("^(?:.*?):(\\d+):(\\d+): *([^\n]*)", msg, perl = TRUE))[[1L]]
  line <- if (length(m) >= 2L) as.integer(m[[2L]]) else chunk %||% 1L
  what <- if (length(m) >= 4L) sprintf("%s:%s: %s", m[[2L]], m[[3L]], m[[4L]]) else strsplit(msg, "\n")[[1L]][[1L]]
  rest <- strsplit(msg, "\n")[[1L]][-1L]
  structure(list(list(
    kind = "syntax", file = file, line = line, name = NA_character_, source = NA_character_, columns = character(),
    message = sprintf("%s does not parse (%s), so nothing in %s was checked%s",
                      if (is.null(chunk)) "this file" else sprintf("the R chunk starting at line %d", chunk), what,
                      if (is.null(chunk)) "it" else "that chunk",
                      if (length(rest)) paste0(":\n", paste(rest, collapse = "\n")) else ""))),
    stats = list(checked = 0L, partial = 0L, skipped = 0L, unparsed = 1L))
}

# Notes about data sources: frames the checker could not resolve that are
# then piped into verbs, and cache entries older than 90 days.
source_notes <- function(ctx) {
  notes <- list()
  note <- function(line, name, message, ...) {
    notes[[length(notes) + 1L]] <<- c(list(kind = "note", file = ctx$file, line = line, name = name,
                                            source = NA_character_, columns = character(), message = message), list(...))
  }
  for (nm in names(ctx$unresolved)) {
    u <- ctx$unresolved[[nm]]
    if (!isTRUE(u$used)) next
    if (!is.null(u$stopped)) {
      s <- u$stopped
      # a project or package function: cache what it returns, by name when
      # its output is fixed, by name and argument when the output depends on one
      remedy <- if (is.null(s$key)) "A `# cols: KEY` comment on this line covers it: learn KEY from an expression that returns the finished table."
        else sprintf("If %s() returns the same columns every call, cache it once by name: r-colcheck learn \"%s\" '<an expression that returns its table>' (every call that passes no string literal then resolves); if its columns depend on an argument, key the entry on that argument: r-colcheck learn '%s(arg = \"value\")' '<expr>', the argument written as the calls pass it, named or positional. A `# cols: KEY` comment on this line covers it too.",
                     s$key, s$key, s$key)
      note(u$line, nm, sprintf(
        "'%s' is known up to %s on line %d, whose output columns the checker cannot work out (%s), so column names downstream are not checked. %s",
        nm, s$expr, s$line, s$reason, remedy))
      next
    }
    if (!is.null(u$package_fn)) {
      learn_expr <- if (isTRUE(u$self_contained)) gsub("'", "'\\\\''", u$expr) else "<an expression that returns its table>"
      note(u$line, nm, sprintf(
        "'%s' comes from %s(), whose output columns could not be read from its code (%s), so column names downstream are not checked. If it returns the same columns every call, cache it once by name: r-colcheck learn \"%s\" '%s' (every call that passes no string literal then resolves); if its columns depend on an argument, add `# cols: KEY` on this line and learn KEY instead.",
        nm, u$package_fn, u$expr, u$package_fn, learn_expr),
        key = u$package_fn, expr = u$expr,
        command = if (isTRUE(u$self_contained)) learn_command(u$package_fn, u$expr) else NULL)
      next
    }
    cached <- unique(u$keys[u$keys %in% names(ctx$schemas)])
    extra <- u$learn$extra %||% character()
    if (length(cached) > 1L) {
      # every literal is cached, and they disagree: the entry to learn is
      # the call's own, not one of its arguments' (which must not be overwritten)
      k <- gsub("'", "'\\\\''", u$expr)
      note(u$line, nm, sprintf(
        "'%s' comes from %s, which names %d cached datasets (%s); the checker cannot tell which columns come out, so column names downstream are not checked. Add `# cols: KEY` on this line, or key an entry on the call itself: r-colcheck learn '%s' '%s'%s",
        nm, u$expr, length(cached), paste(sprintf("\"%s\"", cached), collapse = ", "), k, k,
        if (length(extra)) paste0(" ", paste(extra, collapse = " ")) else ""),
        keys = I(cached), expr = u$expr)
      next
    }
    keys <- u$keys[!(u$keys %in% names(ctx$schemas))]
    key <- if (length(keys)) keys[[1L]] else if (length(u$keys)) u$keys[[1L]] else nm
    learn_expr <- u$learn$expr %||% u$expr
    command <- learn_command(key, learn_expr, extra)
    note(u$line, nm, sprintf(
      "'%s' comes from a source the checker cannot open (%s%s); no cached schema for key \"%s\", so column names downstream are not checked. Learn it once: %s%s",
      nm, u$expr, if (is.null(u$why)) "" else paste0(": ", u$why), key, command,
      if (!is.null(u$learn$why) && (!identical(learn_expr, u$expr) || length(extra))) sprintf(" (%s)", u$learn$why) else ""),
      key = key, expr = u$expr, learn_expr = learn_expr, learn_args = I(extra), command = command)
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

# The R and literate files of a project, library and VCS internals left out
# (the call map, the session context and `discover` share this).
# inst/doc holds built copies of the vignettes and *.Rcheck a copy of the
# package: the same code twice.
project_skip_dirs <- "/(renv|packrat|[.]git|node_modules|inst/doc|[^/]*[.]Rcheck)/"
project_r_files <- function(root) {
  r <- list.files(root, pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE)
  m <- list.files(root, pattern = "[.](Rmd|rmd|Rmarkdown|qmd|Rnw|rnw)$", recursive = TRUE, full.names = TRUE)
  list(r = r[!grepl(project_skip_dirs, r)], rmd = m[!grepl(project_skip_dirs, m)])
}

# Every data source in the project the checker cannot open, with where it is
# used and the learn command for it; the cached entries with their age and
# where their key is mentioned. Static: nothing is evaluated.
colcheck_discover <- function(root) {
  root <- normalizePath(root, mustWork = FALSE)
  found <- project_r_files(root)
  files <- c(found$r, found$rmd)
  findings <- colcheck_files(files, root)
  schemas <- load_schemas(root)
  ttl <- ttl_days(load_config(root))
  sources <- list()
  for (x in findings) {
    if (!identical(x$kind, "note") || is.null(x$key) || is.null(x$command)) next
    s <- sources[[x$key]] %||% list(key = x$key, expr = x$expr, learn_expr = x$learn_expr %||% x$expr,
                                     learn_args = as.character(x$learn_args %||% character()), command = x$command, sites = character())
    s$sites <- unique(c(s$sites, sprintf("%s:%d", relative_path(x$file, root), x$line)))
    sources[[x$key]] <- s
  }
  texts <- lapply(files, function(f) quiet(readLines(f, warn = FALSE), character()))
  # a key is a literal in the code, or a function's name (`pkg::fn`,
  # `fn(type = "monthly")`): the bare name is what the code mentions
  mentions <- function(key) {
    needle <- if (grepl("^[A-Za-z.][A-Za-z0-9._]*(::[A-Za-z.][A-Za-z0-9._]*)?(\\(.*\\))?$", key)) sub("\\(.*$", "", sub("^.*::", "", key)) else key
    out <- character()
    for (i in seq_along(files)) {
      ln <- grep(needle, texts[[i]], fixed = TRUE)
      if (length(ln)) out <- c(out, sprintf("%s:%d", relative_path(files[[i]], root), ln))
    }
    out
  }
  cached <- lapply(names(schemas), function(k) {
    e <- schemas[[k]]
    observed <- substr(as.character(e$observed_at %||% ""), 1L, 10L)
    age <- quiet(as.integer(Sys.Date() - as.Date(observed)))
    if (is.null(age)) age <- NA_integer_
    list(key = k, observed = observed, age = age, stale = !is.na(age) && age > ttl, sites = mentions(k), n = length(e$columns))
  })
  list(root = root, sources = unname(sources), cached = cached, n_files = length(files), ttl = ttl)
}

discover_format <- function(d) {
  n_sites <- sum(lengths(lapply(d$sources, `[[`, "sites")))
  out <- sprintf("r-colcheck discover: %d data source%s the checker cannot open, used in %d place%s across %d file%s; %d cached schema%s.",
                 length(d$sources), if (length(d$sources) == 1L) "" else "s", n_sites, if (n_sites == 1L) "" else "s",
                 d$n_files, if (d$n_files == 1L) "" else "s", length(d$cached), if (length(d$cached) == 1L) "" else "s")
  for (s in d$sources) {
    out <- c(out, sprintf("  \"%s\"   from %s   at %s", s$key, s$expr, paste(head(s$sites, 6L), collapse = ", ")),
             sprintf("    %s", s$command))
  }
  if (length(d$cached)) {
    out <- c(out, "Cached:")
    for (cc in d$cached) {
      out <- c(out, sprintf("  \"%s\"   %d columns, observed %s%s   %s", cc$key, cc$n, cc$observed,
                            if (!is.na(cc$age)) sprintf(" (%d days ago%s)", cc$age, if (cc$stale) ", stale" else "") else "",
                            if (length(cc$sites)) paste0("mentioned at ", paste(head(cc$sites, 4L), collapse = ", ")) else "mentioned in no file (r-colcheck forget drops it)"))
    }
    if (any(vapply(d$cached, `[[`, TRUE, "stale"))) out <- c(out, sprintf("Stale entries (older than %d days): r-colcheck refresh --stale", d$ttl))
  }
  if (length(d$sources)) out <- c(out, "Learn every source above in one run (each expression is evaluated once, with the shell's credentials; nothing here runs from a hook): r-colcheck learn --all   (--dry-run lists the commands)")
  out
}

# Findings for FILES, with a "stats" attribute: pipeline steps checked
# against known columns, steps skipped (columns unknowable), files read.
colcheck_files <- function(files, root = NULL) {
  all <- list()
  steps <- list()
  stats <- list(checked = 0L, partial = 0L, skipped = 0L, files = 0L, errors = 0L, unparsed = 0L)
  for (f in files) {
    if (!file.exists(f)) next
    res <- tryCatch(colcheck_file(f, root), error = function(err) list(list(
      kind = "error", file = f, line = 1L, name = NA_character_, source = NA_character_, columns = character(),
      message = sprintf("r-colcheck could not check this file at all (%s). This is a checker bug worth reporting.",
                        conditionMessage(err)))))
    st <- attr(res, "stats")
    all <- c(all, res)
    steps <- c(steps, attr(res, "steps"))
    stats$files <- stats$files + 1L
    stats$checked <- stats$checked + (st$checked %||% 0L)
    stats$partial <- stats$partial + (st$partial %||% 0L)
    stats$skipped <- stats$skipped + (st$skipped %||% 0L)
    stats$errors <- stats$errors + sum(vapply(res, function(x) identical(x$kind, "error"), TRUE))
    stats$unparsed <- stats$unparsed + (st$unparsed %||% 0L)
  }
  # The walker can visit a pipeline twice (assignment and its right-hand side).
  keys <- vapply(all, function(x) paste(x$file, x$line, x$name, x$source), "")
  structure(all[!duplicated(keys)], stats = stats, steps = steps)
}

# One line per step for `--steps`: file:line, the call, the columns after it.
steps_format <- function(steps, root = NULL) {
  vapply(steps, function(s) sprintf("%s:%s: %s => %s", relative_path(s$file, root), s$line, s$call,
    if (!is.null(s$fate)) sprintf("%s %s(): %s", if (s$fate == "written") "written by" else "returned by", s$fate_fn,
                                  paste(s$columns, collapse = ", "))
    else if (is.null(s$columns)) "(unknown)" else paste0(
      if (isTRUE(s$listed)) "each: " else "", if (isTRUE(s$open)) "at least " else "", paste(s$columns, collapse = ", "),
      if (length(s$groups)) sprintf(" [grouped by %s]", paste(s$groups, collapse = ", ")) else "")), "")
}

relative_path <- function(file, root) {
  shown <- normalizePath(file, mustWork = FALSE)
  base <- normalizePath(root %||% colcheck_root(file), mustWork = FALSE)
  if (startsWith(shown, paste0(base, "/"))) substring(shown, nchar(base) + 2L) else shown
}

colcheck_format <- function(findings, root = NULL) {
  vapply(findings, function(x) sprintf("%s:%s: %s", relative_path(x$file, root), x$line, x$message), "")
}
