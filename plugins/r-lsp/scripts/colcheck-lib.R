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
# downstream is checked. Nothing is reported when the source cannot be
# resolved. Rmd/qmd files are skipped.
#
# Entry points: colcheck_files(files, root) -> list of findings;
#               colcheck_format(findings, root) -> character lines.

`%||%` <- function(a, b) if (is.null(a)) b else a
quiet <- function(expr, default = NULL) {
  tryCatch(suppressWarnings(suppressMessages(expr)), error = function(e) default)
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

call_name <- function(e) {
  if (!is.call(e)) return(NA_character_)
  h <- e[[1L]]
  if (is.symbol(h)) return(as.character(h))
  if (is.call(h) && as.character(h[[1L]]) %in% c("::", ":::")) return(as.character(h[[3L]]))
  NA_character_
}

string_arg <- function(e) if (is.character(e) && length(e) == 1L) e else NULL

resolve_path <- function(e, ctx) {
  if (is.character(e) && length(e) == 1L) {
    cands <- c(e, file.path(ctx$root, e), file.path(ctx$filedir, e))
    hit <- cands[file.exists(cands)]
    return(if (length(hit)) normalizePath(hit[[1L]]) else NULL)
  }
  if (is.call(e) && call_name(e) %in% c("here", "file.path")) {
    parts <- lapply(as.list(e)[-1L], string_arg)
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
frame_columns <- function(e, ctx) {
  if (is.symbol(e)) {
    nm <- as.character(e)
    if (!is.null(ctx$frames[[nm]])) return(ctx$frames[[nm]])
    if (nm %in% ctx$unknown_frames || nm %in% ctx$variables) return(NULL)
    return(builtin_dataset(nm, ctx))
  }
  if (!is.call(e)) return(NULL)
  fn <- call_name(e)
  if (is.na(fn)) return(NULL)
  if (fn %in% c("%>%", "%<>%", "%T>%")) {
    lhs <- frame_columns(e[[2L]], ctx)
    if (is.null(lhs)) return(NULL)
    rhs <- e[[3L]]
    if (is.symbol(rhs)) rhs <- as.call(list(rhs))
    if (!is.call(rhs)) return(NULL)
    return(apply_verb(rhs, lhs, ctx, piped = TRUE))
  }
  if (fn %in% readers) {
    path_arg <- if (length(e) >= 2L) e[[2L]] else NULL
    nms <- names(e)
    if (!is.null(nms)) {
      for (k in c("file", "path", "input", "sources")) if (k %in% nms) path_arg <- e[[k]]
    }
    path <- resolve_path(path_arg, ctx)
    if (is.null(path)) return(NULL)
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
    hdr <- Filter(function(a) is.call(a) && identical(a[[1L]], as.name("~")), as.list(e)[-1L])
    cols <- vapply(hdr, function(a) as.character(a[[2L]]), "")
    if (!length(cols)) return(NULL)
    attr(cols, "source") <- "tribble() literal"
    return(cols)
  }
  # native pipe is parsed into nested calls: verb(frame_expr, ...)
  if (length(e) >= 2L) {
    inner <- frame_columns(e[[2L]], ctx)
    if (!is.null(inner)) return(apply_verb(e, inner, ctx, piped = FALSE))
  }
  NULL
}

# Calls whose arguments are selections or quoted code, never bare columns to check.
quoting_calls <- c("across", "if_any", "if_all", "pick", "c_across", "where", "all_of", "any_of",
                   "starts_with", "ends_with", "contains", "matches", "num_range", "everything",
                   "last_col", "vars", "quote", "bquote", "expr", "sym", "syms", "!!", "!!!",
                   "glue", "str_glue")

# Names used as bare symbols inside an expression evaluated with data masking.
masked_symbols <- function(e, acc = character()) {
  if (is.symbol(e)) return(c(acc, as.character(e)))
  if (!is.call(e)) return(acc)
  fn <- call_name(e)
  if (!is.na(fn) && fn %in% c("$", "@", "::", ":::")) return(acc)
  if (!is.na(fn) && fn %in% quoting_calls) return(acc)
  if (is.call(e[[1L]]) && call_name(e[[1L]]) %in% c("::", ":::")) {
    # pkg::fun(args): walk the args only
  }
  for (p in as.list(e)[-1L]) acc <- masked_symbols(p, acc)
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
    } else return(NULL)
  }
  out
}

report <- function(ctx, name, cols) {
  src <- attr(cols, "source") %||% "the data"
  ctx$findings[[length(ctx$findings) + 1L]] <- list(
    file = ctx$file, line = ctx$line, name = name, source = src,
    columns = as.character(cols),
    message = sprintf("'%s' is not a column at this point (columns: %s; data from %s)", name,
                      paste(head(cols, 12), collapse = ", "), src))
}

check_symbols <- function(syms, cols, ctx) {
  syms <- unique(syms)
  bad <- syms[!(syms %in% cols) & !(syms %in% ctx$variables) & !(syms %in% ctx$known_names) &
              !vapply(syms, function(s) exists(s, envir = baseenv()), TRUE)]
  for (b in bad) report(ctx, b, cols)
}

# Apply one verb call to a known column set; returns the new set or NULL (unknown).
apply_verb <- function(call, cols, ctx, piped) {
  fn <- call_name(call)
  if (is.na(fn)) return(NULL)
  args <- as.list(call)[-1L]
  if (!piped && length(args)) args <- args[-1L]
  nms <- names(args) %||% rep("", length(args))
  dot_args <- startsWith(nms, ".")
  pos <- args[!nzchar(nms)]
  named <- args[nzchar(nms) & !dot_args]
  group_cols <- attr(cols, "groups") %||% character()
  with_groups <- function(x, g = group_cols) { attr(x, "groups") <- g; keep_source(x, cols) }
  check_all <- function() check_symbols(unlist(lapply(c(pos, named), masked_symbols)), cols, ctx)
  has_nonsymbol_pos <- function() any(vapply(pos, function(p) !is.symbol(p), TRUE))

  if (fn %in% c("filter", "arrange", "slice", "slice_head", "slice_tail", "slice_min", "slice_max",
                "slice_sample", "drop_na", "fill", "relocate", "ungroup", "rowwise", "glimpse", "print",
                "head", "tail", "collect", "compute", "as_tibble", "as.data.frame")) {
    check_all()
    return(with_groups(cols, if (fn == "ungroup") character() else group_cols))
  }
  if (fn == "group_by") {
    check_all()
    g <- selection_names(pos)
    add <- isTRUE(args[[".add"]] %||% FALSE)
    return(with_groups(cols, if (is.null(g)) group_cols else unique(c(if (add) group_cols, g))))
  }
  if (fn %in% c("mutate", "transmute")) {
    check_all()
    if (has_nonsymbol_pos()) return(NULL)
    new <- names(named)
    if (fn == "mutate") return(with_groups(unique(c(cols, new))))
    return(with_groups(unique(c(group_cols, new))))
  }
  if (fn %in% c("summarise", "summarize", "reframe")) {
    check_all()
    if (has_nonsymbol_pos()) return(NULL)
    by <- if (".by" %in% names(args)) selection_names(list(args[[".by"]])) else group_cols
    if (is.null(by)) return(NULL)
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
    return(with_groups(if (fn == "add_count") unique(c(cols, nm)) else unique(c(group_cols, g, nm)), character()))
  }
  if (fn == "distinct") {
    check_all()
    g <- selection_names(pos)
    if (is.null(g)) return(NULL)
    if (!length(g) || isTRUE(args[[".keep_all"]] %||% FALSE)) return(with_groups(cols))
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
    other <- frame_columns(if (length(pos)) pos[[1L]] else args[["y"]], ctx)
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
  cols <- frame_columns(base[[2L]], ctx)
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

verbs <- c("mutate", "filter", "summarise", "summarize", "select", "arrange", "group_by", "rename",
           "transmute", "distinct", "count", "pivot_longer", "pivot_wider", "ggplot", "slice", "reframe",
           "left_join", "inner_join", "right_join", "full_join", "semi_join", "anti_join")

walk_expr <- function(e, ctx) {
  if (!is.call(e)) return(invisible())
  fn <- call_name(e)
  if (!is.na(fn) && fn %in% c("<-", "=", "<<-") && length(e) == 3L && is.symbol(e[[2L]])) {
    target <- as.character(e[[2L]])
    cols <- frame_columns(e[[3L]], ctx)
    if (!is.null(cols)) {
      ctx$frames[[target]] <- cols
    } else {
      ctx$frames[[target]] <- NULL
      ctx$unknown_frames <- union(ctx$unknown_frames, target)
    }
    ctx$variables <- union(ctx$variables, target)
    check_ggplot(e[[3L]], ctx)
    if (is.call(e[[3L]]) && !identical(call_name(e[[3L]]), "function")) walk_expr(e[[3L]], ctx)
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
    walk_expr(e[[3L]], ctx)
    ctx$variables <- saved_vars
    ctx$unknown_frames <- saved_unknown
    return(invisible())
  }
  if (!is.na(fn) && fn %in% c("%>%", "%<>%", "%T>%")) {
    frame_columns(e, ctx)
    return(invisible())
  }
  if (!is.na(fn) && fn == "+") {
    check_ggplot(e, ctx)
    return(invisible())
  }
  if (!is.na(fn) && fn %in% verbs) frame_columns(e, ctx)
  for (p in as.list(e)[-1L]) if (is.call(p)) walk_expr(p, ctx)
  invisible()
}

colcheck_file <- function(file, root = NULL) {
  ctx <- new.env()
  ctx$file <- file
  ctx$root <- root %||% colcheck_root(file)
  ctx$filedir <- dirname(normalizePath(file, mustWork = FALSE))
  ctx$frames <- list()
  ctx$unknown_frames <- character()
  ctx$variables <- character()
  ctx$packages <- character()
  ctx$findings <- list()
  ctx$known_names <- c("TRUE", "FALSE", "NA", "NULL", "T", "F", "Inf", "NaN", ".", ".x", ".y", ".data", ".env",
                       "desc", "n", "everything", "where", "across", "pi", "letters", "LETTERS",
                       "month.name", "month.abb", "self", "private")
  exprs <- quiet(parse(file, keep.source = TRUE))
  if (is.null(exprs)) return(list())
  src <- attr(exprs, "srcref")
  # Names assigned anywhere in the file count as variables: data masking falls back to the environment.
  for (e in exprs) {
    if (is.call(e) && call_name(e) %in% c("<-", "=", "<<-") && is.symbol(e[[2L]])) {
      ctx$variables <- union(ctx$variables, as.character(e[[2L]]))
    }
  }
  for (i in seq_along(exprs)) {
    ctx$line <- if (!is.null(src)) as.integer(src[[i]][[1L]]) else NA_integer_
    quiet(walk_expr(exprs[[i]], ctx))
  }
  ctx$findings
}

colcheck_files <- function(files, root = NULL) {
  all <- list()
  for (f in files) {
    if (!file.exists(f) || grepl("[.](Rmd|rmd|qmd)$", f)) next
    all <- c(all, colcheck_file(f, root))
  }
  # The walker can visit a pipeline twice (assignment and its right-hand side).
  keys <- vapply(all, function(x) paste(x$file, x$line, x$name, x$source), "")
  all[!duplicated(keys)]
}

colcheck_format <- function(findings, root = NULL) {
  vapply(findings, function(x) {
    shown <- normalizePath(x$file, mustWork = FALSE)
    base <- normalizePath(root %||% colcheck_root(x$file), mustWork = FALSE)
    if (startsWith(shown, paste0(base, "/"))) shown <- substring(shown, nchar(base) + 2L)
    sprintf("%s:%s: %s", shown, x$line, x$message)
  }, "")
}
