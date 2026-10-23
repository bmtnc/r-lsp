# CLI entry for callmap: r-callmap [--json] [--root DIR] [--depth N] [NAME ...]
#                        r-callmap --diff [REF] | --tests NAME ... | --file PATH
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
source(file.path(script_dir, "colcheck-lib.R"), local = TRUE)
source(file.path(script_dir, "callmap-lib.R"), local = TRUE)

args <- commandArgs(TRUE)
# status 2 (a bad root, an unknown flag) prints the usage to stderr.
usage <- function(status = 0) {
  cat("usage: r-callmap [--json] [--root DIR] [--depth N] [NAME ...]\n",
      "       r-callmap [--json] [--depth N] --diff [REF]     what the working tree's diff against REF (default HEAD) touches\n",
      "       r-callmap [--json] --tests NAME ...             the test files that reach NAME through callers, with the commands\n",
      "       r-callmap [--json] [--depth N] --file PATH      the callers and callees of every function defined in PATH\n",
      "Prints a static call map of the project: every function definition with its file:line,\n",
      "arguments, the project functions it calls and the functions and files that call it,\n",
      "then the entry points, unreferenced functions and cycles. With NAME, only the callers\n",
      "and callees of NAME, expanded to --depth levels (default 2), with each call site.\n",
      "--diff lists the definitions the diff changed (new, arguments changed, body changed),\n",
      "the definitions it removed with where they are still mentioned, the top-level code it\n",
      "changed, then the callers and callees of the changed definitions and the tests that\n",
      "reach them. Only functions defined in the project are listed. Exits 0; 2 when the root is\n",
      "not a directory or has no R, Rmd or qmd file under it.\n", sep = "", file = if (status) stderr() else stdout())
  quit(save = "no", status = status)
}
as_json <- FALSE
root <- NULL
depth <- 2L
names <- character()
mode <- "map"
ref <- NULL
file <- NULL
i <- 1L
while (i <= length(args)) {
  a <- args[[i]]
  if (a %in% c("-h", "--help")) usage()
  if (a == "--json") { as_json <- TRUE; i <- i + 1L; next }
  if (a == "--diff") {
    mode <- "diff"
    if (i < length(args) && !startsWith(args[[i + 1L]], "-")) { ref <- args[[i + 1L]]; i <- i + 2L } else i <- i + 1L
    next
  }
  if (a == "--tests") { mode <- "tests"; i <- i + 1L; next }
  if (a == "--file") {
    if (i == length(args)) { cat("r-callmap: --file needs a path\n", file = stderr()); quit(save = "no", status = 2) }
    mode <- "file"; file <- args[[i + 1L]]; i <- i + 2L; next
  }
  # The last --root wins, so the wrapper's default can be overridden.
  if (a == "--root" && i < length(args)) { root <- args[[i + 1L]]; i <- i + 2L; next }
  if (a == "--depth" && i < length(args)) {
    depth <- suppressWarnings(as.integer(args[[i + 1L]]))
    if (is.na(depth) || depth < 1L) depth <- 1L
    i <- i + 2L
    next
  }
  names <- c(names, a)
  i <- i + 1L
}
root <- normalizePath(root %||% colcheck_root(file.path(getwd(), "x.R")), mustWork = FALSE)
# A wrong root must not come back as an empty map that reads as "no functions".
if (!dir.exists(root)) {
  cat(sprintf("r-callmap: no such directory: %s\n", root), file = stderr())
  usage(2)
}

map <- callmap_build(root)
if (map$n_files == 0L) {
  cat(sprintf("r-callmap: no R, Rmd or qmd file under %s (is this the project root? pass --root DIR)\n", root), file = stderr())
  quit(save = "no", status = 2)
}
if (mode == "diff") {
  lines <- callmap_diff(map, ref, depth)
  ids <- attr(lines, "ids")
} else if (mode == "tests") {
  if (!length(names)) { cat("r-callmap --tests needs at least one NAME\n", file = stderr()); quit(save = "no", status = 2) }
  lines <- callmap_tests_view(map, names)
  ids <- attr(lines, "ids")
} else if (mode == "file") {
  ids <- callmap_file_ids(map, file)
  lines <- if (length(ids)) callmap_neighbourhood(map, unique(map$defs$label[ids]), depth)
    else c(callmap_header(map), "", sprintf("%s: no function is defined in that file (or it is not in the map).", file))
} else if (length(names)) {
  lines <- callmap_neighbourhood(map, names, depth)
  ids <- attr(lines, "ids")
} else {
  lines <- callmap_format(map)
  ids <- seq_along(map$defs$label)
}
if (as_json) {
  # stdout stays pure JSON; the header goes to stderr.
  message(paste(c(callmap_header(map), grep("not defined in the project|not inside a git repository|git diff .* failed", lines, value = TRUE)), collapse = "\n"))
  json <- if (mode == "diff" && is.null(attr(lines, "changed"))) list(changed = list(), deleted = list(), tests = list(), untested = I(character()))
    else if (mode == "diff") callmap_diff_json(map, lines)
    else if (mode == "tests") {
      reach <- callmap_tests(map, ids)
      list(tests = lapply(names(reach), function(f) list(file = f, reaches = I(unique(vapply(reach[[f]], function(x) map$defs$label[[x$id]], ""))),
                                                         command = test_command(map, f))),
           untested = I(map$defs$label[setdiff(ids, unique(unlist(lapply(reach, function(l) vapply(l, function(x) x$id, 1L)))))]))
    } else callmap_json(map, ids)
  cat(jsonlite::toJSON(json, auto_unbox = TRUE, pretty = TRUE), "\n")
} else {
  cat(lines, sep = "\n")
}
quit(save = "no", status = 0)
