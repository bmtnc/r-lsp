# CLI entry for colmap: r-colmap [--json] [--root DIR] [--file PATH ...] [--source NAME ...] [COLUMN ...]
#                       r-colmap [--json] [--root DIR] --diff [REF]
#                       r-colmap [--root DIR] --at FILE:LINE
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
source(file.path(script_dir, "colcheck-lib.R"), local = TRUE)
source(file.path(script_dir, "callmap-lib.R"), local = TRUE)   # git_changes(), in_ranges() for --diff
source(file.path(script_dir, "colmap-lib.R"), local = TRUE)

args <- commandArgs(TRUE)
usage <- function(status = 0) {
  cat("usage: r-colmap [--json] [--root DIR] [--file PATH ...] [--source NAME ...] [COLUMN ...]\n",
      "       r-colmap [--json] [--root DIR] --diff [REF]      what the working tree's diff against REF (default HEAD) touches\n",
      "       r-colmap [--root DIR] --at FILE:LINE               the column set at a line: before and after each step there\n",
      "Prints a map of the data: every data source the project's code reads (a file, a cached\n",
      "schema, a literal), its columns, and for each column where the code uses it (which\n",
      "verb, file:line), drops it, or creates it and from which columns, plus the columns never\n",
      "used. With COLUMN, every site that touches that column in any source, what it derives\n",
      "from and what is computed from it. --source NAME shows the sources whose name contains\n",
      "NAME (a file, a cached schema key, a target). --diff lists the columns the changed lines use, create\n",
      "or drop, where else each is used, the columns the changed files no longer create, and the\n",
      "checker's findings on the changed lines. --file limits the files read.\n",
      "Static: r-colcheck's tracking, so frames it cannot follow are not here. Exits 0; 2 on a bad argument.\n",
      sep = "", file = if (status) stderr() else stdout())
  quit(save = "no", status = status)
}
as_json <- FALSE
root <- NULL
files <- character()
names <- character()
srcs <- character()
mode <- "map"
ref <- NULL
at <- NULL
i <- 1L
while (i <= length(args)) {
  a <- args[[i]]
  if (a %in% c("-h", "--help") || (a == "help" && !length(names))) usage()   # `help` as the first name, as r-colcheck takes it
  if (a == "--json") { as_json <- TRUE; i <- i + 1L; next }
  if (a %in% c("--root", "--file", "--source") && i == length(args)) { cat("r-colmap: ", a, " needs a value\n", sep = "", file = stderr()); usage(2) }
  if (a == "--root" && i < length(args)) { root <- args[[i + 1L]]; i <- i + 2L; next }
  if (a == "--file" && i < length(args)) { files <- c(files, args[[i + 1L]]); i <- i + 2L; next }
  if (a == "--source" && i < length(args)) { srcs <- c(srcs, args[[i + 1L]]); i <- i + 2L; next }
  if (a == "--diff") {
    mode <- "diff"
    if (i < length(args) && !startsWith(args[[i + 1L]], "-")) { ref <- args[[i + 1L]]; i <- i + 2L } else i <- i + 1L
    next
  }
  if (a == "--at") {
    if (i == length(args) || !grepl(":[0-9]+$", args[[i + 1L]])) { cat("r-colmap: --at needs FILE:LINE\n", file = stderr()); usage(2) }
    mode <- "at"
    at <- args[[i + 1L]]
    i <- i + 2L
    next
  }
  if (startsWith(a, "-")) { cat("r-colmap: unknown option ", a, "\n", sep = "", file = stderr()); usage(2) }
  if (nzchar(a)) names <- c(names, a)
  i <- i + 1L
}
root <- normalizePath(root %||% colcheck_root(file.path(getwd(), "x.R")), mustWork = FALSE)
if (mode == "at") {
  at_file <- sub(":[0-9]+$", "", at)
  at_line <- as.integer(sub("^.*:", "", at))
  if (!file.exists(at_file) && file.exists(file.path(root, at_file))) at_file <- file.path(root, at_file)
  if (!file.exists(at_file)) { cat("r-colmap: no such file: ", at_file, "\n", sep = "", file = stderr()); quit(save = "no", status = 2) }
  # the whole project is read, so the sites that read the step's columns are known
}
missing <- files[!file.exists(files)]
if (length(missing)) { cat("r-colmap: no such file: ", paste(missing, collapse = ", "), "\n", sep = "", file = stderr()); quit(save = "no", status = 2) }
map <- colmap_build(root, if (length(files)) normalizePath(files) else NULL)
if (mode == "at") {
  cat(colmap_at(map, relative_path(at_file, root), at_line), sep = "\n")
  quit(save = "no", status = 0)
}
if (mode == "diff") {
  d <- colmap_diff(map, ref)
  if (as_json) {
    message(paste(colmap_header(map, sprintf("changes in the working tree against %s", d$base)), collapse = "\n"))
    cat(jsonlite::toJSON(colmap_diff_json(map, d), auto_unbox = TRUE, pretty = TRUE, null = "null"), "\n")
  } else cat(colmap_diff_format(map, d), sep = "\n")
  quit(save = "no", status = 0)
}
if (as_json) {
  message(paste(colmap_header(map), collapse = "\n"))
  # the sources as before; the files the code writes under a "written" key when nothing is asked by name
  out <- Filter(Negate(is.null), colmap_json(map, names))
  if (!length(names) && length(map$sinks)) out <- list(sources = out, written = colmap_sinks_json(map))
  cat(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null"), "\n")
} else {
  cat(colmap_format(map, names, srcs), sep = "\n")
}
quit(save = "no", status = 0)
