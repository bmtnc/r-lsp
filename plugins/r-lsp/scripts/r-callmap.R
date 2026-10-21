# CLI entry for callmap: r-callmap [--json] [--root DIR] [--depth N] [NAME ...]
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
source(file.path(script_dir, "colcheck-lib.R"), local = TRUE)
source(file.path(script_dir, "callmap-lib.R"), local = TRUE)

args <- commandArgs(TRUE)
usage <- function() {
  cat("usage: r-callmap [--json] [--root DIR] [--depth N] [NAME ...]\n",
      "Prints a static call map of the project: every function definition with its file:line,\n",
      "arguments, the project functions it calls and the functions and files that call it,\n",
      "then the entry points, unreferenced functions and cycles. With NAME, only the callers\n",
      "and callees of NAME, expanded to --depth levels (default 2), with each call site.\n",
      "Only functions defined in the project are listed. Always exits 0.\n", sep = "")
  quit(save = "no", status = 0)
}
as_json <- FALSE
root <- NULL
depth <- 2L
names <- character()
i <- 1L
while (i <= length(args)) {
  a <- args[[i]]
  if (a %in% c("-h", "--help")) usage()
  if (a == "--json") { as_json <- TRUE; i <- i + 1L; next }
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

map <- callmap_build(root)
if (length(names)) {
  lines <- callmap_neighbourhood(map, names, depth)
  ids <- attr(lines, "ids")
} else {
  lines <- callmap_format(map)
  ids <- seq_along(map$defs$label)
}
if (as_json) {
  # stdout stays pure JSON; the header goes to stderr.
  message(paste(c(callmap_header(map), grep("not defined in the project", lines, value = TRUE)), collapse = "\n"))
  cat(jsonlite::toJSON(callmap_json(map, ids), auto_unbox = TRUE, pretty = TRUE), "\n")
} else {
  cat(lines, sep = "\n")
}
quit(save = "no", status = 0)
