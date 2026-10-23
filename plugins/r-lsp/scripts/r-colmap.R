# CLI entry for colmap: r-colmap [--json] [--root DIR] [--file PATH ...] [COLUMN ...]
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
source(file.path(script_dir, "colcheck-lib.R"), local = TRUE)
source(file.path(script_dir, "colmap-lib.R"), local = TRUE)

args <- commandArgs(TRUE)
usage <- function() {
  cat("usage: r-colmap [--json] [--root DIR] [--file PATH ...] [COLUMN ...]\n",
      "Prints a map of the data: every data source the project's code reads (a file, a cached\n",
      "schema, a literal), its columns, and for each column where the code uses it (which\n",
      "verb, file:line), drops it, or creates it, plus the columns never used. With COLUMN,\n",
      "every site that touches that column in any source. --file limits the files read.\n",
      "Static: r-colcheck's tracking, so frames it cannot follow are not here. Always exits 0.\n", sep = "")
  quit(save = "no", status = 0)
}
as_json <- FALSE
root <- NULL
files <- character()
names <- character()
i <- 1L
while (i <= length(args)) {
  a <- args[[i]]
  if (a %in% c("-h", "--help")) usage()
  if (a == "--json") { as_json <- TRUE; i <- i + 1L; next }
  if (a == "--root" && i < length(args)) { root <- args[[i + 1L]]; i <- i + 2L; next }
  if (a == "--file" && i < length(args)) { files <- c(files, args[[i + 1L]]); i <- i + 2L; next }
  if (startsWith(a, "-")) { cat("r-colmap: unknown option ", a, "\n", sep = "", file = stderr()); quit(save = "no", status = 2) }
  if (nzchar(a)) names <- c(names, a)
  i <- i + 1L
}
root <- normalizePath(root %||% colcheck_root(file.path(getwd(), "x.R")), mustWork = FALSE)
missing <- files[!file.exists(files)]
if (length(missing)) { cat("r-colmap: no such file: ", paste(missing, collapse = ", "), "\n", sep = "", file = stderr()); quit(save = "no", status = 2) }
map <- colmap_build(root, if (length(files)) normalizePath(files) else NULL)
if (as_json) {
  message(paste(colmap_header(map), collapse = "\n"))
  cat(jsonlite::toJSON(Filter(Negate(is.null), colmap_json(map, names)), auto_unbox = TRUE, pretty = TRUE, null = "null"), "\n")
} else {
  cat(colmap_format(map, names), sep = "\n")
}
quit(save = "no", status = 0)
