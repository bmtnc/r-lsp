# CLI entry for colcheck: r-colcheck [--json] [--root DIR] FILE.R ...
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
source(file.path(script_dir, "colcheck-lib.R"), local = TRUE)

args <- commandArgs(TRUE)
as_json <- "--json" %in% args
args <- setdiff(args, "--json")
root <- NULL
if (length(args) >= 2L && args[[1L]] == "--root") {
  root <- args[[2L]]
  args <- args[-(1:2)]
}
if (!length(args)) {
  cat("usage: r-colcheck [--json] [--root DIR] FILE.R ...\n",
      "Reports bare column names in tidyverse pipelines that cannot exist, checked against the\n",
      "columns of the data files the code reads (headers only). Silent when the data source\n",
      "cannot be resolved statically.\n", sep = "")
  quit(save = "no", status = 0)
}
findings <- colcheck_files(args, root)
if (as_json) {
  cat(jsonlite::toJSON(findings, auto_unbox = TRUE, pretty = TRUE), "\n")
} else {
  for (line in colcheck_format(findings, root)) cat(line, "\n", sep = "")
}
quit(save = "no", status = 0)
