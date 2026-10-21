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
      "       r-colcheck learn KEY 'EXPR' [--source FILE.R ...]\n",
      "       r-colcheck schemas | forget KEY\n",
      "Reports bare column names in tidyverse pipelines that cannot exist, checked against the\n",
      "columns of the data files the code reads (headers only) or, for data it cannot open\n",
      "(S3, databases, project loaders), against the schema cache .colcheck/schemas.json.\n",
      "Frames from sources it cannot resolve are noted when they are piped into verbs; `learn`\n",
      "evaluates the loader expression once, in the project, and caches the column names.\n", sep = "")
  quit(save = "no", status = 0)
}
findings <- colcheck_files(args, root)
st <- attr(findings, "stats")
kinds <- vapply(findings, function(x) x$kind %||% "column", "")
if (as_json) {
  cat(jsonlite::toJSON(unclass(findings), auto_unbox = TRUE, pretty = TRUE), "\n")
} else {
  for (line in colcheck_format(findings, root)) cat(line, "\n", sep = "")
}
# So that silence means "checked and clean", not "nothing was checkable".
message(sprintf("r-colcheck: %d pipeline step(s) checked against known columns in %d file(s); %d step(s) skipped (columns unknowable); %d finding(s), %d note(s).",
                st$checked, st$files, st$skipped, sum(kinds != "note"), sum(kinds == "note")))
quit(save = "no", status = if (any(kinds != "note")) 1L else 0L)
