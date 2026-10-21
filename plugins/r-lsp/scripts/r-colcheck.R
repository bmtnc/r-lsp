# CLI entry for colcheck: r-colcheck [--json] [--steps] [--root DIR] FILE.R ...
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
source(file.path(script_dir, "colcheck-lib.R"), local = TRUE)

args <- commandArgs(TRUE)
as_json <- "--json" %in% args
with_steps <- "--steps" %in% args
args <- setdiff(args, c("--json", "--steps"))
root <- NULL
if (length(args) >= 2L && args[[1L]] == "--root") {
  root <- args[[2L]]
  args <- args[-(1:2)]
}
if (!length(args)) {
  cat("usage: r-colcheck [--json] [--steps] [--root DIR] FILE.R ...\n",
      "       r-colcheck learn KEY 'EXPR' [--source FILE.R ...] [--load-all]\n",
      "       r-colcheck schemas | forget KEY\n",
      "Reports bare column names in tidyverse pipelines that cannot exist, checked against the\n",
      "columns of the data files the code reads (headers only) or, for data it cannot open\n",
      "(S3, databases, project loaders), against the schema cache .colcheck/schemas.json.\n",
      "Frames from sources it cannot resolve are noted when they are piped into verbs; `learn`\n",
      "evaluates the loader expression once, in the project, and caches the column names.\n",
      "`--steps` lists the column set after every pipeline step (with --json: {findings, steps}).\n", sep = "")
  quit(save = "no", status = 0)
}
findings <- colcheck_files(args, root)
st <- attr(findings, "stats")
kinds <- vapply(findings, function(x) x$kind %||% "column", "")
steps <- attr(findings, "steps")
if (as_json) {
  out <- if (with_steps) list(findings = unclass(findings), steps = lapply(steps, function(s) {
    for (k in c("columns", "groups")) if (!is.null(s[[k]])) s[[k]] <- I(s[[k]])
    s
  })) else unclass(findings)
  cat(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null"), "\n")
} else {
  for (line in colcheck_format(findings, root)) cat(line, "\n", sep = "")
  if (with_steps) for (line in steps_format(steps, root)) cat(line, "\n", sep = "")
}
# So that silence means "checked and clean", not "nothing was checkable".
message(sprintf("r-colcheck: %d pipeline step(s) checked against known columns in %d file(s); %d step(s) skipped (columns unknowable); %d finding(s), %d note(s)%s%s.",
                st$checked, st$files, st$skipped, sum(!(kinds %in% c("note", "error"))), sum(kinds == "note"),
                if (st$unparsed > 0L) sprintf("; %d file(s) did not parse", st$unparsed) else "",
                if (st$errors > 0L) sprintf("; %d statement(s) dropped by a checker error (listed above; R_LSP_COLCHECK_DEBUG=1 shows where)", st$errors) else ""))
quit(save = "no", status = if (any(!(kinds %in% c("note", "error")))) 1L else 0L)
