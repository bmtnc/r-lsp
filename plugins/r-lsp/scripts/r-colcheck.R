# CLI entry for colcheck: r-colcheck [--json] [--steps] [--root DIR] FILE.R ...
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
source(file.path(script_dir, "colcheck-lib.R"), local = TRUE)

usage <- function(to = stdout()) {
  cat("usage: r-colcheck [--json] [--steps] [--root DIR] FILE.R ...\n",
      "       r-colcheck learn KEY 'EXPR' [--source FILE.R ...] [--load-all] [--root DIR]\n",
      "       r-colcheck refresh [--stale] [KEY ...] [--root DIR]\n",
      "       r-colcheck discover | learn --all [--dry-run]   [--root DIR]\n",
      "       r-colcheck schemas | forget KEY | help           [--root DIR]\n",
      "Reports bare column names in tidyverse pipelines that cannot exist, checked against the\n",
      "columns of the data files the code reads (headers only) or, for data it cannot open\n",
      "(S3, databases, project loaders), against the schema cache .colcheck/schemas.json.\n",
      "Frames from sources it cannot resolve are noted when they are piped into verbs; `learn`\n",
      "evaluates the loader expression once, in the project, and caches the column names;\n",
      "`refresh` re-runs the cached expressions (all, the stale ones, or the keys given);\n",
      "`discover` lists every source the project uses that is not cached, with its learn command,\n",
      "and `learn --all` runs them (a loader registry in .colcheck.json gives each its cheap expression).\n",
      "`--steps` lists the column set after every pipeline step (with --json: {findings, steps}).\n",
      "The summary on stderr says how many steps were checked of all the steps seen.\n", sep = "", file = to)
}
args <- commandArgs(TRUE)
if (!length(args) || any(args %in% c("--help", "-h", "help"))) {
  usage()
  quit(save = "no", status = 0)
}
as_json <- "--json" %in% args
with_steps <- "--steps" %in% args
args <- setdiff(args, c("--json", "--steps"))
root <- NULL
# --root DIR anywhere among the arguments (the last one wins)
while (length(i <- which(args == "--root"))) {
  i <- i[[length(i)]]
  if (i == length(args)) { cat("r-colcheck: --root needs a directory\n", file = stderr()); usage(stderr()); quit(save = "no", status = 2) }
  root <- args[[i + 1L]]
  args <- args[-c(i, i + 1L)]
}
# an unknown flag, a subcommand that does not exist, or a file that is not
# there is an error, never a check of nothing that reads as clean
flags <- args[startsWith(args, "-")]
missing <- args[!startsWith(args, "-") & !file.exists(args)]
if (length(flags) || length(missing) || !length(args)) {
  for (f in flags) cat("r-colcheck: unknown option ", f, "\n", sep = "", file = stderr())
  for (m in missing) cat("r-colcheck: no such file: ", m,
                         if (!grepl("[.][A-Za-z]+$", m)) " (not a subcommand either; see below)" else "", "\n", sep = "", file = stderr())
  usage(stderr())
  quit(save = "no", status = 2)
}
# the files named are checked afresh; the writers, helpers and targets their check reaches come from the cache
findings <- colcheck_files(args, root, fresh = TRUE)
st <- attr(findings, "stats")
kinds <- vapply(findings, function(x) x$kind %||% "column", "")
steps <- attr(findings, "steps")
if (as_json) {
  # paths relative to the root, as the text output and r-colmap --json print them
  rel <- function(x) { x$file <- relative_path(x$file, root %||% colcheck_root(x$file)); x }
  out <- if (with_steps) list(findings = lapply(unclass(findings), rel), steps = lapply(steps, function(s) {
    for (k in c("columns", "groups", "before", "used", "overwritten")) if (!is.null(s[[k]])) s[[k]] <- I(s[[k]])
    if (!is.null(s$derived)) s$derived <- lapply(s$derived, I)
    rel(s)
  })) else lapply(unclass(findings), rel)
  cat(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null"), "\n")
} else {
  for (line in colcheck_format(findings, root)) cat(line, "\n", sep = "")
  if (with_steps) for (line in steps_format(steps, root)) cat(line, "\n", sep = "")
}
# So that silence means "checked and clean", not "nothing was checkable":
# coverage first, with the steps on partly known frames (an open set, "at
# least these", nothing reported against it) counted apart from the unknown.
total <- st$checked + st$partial + st$skipped
message(sprintf("r-colcheck: %d of %d pipeline step(s) checked against known columns; %d not checked (%d on partly known columns, %d on unknown); %d finding(s), %d note(s) in %d file(s)%s%s.",
                st$checked, total, st$partial + st$skipped, st$partial, st$skipped,
                sum(!(kinds %in% c("note", "error", "grouped"))), sum(kinds %in% c("note", "grouped")), st$files,
                if (st$unparsed > 0L) sprintf("; %d file(s) did not parse", st$unparsed) else "",
                if (st$errors > 0L) sprintf("; %d statement(s) dropped by a checker error (listed above; R_LSP_COLCHECK_DEBUG=1 shows where)", st$errors) else ""))
quit(save = "no", status = if (any(!(kinds %in% c("note", "error", "grouped")))) 1L else 0L)
