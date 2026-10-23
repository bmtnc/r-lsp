# CLI entry for the plugin's bug linters: r-lint [--json] [--root DIR] FILE.R ...
# The same profile and the same code path as the hooks' note (lint_file in
# hook-check-lib.R), so the two cannot drift: undefined functions, undefined
# variables inside functions, non-exported pkg::fun, missing packages,
# deprecated, superseded, defunct or experimental calls judged against the
# project's library. Exits 1 with findings, 0 clean, 2 on usage.
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
args <- commandArgs(TRUE)
usage <- function(to = stdout()) {
  cat("usage: r-lint [--json] [--root DIR] FILE.R ...\n",
      "Runs the plugin's bug linters (the profile the language server uses when no .lintr\n",
      "wins) on the files given, judged against the project's renv library when there is one.\n",
      "Prints file:line:col: message [linter]; exits 1 with findings, 0 when clean, 2 on usage.\n",
      "Parse errors are reported too. Style rules are not part of the profile.\n", sep = "", file = to)
}
if (!length(args) || any(args %in% c("-h", "--help", "help"))) { usage(); quit(save = "no", status = 0) }
as_json <- "--json" %in% args
args <- setdiff(args, "--json")
root <- NULL
if (length(args) >= 2L && args[[1L]] == "--root") { root <- args[[2L]]; args <- args[-(1:2)] }
flags <- args[startsWith(args, "-")]
missing <- args[!startsWith(args, "-") & !file.exists(args)]
if (length(flags) || length(missing) || !length(args)) {
  for (f in flags) cat("r-lint: unknown option ", f, "\n", sep = "", file = stderr())
  for (m in missing) cat("r-lint: no such file: ", m, "\n", sep = "", file = stderr())
  usage(stderr())
  quit(save = "no", status = 2)
}
source(file.path(script_dir, "hook-check-lib.R"), local = TRUE)
if (lint_profile_off()) {
  cat("r-lint: the profile is off (R_LSP_AGENT_LINTERS=false); nothing to run\n", file = stderr())
  quit(save = "no", status = 0)
}
root <- normalizePath(root %||% colcheck_root(args[[1L]]), mustWork = FALSE)
lines <- character()
failures <- character()
parse_errors <- 0L
for (f in args) {
  f <- normalizePath(f, mustWork = FALSE)
  # a file that does not parse: the parser's message, as the hooks give it
  perr <- tryCatch({ parse(f, keep.source = TRUE); NULL }, error = function(e) conditionMessage(e))
  if (!is.null(perr)) {
    parse_errors <- parse_errors + 1L
    lines <- c(lines, sprintf("%s: does not parse: %s [parse]", relative_path(f, root), strsplit(perr, "\n")[[1L]][[1L]]))
    next
  }
  r <- lint_file(f, root)
  if (!is.null(r$failure)) failures <- c(failures, sprintf("%s: the linters could not run: %s", relative_path(f, root), r$failure))
  lines <- c(lines, r$lines)
}
if (as_json) {
  cat(jsonlite::toJSON(list(lints = I(lines), failures = I(failures)), auto_unbox = TRUE, pretty = TRUE), "\n")
} else {
  for (l in c(lines, failures)) cat(l, "\n", sep = "")
}
message(sprintf("r-lint: %d finding(s) in %d file(s)%s%s", length(lines), length(args),
                if (parse_errors) sprintf(", %d did not parse", parse_errors) else "",
                if (length(failures)) sprintf("; the linters failed on %d file(s) (listed above)", length(failures)) else ""))
quit(save = "no", status = if (length(lines) || length(failures)) 1L else 0L)
