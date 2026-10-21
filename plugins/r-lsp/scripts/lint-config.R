# Which lintr configuration wins for a file. lintr takes the first that
# exists: a project `.lintr.R` or `.lintr` (walking up from the file's
# directory, also under .github/linters/), then `~/.lintr.R` or `~/.lintr`,
# then the user config directory, which the wrapper points at the plugin's
# profile. It stops at the first match and merges nothing, so any project or
# home file switches the profile off entirely. This mirrors
# lintr:::find_config (lintr 3.1 to 3.3) so the server, the session-start
# context and the post-edit hook agree on which case they are in.
#
# Returns the winning project or home file, or NULL when the profile applies.
lint_config_file <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)
  dir <- if (dir.exists(path)) path else dirname(path)
  names <- c(".lintr.R", ".lintr")
  repeat {
    cands <- c(file.path(dir, names), file.path(dir, ".github", "linters", names))
    hit <- cands[file.exists(cands)]
    if (length(hit)) return(hit[[1L]])
    parent <- dirname(dir)
    if (identical(parent, dir)) break
    dir <- parent
  }
  home <- file.path(Sys.getenv("HOME", unset = "~"), names)
  hit <- home[file.exists(home)]
  if (length(hit)) hit[[1L]] else NULL
}

lint_profile_off <- function() identical(Sys.getenv("R_LSP_AGENT_LINTERS", "true"), "false")

# The profile is written against lintr 3.2.0 (Linter(linter_level =)); on an
# older lintr it runs in a compatibility mode. Said once so a missing
# diagnostic is not mistaken for a clean file.
lintr_version_note <- function() {
  v <- tryCatch(utils::packageVersion("lintr"), error = function(e) NULL)
  if (is.null(v) || v >= "3.2.0") return(NULL)
  sprintf(paste("lintr %s is older than 3.2.0: the diagnostics profile runs in a compatibility mode;",
                "upgrade with Rscript --vanilla -e 'install.packages(\"lintr\")'"), v)
}
