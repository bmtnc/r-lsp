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

# Off when R_LSP_AGENT_LINTERS says so (false, 0, no, off, in any case: the
# spellings R_LSP_HOOKS takes), or when the shell that started Claude Code
# had R_USER_CONFIG_DIR set to another directory: the wrapper then leaves it
# alone and lintr reads that directory's config, not the profile.
lint_profile_off <- function() {
  if (tolower(Sys.getenv("R_LSP_AGENT_LINTERS", "true")) %in% c("false", "0", "no", "off")) return(TRUE)
  cfg <- Sys.getenv("R_USER_CONFIG_DIR", "")
  plugin <- Sys.getenv("R_LSP_PLUGIN_ROOT", "")
  if (!nzchar(cfg)) return(FALSE)
  profile <- if (nzchar(plugin)) file.path(plugin, "lintr", "user-config") else file.path(dirname(script_dir), "lintr", "user-config")
  !identical(normalizePath(cfg, mustWork = FALSE), normalizePath(profile, mustWork = FALSE))
}

# The profile is written against lintr 3.2.0 (Linter(linter_level =)); an
# older lintr runs the same checks, registered without a level. Said once.
lintr_version_note <- function() {
  v <- tryCatch(utils::packageVersion("lintr"), error = function(e) NULL)
  if (is.null(v) || v >= "3.2.0") return(NULL)
  sprintf(paste("lintr %s is older than the 3.2.0 the diagnostics profile is written for; the same checks run.",
                "Upgrade when you can: Rscript --vanilla -e 'install.packages(\"lintr\")'"), v)
}
