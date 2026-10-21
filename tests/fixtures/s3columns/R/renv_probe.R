# Loader whose column name records whether renv was active when it ran:
# `r-colcheck learn` must run inside the project's .Rprofile, not --vanilla.
probe <- function() {
  if (nzchar(Sys.getenv("RENV_PROJECT"))) data.frame(renv_on = 1) else data.frame(renv_off = 1)
}
