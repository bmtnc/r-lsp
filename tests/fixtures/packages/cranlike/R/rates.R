# A public package whose function builds its result at run time: no cache
# key describes it, so the checker says nothing about it.
pull_rates <- function() stop("no network in a test package")
