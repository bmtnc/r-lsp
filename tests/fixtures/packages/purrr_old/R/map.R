map <- function(.x, .f, ...) lapply(.x, .f, ...)
map_dfr <- function(.x, .f, ...) do.call(rbind, lapply(.x, .f, ...))
