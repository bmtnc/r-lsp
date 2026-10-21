#' Flag missing values
#' @param x numeric
#' @export
flag_missing <- function(x) {
  if (!is.na(x[1])) y <- sum(x) else y <- -sum(x, na.rm = TRUE)
  names(y) <- as.character(length(x))
  y
}
