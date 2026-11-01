# p1a: the callee hits a step the checker cannot follow before returning
make_panel2 <- function(df) {
  df %>%
    dplyr::mutate(created_before_stop = value + 1) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round))
}
report2 <- function(panel) {
  panel %>% dplyr::filter(created_before_stop > 0)
}
# p1b: exported entry point; the caller is outside the package
#' @export
make_panel3 <- function(df) {
  df %>% dplyr::mutate(created_for_export = value * 3)
}
# p1c: explicit return()
make_panel4 <- function(df) {
  out <- df %>% dplyr::mutate(created_then_returned = value * 4)
  return(out)
}
report4 <- function(panel) panel %>% dplyr::filter(created_then_returned > 0)
# p4: whole-tibble verbs
verbs <- function(df) {
  a <- df %>% dplyr::mutate(neg = -value) %>% dplyr::select(-neg)
  b <- df %>% dplyr::mutate(ev = value) %>% dplyr::select(dplyr::everything())
  c <- df %>% dplyr::mutate(nst = value) %>% tidyr::nest(data = -id)
  d <- df %>% dplyr::mutate(acr = value) %>% dplyr::summarise(dplyr::across(c(value, acr), mean))
  list(a, b, c, d)
}
run_more <- function() {
  df <- load_panel()
  r2 <- report2(make_panel2(df))
  r4 <- report4(make_panel4(df))
  p3 <- make_panel3(df)   # a local, non-exported-style call too
  v <- verbs(df)
  m <- purrr::map(list(df, df), make_panel4)
  list(r2, r4, p3, v, m)
}
