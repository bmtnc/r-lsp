write_totals <- function() {
  out <- tibble::tribble(
    ~a, ~b,
    1,  2
  ) %>%
    dplyr::mutate(total = a + b)
  saveRDS(out, "out.rds")
}

return_totals <- function() {
  tibble::tribble(
    ~a, ~b,
    1,  2
  ) %>%
    dplyr::mutate(total = a + b)
}
