run <- function() {
  lookup <- tibble::tribble(~id, ~nested, 1, 2) %>%
    dplyr::mutate(score = nested * 2)
  base <- tibble::tribble(~id, ~v, 1, 3)
  base %>%
    dplyr::left_join(y = lookup, by = "id") %>%
    dplyr::filter(score > 0)
}
