f <- function(orders, users) {
  lookup <- users %>%
    dplyr::select(user_id, status)
  orders %>%
    dplyr::left_join(lookup, by = "user_id")
}
