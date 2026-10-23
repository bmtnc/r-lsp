g <- function(orders, users) {
  lookup <- users %>%
    dplyr::rename(user_name = name)
  orders %>%
    dplyr::left_join(lookup, by = "user_id") %>%
    dplyr::filter(!is.na(user_name))
}
