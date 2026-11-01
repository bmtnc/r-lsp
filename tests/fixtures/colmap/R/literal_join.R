join_users <- function() {
  orders <- tibble::tribble(
    ~user_id, ~amount,
    1,        10
  )
  users <- tibble::tribble(
    ~user_id, ~user_name,
    1,        "a"
  )
  orders %>%
    dplyr::left_join(users, by = "user_id") %>%
    dplyr::filter(!is.na(user_name))
}
