# pattern 1: created in the callee, used by the caller
make_panel <- function(df) {
  df %>%
    dplyr::mutate(rolling_12m_value_add = cumsum(value))
}
# pattern 2: terminal sink
save_panel <- function(df) {
  result <- df %>% dplyr::mutate(trailing_value_add = value * 2)
  saveRDS(result, "output.rds")
}
# pattern 3: join payload
with_users <- function(df) {
  dplyr::left_join(df, load_users(), by = "id")
}
# pattern 4: whole-tibble verbs
as_lookup <- function(df) {
  df %>%
    dplyr::mutate(label = paste0("row", id)) %>%
    dplyr::select(id, label) %>%
    tibble::deframe()
}
wide <- function(df) {
  df %>%
    dplyr::mutate(yr = substr(date, 1, 4)) %>%
    tidyr::pivot_wider(names_from = yr, values_from = value)
}
