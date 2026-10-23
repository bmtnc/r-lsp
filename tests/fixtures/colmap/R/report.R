assemble_report <- function(panel) {
  panel %>% dplyr::filter(!is.na(rolling_12m_value_add))
}
run_pipeline <- function() {
  df <- load_panel()
  panel <- make_panel(df)
  out <- assemble_report(panel)
  save_panel(df)
  joined <- with_users(df)
  lk <- as_lookup(df)
  w <- wide(df)
  list(out, joined, lk, w)
}
