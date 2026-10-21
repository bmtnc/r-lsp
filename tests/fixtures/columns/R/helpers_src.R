# source()d by R/funcs.R: a helper defined in another file.
add_share <- function(df) {
  df %>%
    mutate(share = amount / sum(amount))
}
widen_src <- function(df) df %>% pivot_wider(names_from = year, values_from = amount)
