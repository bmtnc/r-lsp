# Package functions from another file in R/ are known without source().
run <- function() {
  sales <- load_sales()
  ok <- sales |> north_only() |> dplyr::mutate(half = amount / 2)
  bad <- sales |> north_only() |> dplyr::filter(amout > 1)                            # bad
  ok
}
