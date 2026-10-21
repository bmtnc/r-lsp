load_sales <- function() readr::read_csv("data/sales.csv")

north_only <- function(df) df |> dplyr::filter(region == "North")
