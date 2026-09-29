# Fixture for tests/smoke_test.py. Sections are found by their marker
# comments, so lines can be added without breaking the test.

# == correct: no diagnostics expected in this section ==
library(tidyverse)

sales <- read_csv("data/sales.csv")

by_region <- sales %>%
  filter(!is.na(amount), year >= 2020) %>%
  group_by(region) %>%
  summarise(total = sum(amount), n = n(), .groups = "drop") %>%
  arrange(desc(total))

summarise_by <- function(df, grp) {
  df |>
    group_by({{ grp }}) |>
    summarise(avg = mean(amount, na.rm = TRUE), .groups = "drop") |>
    mutate(share = avg / sum(avg))
}

widen <- function(df) {
  df %>%
    select(region, year, amount) %>%
    pivot_wider(names_from = year, values_from = amount)
}

plot_sales <- function(df) {
  ggplot(df, aes(x = year, y = amount, colour = region)) +
    geom_line()
}

tidy_text <- function(df) {
  df %>% mutate(across(where(is.character), str_trim))
}

# == outdated: each call flagged as superseded or deprecated ==
old1 <- sales %>% mutate_at(vars(amount), funs(round(., 1)))
old2 <- sales %>% gather(key, value, -region)
old3 <- sales %>% group_by(region) %>% do(head(., 1))
old4 <- sales %>% top_n(3, amount)

old5 <- function(df) {
  df %>% summarise(rows = list(cur_data()))
}

# == bugs: each line flagged ==
bug1 <- sales %>% filterr(amount > 0)
bug2 <- sales %>% dplyr::summarize_everything()
bug3 <- function(df) df %>% mutate(z = undefined_helper(amount))
