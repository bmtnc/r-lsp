# Fixture for tests/smoke_test.py (r-colcheck through project functions).
# Lines marked `# bad` must be reported; every other line must stay silent.
# Bodies are checked when a function is called with a known frame, with both
# pipes, and the function's result keeps the column set.
library(tidyverse)
source("R/helpers_src.R")

sales <- read_csv("data/sales.csv")

clean <- function(df, min_amount = 0) {
  df |>
    filter(amount > min_amount) |>
    mutate(big = amount > 100)
}

summarise_region <- function(df) df %>% group_by(region) %>% summarise(total = sum(amount))

broken <- function(df) {
  n <- nrow(df)
  df %>% filter(regoin == "North")                                                   # bad
}

never_called <- function(df) df |> filter(nope > 1)

with_return <- function(df) {
  out <- df |> select(region, amount)
  return(out)
}

cleaned <- clean(sales)
ok1 <- cleaned |> filter(big, amount > 1)
bad1 <- cleaned |> filter(bigg)                                                       # bad
s <- summarise_region(sales)
ok2 <- s %>% arrange(desc(total))
bad2 <- s |> mutate(x = amount)                                                       # bad
b <- broken(sales)
piped <- sales %>% clean() %>% mutate(ratio = amount / 2)
bad3 <- sales %>% clean(min_amount = 5) %>% filter(amt > 0)                           # bad
bad4 <- sales |> clean() |> select(big, amount, customer, yr)                         # bad
shared <- sales %>% add_share()
bad5 <- shared |> filter(shar > 0.5)                                                  # bad
ok3 <- with_return(sales) |> filter(amount > 2)
bad6 <- with_return(sales) |> filter(customer == "A", year > 2020)                    # bad
unknown_in <- never_called(something_else)

# Columns made earlier in the same mutate() are visible to later arguments;
# a column a function tests for with names() counts as present inside it.
seq_ok <- sales |> mutate(half = amount / 2, quarter = half / 2, flag = quarter > 1)
seq_bad <- sales |> mutate(flag = quarter > 1, quarter = amount / 4)                 # bad
guarded <- function(df) {
  if (!"share" %in% names(df)) {
    return(df |> mutate(share = 0))
  }
  df |> filter(share > 0.1)
}
g1 <- guarded(sales)

# A helper that rewrites columns in place keeps the column set; one whose
# output cannot be worked out stops tracking with a note at the call site
# naming the step inside it, also for a direct call.
to_chr <- function(df, cols) df %>% mutate(across(all_of(cols), as.character))
widen <- function(df) {
  df %>%
    pivot_wider(names_from = year, values_from = amount)
}
opaque_fn <- function(df) do.call(rbind, df)
tc <- to_chr(sales, c("year"))
bad7 <- tc |> filter(yeer > 1)                                                        # bad
wd <- widen(sales)
wd |> filter(zz > 1)
op <- sales %>% opaque_fn()
op |> filter(zz > 1)
ws <- widen_src(sales)
ws |> filter(zz > 1)
