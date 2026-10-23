# Fixture for tests/smoke_test.py (r-colcheck through verbs that change the
# column set). Lines marked `# bad` must be reported; every other line must
# stay silent. Each block pins one rule: a:b ranges, join suffixes and
# renamed keys, the groups summarise() leaves, pivot_longer() with several
# names_to, verbs that keep the set, %<>% and assignment inside a branch.
library(tidyverse)
library(magrittr)
sales <- read_csv("data/sales.csv")
other <- tibble(region = "North", amount = 1, pop = 5)
lookup <- tibble(id = "A", score = 1)
# a:b is every column from a to b, in either direction
r1 <- sales |> select(region:amount) |> mutate(z = year)
r2 <- sales |> select(amount:region) |> mutate(z = year + region)
r3 <- sales |> select(-(region:amount)) |> mutate(z = customer)
r4 <- sales |> mutate(across(region:amount, as.character, .names = "{.col}_c")) |> mutate(z = year_c)
r5 <- sales |> pivot_longer(region:amount, names_to = "k", values_to = "v") |> mutate(z = year)       # bad
r6 <- sales |> select(region:bogus)                                                                    # bad
r7 <- sales |> select(-(region:amount)) |> mutate(z = year)                                           # bad
# joins: overlapping non-key columns get .x/.y (or the suffix given), y's key is dropped
j1 <- sales |> left_join(other, by = "region") |> mutate(z = amount.x + amount.y + pop)
j2 <- sales |> left_join(other, by = join_by(region)) |> mutate(z = amount.y)
j3 <- sales |> left_join(other, by = "region", suffix = c("_s", "_o")) |> mutate(z = amount_s + amount_o)
j4 <- sales |> left_join(lookup, by = c("customer" = "id")) |> mutate(z = customer + score)
j5 <- sales |> left_join(lookup, by = join_by(customer == id)) |> mutate(z = score)
j6 <- sales |> left_join(lookup, by = join_by(x$customer == y$id), keep = TRUE) |> mutate(z = id)
j7 <- sales |> left_join(other) |> mutate(z = pop + region + amount)
j8 <- sales |> group_by(amount) |> left_join(other, by = "region") |> summarise(n = n()) |> mutate(z = amount.x)
j9 <- sales |> semi_join(other, by = "region") |> mutate(z = amount)
j10 <- sales |> full_join(other, by = "region") |> mutate(z = amount)                                 # bad
j11 <- sales |> left_join(lookup, by = c("customer" = "id")) |> mutate(z = id)                        # bad
j12 <- sales |> left_join(lookup, by = join_by(customer == id)) |> mutate(z = id)                     # bad
j13 <- sales |> left_join(lookup, by = c("nope" = "id"))                                              # bad
j14 <- sales |> left_join(other, by = c("region", "nope2"))                                           # bad
j15 <- sales |> left_join(lookup, by = c("customer" = "idd"))                                         # bad
j16 <- sales |> anti_join(other, by = "region") |> mutate(z = pop)                                    # bad
# summarise() keeps all but the last grouping column; count()/add_count() keep the groups, tally() drops the last
g1 <- sales |> group_by(region, year) |> summarise(m = mean(amount)) |> summarise(t = sum(m)) |> mutate(z = region)
g2 <- sales |> group_by(region, year) |> summarise(m = mean(amount), .groups = "keep") |> summarise(t = sum(m)) |> mutate(z = region + year)
g3 <- sales |> group_by(region, year) |> summarise(m = mean(amount), .groups = "rowwise") |> summarise(t = sum(m)) |> mutate(z = region + year)
g4 <- sales |> group_by(region, year) |> count() |> summarise(t = sum(n)) |> mutate(z = paste(region, year))
g5 <- sales |> group_by(region, year) |> tally() |> summarise(t = sum(n)) |> mutate(z = region)
g6 <- sales |> group_by(region, year) |> add_count() |> summarise(t = sum(n)) |> mutate(z = region)
g7 <- sales |> group_by(region) |> count(year) |> summarise(t = sum(n)) |> mutate(z = region)
g8 <- sales |> group_by(region, year) |> summarise(m = mean(amount)) |> count() |> mutate(z = region)
g9 <- sales |> group_by(region, year) |> summarise(m = mean(amount)) |> transmute(k = m) |> mutate(z = region)
g10 <- sales |> group_by(region, year) |> summarise(m = mean(amount), .groups = "drop") |> summarise(t = sum(m)) |> mutate(z = region)   # bad
g11 <- sales |> group_by(region) |> summarise(m = mean(amount)) |> summarise(t = sum(m)) |> mutate(z = region)                          # bad
g12 <- sales |> group_by(region, year) |> summarise(m = mean(amount)) |> summarise(t = sum(m)) |> mutate(z = year)                      # bad
g13 <- sales |> group_by(region, year) |> reframe(m = mean(amount)) |> summarise(t = sum(m)) |> mutate(z = region)                      # bad
g14 <- sales |> group_by(region, year) |> tally() |> summarise(t = sum(n)) |> mutate(z = year)                                           # bad
# pivot_longer(): every names_to entry becomes a column; .value takes the pieces of the pivoted names
p1 <- sales |> mutate(a_x = 1, b_x = 2) |> pivot_longer(c(a_x, b_x), names_to = c("k", "set"), names_sep = "_") |> mutate(z = k + set + value)
p2 <- sales |> mutate(a_x = 1, b_x = 2) |> pivot_longer(c(a_x, b_x), names_to = c(".value", "set"), names_sep = "_") |> mutate(z = a + b + set)
p3 <- sales |> pivot_longer(c(amount, year), names_prefix = "a", names_to = "k") |> mutate(z = k + value)
p4 <- sales |> mutate(a_x = 1, b_x = 2) |> pivot_longer(c(a_x, b_x), names_to = c("k", "set"), names_sep = "_") |> mutate(z = name)      # bad
p5 <- sales |> mutate(a_x = 1, b_x = 2) |> pivot_longer(c(a_x, b_x), names_to = c(".value", "set"), names_sep = "_") |> mutate(z = value)  # bad
# verbs that keep the column set, or change it in a way that can be read off the call
c1 <- sales |> replace_na(list(amount = 0)) |> mutate_if(is.numeric, round) |> mutate_all(as.character) |> na.omit() |> filter(amount > 1)
c2 <- sales |> separate_rows(region) |> complete(region, year) |> sample_n(1) |> top_n(1, amount) |> filter_at(vars(amount), all_vars(. > 1)) |> filter(amount > 1)
c3 <- sales |> rownames_to_column("rn") |> add_column(z = 1) |> filter(rn == "1", z > 0, amount > 1)
c4 <- sales |> unite("key", region, year) |> filter(key == "a", amount > 1)
c5 <- sales |> unite("key", region, year, remove = FALSE) |> filter(key == "a", region == "b")
c6 <- sales |> setNames(c("a", "b", "c", "d")) |> filter(a == "x", d == "y")
c7 <- sales |> rename_with(toupper) |> rename_all(tolower) |> filter(amount > 1)
c8 <- sales |> rename_with(toupper, starts_with("am")) |> filter(AMOUNT > 1, region == "x")
c9 <- sales |> column_to_rownames("customer") |> filter(amount > 1)
c10 <- sales |> add_row(region = "x", amount = 1) |> filter(amount > 1)
c11 <- sales |> mutate_if(is.numeric, list(r = round)) |> filter(amuont_r > 1)
c12 <- sales |> replace_na(list(amount = 0)) |> filter(amuont > 1)                                     # bad
c13 <- sales |> mutate_if(is.numeric, round) |> filter(amuont > 1)                                     # bad
c14 <- sales |> na.omit() |> filter(amuont > 1)                                                        # bad
c15 <- sales |> rownames_to_column("rn") |> filter(rnn == "1")                                         # bad
c16 <- sales |> unite("key", region, year) |> filter(region == "b")                                    # bad
c17 <- sales |> setNames(c("a", "b", "c", "d")) |> filter(amount > 1)                                  # bad
c18 <- sales |> rename_with(toupper) |> filter(amount > 1)                                             # bad
c19 <- sales |> column_to_rownames("customer") |> filter(customer == "A")                             # bad
c20 <- sales |> add_row(regoin = "x")                                                                  # bad
c21 <- sales |> filter_at(vars(amuont), all_vars(. > 1))                                               # bad
c22 <- sales |> add_column(z = 1) |> filter(zz > 1)                                                    # bad
# %<>% assigns its result; a frame assigned inside a branch has the columns of either outcome
sales2 <- sales
sales2 %<>% mutate(flag = amount > 1)
m1 <- sales2 |> select(flag, region)
m2 <- sales2 |> select(flagg)                                                                           # bad
sales3 <- sales
if (nrow(sales3) > 100) sales3 <- sales3 |> select(-customer)
m3 <- sales3 |> mutate(z = customer + region)
m4 <- sales3 |> mutate(z = customerr)                                                                   # bad
sales4 <- sales
if (nrow(sales4) > 100) sales4 <- sales4 |> select(region) else sales4 <- sales4 |> select(year)
m5 <- sales4 |> mutate(z = region + year)
m6 <- sales4 |> mutate(z = amount)                                                                      # bad
for (i in 1:2) sales4 <- sales4 |> mutate(extra = i)
m7 <- sales4 |> mutate(z = extra)
m8 <- sales4 |> mutate(z = extraa)                                                                      # bad
