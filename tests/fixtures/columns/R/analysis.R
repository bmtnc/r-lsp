# Fixture for tests/smoke_test.py (r-colcheck). Lines marked `# bad` must be
# reported; every other line must stay silent. data/scores.rds is created by
# the test.
library(tidyverse)

sales <- read_csv("data/sales.csv")
scores <- readRDS("data/scores.rds")
threshold <- 100

ok1 <- sales %>% filter(amount > threshold, year >= 2020) %>% group_by(region) %>% summarise(total = sum(amount), n = n())
ok2 <- ok1 %>% mutate(share = total / sum(total)) %>% arrange(desc(share))
ok3 <- sales |> select(region, year, amount) |> rename(amt = amount) |> mutate(big = amt > 5)
ok4 <- scores %>% filter(score > 1)
ok5 <- mtcars %>% mutate(kpl = mpg * 0.425)
ok6 <- tibble(a = 1:3, b = letters[1:3]) %>% filter(a > 1)
ok7 <- sales %>% pivot_wider(names_from = year, values_from = amount) %>% select(`2020`)
ok8 <- function(df) df %>% filter(whatever > 1)
ok9 <- sales %>% mutate(across(where(is.numeric), scale)) %>% select(zzz)
ok10 <- sales %>% count(region, name = "rows") %>% filter(rows > 2)
p_ok <- ggplot(sales, aes(x = year, y = amount, colour = region)) + geom_point()

bad1 <- sales %>% filter(amt > 0)                                                   # bad
bad2 <- sales %>% group_by(regoin) %>% summarise(t = sum(amount))                   # bad
bad3 <- ok1 %>% mutate(x = amount)                                                  # bad
bad4 <- ok3 %>% select(amount)                                                      # bad
bad5 <- scores %>% mutate(z = scor * 2)                                             # bad
bad6 <- mtcars %>% filter(mpgg > 20)                                                # bad
bad7 <- ok6 %>% mutate(c = aa + 1)                                                  # bad
bad8 <- ggplot(sales, aes(x = yr, y = amount)) + geom_line(aes(colour = regionn))   # bad
bad9 <- sales |> summarise(m = mean(amount), .by = region) |> mutate(k = year)      # bad
bad10 <- sales |> summarise(m = mean(amount), .by = regoin)                          # bad
bad11 <- sales %>% mutate(m = mean(amount), .by = c(region, yaer))                   # bad
bad12 <- sales %>% filter(amount > mean(amount), .by = regin)                        # bad
bad13 <- sales %>% summarise(m = mean(amount), .by = dplyr::all_of("regoin"))            # bad
bad14 <- sales %>% select(all_of(c("region", "amuont")))                                 # bad
bad15 <- sales %>% group_by(across(all_of("regoin"))) %>% summarise(m = mean(amount))       # bad
bad16 <- sales %>% slice_max(amount, by = all_of("regoin"))                                  # bad
bad17 <- sales %>% mutate(across(c(regoin, amount), mean))                                   # bad
bad18 <- sales %>% filter(if_any(c(amuont), ~ .x > 1))                                       # bad
bad19 <- sales %>% group_by(pick(regoin))                                                    # bad
bad20 <- sales %>% summarise(across(all_of(c("amount", "yaer")), mean), .by = region)        # bad
bad21 <- sales %>% rowwise() %>% mutate(s = sum(c_across(amuont:year)))                      # bad
bad22 <- sales %>% group_by(across(c(region, year))) %>% summarise(m = mean(amount)) %>% mutate(z = regoin)  # bad
bad23 <- sales %>% group_by(yr = year * 2) %>% summarise(m = mean(amount)) %>% mutate(z = yrr)   # bad
