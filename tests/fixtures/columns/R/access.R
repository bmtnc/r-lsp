# Fixture for tests/smoke_test.py (r-colcheck: base-R column access and
# in-place column writes). Lines marked `# bad` must be reported; every other
# line must stay silent. data/sales.csv has region, year, amount, customer.
library(tidyverse)
sales <- read_csv("data/sales.csv")
wanted <- c("region", "amount")

d01 <- sales$amount * 2
d02 <- sales$amuont * 2                                                             # bad
d03 <- sales[["year"]]
d04 <- sales[["yaer"]]                                                              # bad
d05 <- sales[, c("region", "amount")]
d06 <- sales[, c("region", "amuont")]                                               # bad
d07 <- sales["customer"]
d08 <- sales["custmer"]                                                             # bad
d09 <- sales[sales$year > 2019, wanted]
d10 <- sales[sales$yeer > 2019, wanted]                                             # bad
d11 <- sales[[2]]
d12 <- sales[-1]
d13 <- sum(sales$amount) / nrow(sales[sales$region == "North", ])
d14 <- mean(sales$amont)                                                            # bad
d15 <- sales |> filter(.data[["region"]] == "North", .data$amount > 1)
d16 <- sales |> filter(.data[["regoin"]] == "North")                                # bad
d17 <- sales |> mutate(z = .data$amuont + 1)                                        # bad
d18 <- sales |> filter(get("region") == "North", !!sym("amount") > 1)
d19 <- sales |> filter(get("regoin") == "North")                                    # bad
d20 <- sales |> arrange(!!sym("amuont"))                                            # bad
col <- "amount"
d21 <- sales |> filter(.data[[col]] > 1, .env$col == "amount")
d22 <- sales |> filter(.data[[Sys.getenv("COL")]] > 1)
d23 <- sales |> filter(get("region", envir = globalenv()) == "North")
parts <- split(sales, sales$region)
d24 <- parts$North |> filter(amount > 1)
shadow <- function(sales) sales |> filter(nothere == 1)
# writes: a column added in place is known from here on; a renamed frame too
sales$extra <- 1
sales[["other"]] <- sales$amount * 2
sales[, "third"] <- 3
d25 <- sales |> filter(extra == 1, other > 1, third == 3) |> select(region, extra)
d26 <- sales |> select(extraa)                                                      # bad
sales$other <- NULL
d27 <- sales |> select(other)                                                       # bad
scores <- readRDS("data/scores.rds")
names(scores) <- c("key", "value")
d28 <- scores |> filter(value > 1) |> select(key)
d29 <- scores |> select(score)                                                      # bad
scores[[paste0("v", 2)]] <- 1
d30 <- scores |> select(anything_goes)
dt <- read_csv("data/sales.csv")
dt[, ratio := amount / year]
d31 <- dt |> select(ratio, region)
d32 <- dt |> select(ratioo)                                                         # bad
dt[, (new_names) := 1]
d33 <- dt |> select(whatever)
