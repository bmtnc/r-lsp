# Fixture for tests/smoke_test.py (r-colcheck with the schema cache). Lines
# marked `# bad` must be reported; `# bad-after-learn` only once the test has
# run `r-colcheck learn "holdings/latest" ...`; `# note` lines must produce an
# unresolved-source note before that and none after. Every other line must
# stay silent. .colcheck/schemas.json ships with "returns/monthly" only.
source("R/loaders.R")
library(dplyr)

returns <- s3_read("returns/monthly")
ok1 <- returns |> filter(ret > 0) |> mutate(yr = format(date, "%Y"))
ok2 <- returns %>% group_by(fund_id) %>% summarise(m = mean(ret)) %>% arrange(desc(m))
bad1 <- returns |> filter(retrun > 0)                                               # bad
bad2 <- returns |> group_by(fund) |> summarise(m = mean(ret))                       # bad
bad3 <- s3_read("returns/monthly") |> mutate(x = rett * 2)                          # bad
bad4 <- ok2 |> select(ret)                                                          # bad

holdings <- s3_read("holdings/latest")                                              # note
ok3 <- holdings |> filter(weight > 0.1)
bad5 <- holdings |> filter(wieght > 0)                                              # bad-after-learn

meta <- s3_read("meta/funds")
n_funds <- nrow(meta)
print(meta)

annotated <- load_from_somewhere("whatever")   # cols: returns/monthly
bad6 <- annotated |> select(fundid)                                                 # bad
ok4 <- annotated |> select(fund_id, ret)
