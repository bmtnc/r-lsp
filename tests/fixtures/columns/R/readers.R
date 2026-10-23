# Fixture for tests/smoke_test.py (r-colcheck: reader arguments that change
# the column names). Lines marked `# bad` must be reported; every other line
# must stay silent. data/sales.csv has the header region,year,amount,customer;
# data/noheader.csv has no header line; data/spaces.csv has the header
# `my col,amount`; data/skip1.csv has a comment line before the header.
library(tidyverse)

r01 <- read_csv("data/sales.csv", col_select = c(region, amount)) |> mutate(z = region + amount)
r02 <- read_csv("data/sales.csv", col_select = c(region, amount)) |> mutate(z = year)                  # bad
r03 <- read_csv("data/sales.csv", col_select = -year) |> mutate(z = region + amount + customer)
r04 <- read_csv("data/sales.csv", col_select = -c(year, customer)) |> mutate(z = year)                  # bad
r05 <- read_csv("data/sales.csv", col_select = c(region, amuont)) |> mutate(z = region)                # bad
r06 <- read_csv("data/sales.csv", col_select = starts_with("a")) |> mutate(z = amount)
r07 <- read_csv("data/sales.csv", col_select = region:amount) |> mutate(z = customer)                   # bad
keep <- c("region", "year")
r08 <- read_csv("data/sales.csv", col_select = all_of(keep)) |> mutate(z = region + year)
r09 <- read_csv("data/sales.csv", col_select = all_of(keep)) |> mutate(z = amount)                     # bad
r10 <- read_csv("data/sales.csv", col_types = cols_only(region = "c", amount = "d")) |> mutate(z = year)   # bad
r11 <- read_csv("data/sales.csv", col_types = cols(year = col_skip())) |> mutate(z = region + amount)
r12 <- read_csv("data/sales.csv", col_types = cols(year = col_skip())) |> mutate(z = year)              # bad
r13 <- read_csv("data/sales.csv", col_types = cols(.default = "c")) |> mutate(z = year)
r14 <- read_csv("data/sales.csv", id = "src_file") |> mutate(z = src_file)
r15 <- read_csv("data/sales.csv", id = "src_file") |> select(src_file, region) |> mutate(z = amount)   # bad
r16 <- read_csv("data/noheader.csv", col_names = FALSE) |> mutate(z = X1 + X4)
r17 <- read_csv("data/noheader.csv", col_names = FALSE) |> mutate(z = X5)                               # bad
r18 <- read_csv("data/noheader.csv", col_names = c("a", "b", "c", "d")) |> mutate(z = a + d)
r19 <- read_csv("data/noheader.csv", col_names = c("a", "b")) |> mutate(z = a + X3 + X4)
r20 <- read_csv("data/noheader.csv", col_names = c("a", "b", "c", "d")) |> mutate(z = X1)              # bad
r21 <- read_csv("data/skip1.csv", skip = 1) |> mutate(z = region)
r22 <- read_csv("data/skip1.csv", comment = "#") |> mutate(z = region)
r23 <- read_csv("data/skip1.csv", skip = 1) |> mutate(z = regoin)                                       # bad
r24 <- read.csv("data/spaces.csv") |> mutate(z = my.col)
r25 <- read.csv("data/spaces.csv") |> mutate(z = `my col`)                                              # bad
r26 <- read.csv("data/spaces.csv", check.names = FALSE) |> mutate(z = `my col`)
r27 <- read.csv("data/noheader.csv", header = FALSE) |> mutate(z = V1)
r28 <- read.csv("data/noheader.csv", header = FALSE) |> mutate(z = X1)                                  # bad
r29 <- read.csv("data/noheader.csv", FALSE) |> mutate(z = V1)
r30 <- read.csv("data/sales.csv", colClasses = c(year = "NULL")) |> mutate(z = region)
r31 <- read.csv("data/sales.csv", colClasses = c(year = "NULL")) |> mutate(z = year)                    # bad
r32 <- read.csv("data/sales.csv", row.names = 1) |> mutate(z = year)
r33 <- read.csv("data/sales.csv", row.names = 1) |> mutate(z = region)                                  # bad
r34 <- read.table("data/sales.csv", header = TRUE, sep = ",") |> mutate(z = region)
r35 <- read.table("data/sales.csv") |> mutate(z = V1)
r36 <- read.delim("data/sales.csv", sep = ",") |> mutate(z = amount)
r37 <- data.table::fread("data/sales.csv", select = c("region", "amount")) |> mutate(z = region)
r38 <- data.table::fread("data/sales.csv", select = c("region", "amount")) |> mutate(z = year)          # bad
r39 <- data.table::fread("data/sales.csv", drop = "year") |> mutate(z = year)                           # bad
r40 <- data.table::fread("data/sales.csv", drop = 2) |> mutate(z = region + amount + customer)
r41 <- data.table::fread("data/noheader.csv", header = FALSE) |> mutate(z = V1)
r42 <- data.table::fread("data/spaces.csv") |> mutate(z = `my col`)
r43 <- vroom::vroom("data/sales.csv", col_select = c(region, amount), id = "f") |> mutate(z = f + region)
r44 <- vroom::vroom("data/sales.csv", col_select = c(region, amount)) |> mutate(z = year)                # bad
# an argument the checker cannot read: the frame is unknown and noted, never checked against the raw header
r45 <- read_csv("data/sales.csv", col_select = all_of(wanted_cols)) |> mutate(z = year)
r46 <- read.csv("data/sales.csv", header = has_header) |> mutate(z = year)
