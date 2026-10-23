# Fixture for tests/smoke_test.py (r-colcheck: readxl and arrow reader
# arguments). The test writes data/datasets.xlsx (readxl's example workbook:
# sheets mtcars, chickwts, quakes) and data/sales.parquet, and runs this file
# only when both packages are installed. Lines marked `# bad` must be
# reported; every other line must stay silent.
library(dplyr)
x01 <- readxl::read_excel("data/datasets.xlsx", sheet = "chickwts") |> mutate(z = weight + feed)
x02 <- readxl::read_excel("data/datasets.xlsx", sheet = "chickwts") |> mutate(z = mpg)                          # bad
x03 <- readxl::read_excel("data/datasets.xlsx") |> mutate(z = mpg)
x04 <- readxl::read_excel("data/datasets.xlsx", sheet = 2) |> mutate(z = weight)
x05 <- readxl::read_excel("data/datasets.xlsx", sheet = 2) |> mutate(z = mpg)                                   # bad
x06 <- readxl::read_excel("data/datasets.xlsx", sheet = "chickwts", col_names = FALSE) |> mutate(z = ...1)
x07 <- readxl::read_excel("data/datasets.xlsx", sheet = "chickwts", col_names = FALSE) |> mutate(z = weight)    # bad
x08 <- readxl::read_excel("data/datasets.xlsx", sheet = "chickwts", skip = 1, col_names = c("w", "f")) |> mutate(z = w + f)
x09 <- readxl::read_excel("data/datasets.xlsx", sheet = "chickwts", range = "B1:B5") |> mutate(z = feed)
x10 <- readxl::read_excel("data/datasets.xlsx", sheet = "chickwts", range = "B1:B5") |> mutate(z = weight)      # bad
x11 <- readxl::read_excel("data/datasets.xlsx", sheet = which_sheet) |> mutate(z = weight)
x12 <- arrow::read_parquet("data/sales.parquet", col_select = c(region, amount)) |> mutate(z = region)
x13 <- arrow::read_parquet("data/sales.parquet", col_select = c(region, amount)) |> mutate(z = year)           # bad
x14 <- arrow::read_parquet("data/sales.parquet") |> mutate(z = year)
x15 <- arrow::open_dataset("data/sales_ds") |> filter(amuont > 1) |> collect()                                 # bad
x16 <- arrow::open_dataset("data/sales_ds") |> filter(year == 2020) |> select(region, amount, customer) |> collect()
