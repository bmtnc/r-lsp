# Fixture for tests/smoke_test.py: planted bugs in base-R style code.
# Each `# bug` line must be flagged; `# ok` lines must not be.
library(dplyr)

total <- summ(c(1, 2, 3))  # bug: undefined function at top level

f <- function(a, b) {
  a + b + c_undefined  # bug: undefined variable
}

f(a = 1, bb = 2)
f(1, 2, 3)

z <- dplyr::mutatee(mtcars, k = 1)  # bug: not exported

g <- function(df) {
  mutate(df, y = x * 2)  # ok: x is a column
}

h <- function(df) {
  unused <- 5
  filterr(df, mpg > 20)  # bug: undefined function
}

badName=1
if (TRUE) {print("x")}
