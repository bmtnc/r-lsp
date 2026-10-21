# A fake in-house data package: fetch_table() reaches a database the checker
# cannot open, through the result of a function factory (a call whose head
# is itself a call, the shape that used to crash the checker); clean_orders()
# cleans the raw frame with dplyr verbs, through an internal helper, so its
# output columns can be read from its code; load_clean() wraps a loader whose
# literal key the schema cache knows.
fetch_table <- function(date, name) {
  getter <- function() stop("no database in a test package")
  base::identity(getter)()
}

clean_orders <- function(raw, customers) {
  x <- keep_open(raw)
  x <- dplyr::select(x, id, customer_id, amount)
  dplyr::rename(x, total = amount)
}

keep_open <- function(df) dplyr::filter(df, status == "open")

load_clean <- function(date) {
  raw <- fetch_table(date, "orders")
  dplyr::select(raw, id, amount)
}

# Output columns that cannot be read statically.
opaque <- function(df) do.call(rbind, list(df))

# Helpers whose output is the same every call but whose calls carry no
# literal: covered by a cache entry keyed on the function name.
taxonomy <- function(products, categories) stop("no database in a test package")
fixed_table <- function() stop("no database in a test package")
