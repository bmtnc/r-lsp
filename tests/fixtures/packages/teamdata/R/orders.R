# A fake in-house data package: fetch_table() reaches a database the checker
# cannot open; clean_orders() cleans the raw frame with dplyr verbs, through
# an internal helper, so its output columns can be read from its code.
fetch_table <- function(date, name) stop("no database in a test package")

clean_orders <- function(raw, customers) {
  x <- keep_open(raw)
  x <- dplyr::select(x, id, customer_id, amount)
  dplyr::rename(x, total = amount)
}

keep_open <- function(df) dplyr::filter(df, status == "open")

# Output columns that cannot be read statically.
opaque <- function(df) do.call(rbind, list(df))
