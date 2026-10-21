# Stand-in for a team's S3 loader: takes a dataset key, returns a frame.
# Nothing here is a local file, so r-colcheck cannot open the data itself.
s3_read <- function(key) {
  switch(key,
    "returns/monthly" = data.frame(date = as.Date("2024-01-01"), fund_id = "F1", ret = 0.01),
    "holdings/latest" = data.frame(fund_id = "F1", security = "X", weight = 0.5),
    "meta/funds" = data.frame(fund_id = "F1", name = "Fund One"),
    stop("unknown key: ", key))
}
