test_that("flag_missing sums", {
  expect_equal(unname(flag_missing(c(1, 2))), 3)
})
