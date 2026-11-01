users_test <- tibble::tribble(
  ~user_id, ~name,
  1,        "a"
)




orders_test <- tibble::tribble(
  ~user_id, ~amount,
  1,        10
)

test_that("g joins", {
  expect_equal(nrow(g(orders_test, users_test)), 1)
})
