test_that("future_loss and add_future_loss create aligned labels", {
  x <- data.frame(close = c(100, 90, 99, 110))
  out <- future_loss(x, horizon = 1)
  expect_equal(out[1], -100 * log(90 / 100))
  expect_true(is.na(out[4]))

  with_loss <- add_future_loss(x, horizons = c(1, 2))
  expect_true(all(c("loss_h1", "loss_h2") %in% names(with_loss)))
  expect_true(is.na(with_loss$loss_h2[3]))
})
