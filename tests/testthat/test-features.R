test_that("carbon_features adds market-state columns", {
  x <- data.frame(
    date = as.Date("2026-01-01") + 0:4,
    open = c(70, 71, 72, 71, 73),
    high = c(72, 73, 73, 74, 75),
    low = c(69, 70, 71, 70, 72),
    close = c(71, 72, 71, 73, 74),
    volume = c(1000, 1200, 900, 1400, 1600),
    value = c(71000, 86400, 63900, 102200, 118400)
  )
  out <- carbon_features(x, volatility_windows = 3)
  expect_true(all(c("return", "range", "illiquidity", "volatility_3", "max_loss_3") %in% names(out)))
  expect_equal(nrow(out), nrow(x))
})
