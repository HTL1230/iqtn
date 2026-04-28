test_that("demo_carbon_market has the expected columns", {
  data(demo_carbon_market)
  expect_true(is.data.frame(demo_carbon_market))
  expect_true(all(c("date", "open", "high", "low", "close", "volume", "value") %in% names(demo_carbon_market)))
  expect_gt(nrow(demo_carbon_market), 100)
})
