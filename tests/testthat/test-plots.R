test_that("plotting functions run on demo data", {
  data(demo_carbon_market)
  features <- carbon_features(demo_carbon_market)
  features$loss <- c(rep(NA_real_, 5), -features$return[-seq_len(5)])
  features$var <- stats::quantile(features$loss, 0.95, na.rm = TRUE)
  warnings <- early_warning_levels(
    var = seq_len(nrow(features)),
    cvar = seq_len(nrow(features)) + 1,
    train_index = seq_len(120)
  )
  plot_data <- cbind(features, warnings)

  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(plot_price_series(demo_carbon_market))
  expect_silent(plot_loss_distribution(features$loss, var = features$var[1]))
  expect_silent(plot_var_backtest(plot_data))
  expect_silent(plot_warning_timeline(plot_data))
  expect_silent(plot_risk_score(plot_data))
})
