test_that("historical_var and historical_cvar use only prior observations", {
  loss <- 1:10
  var <- historical_var(loss, alpha = 0.8, window = 5, min_obs = 5)
  cvar <- historical_cvar(loss, alpha = 0.8, window = 5, min_obs = 5)

  expect_true(all(is.na(var[1:5])))
  expect_equal(var[6], as.numeric(stats::quantile(1:5, 0.8, names = FALSE, type = 8)))
  expect_gte(cvar[6], var[6])
})
