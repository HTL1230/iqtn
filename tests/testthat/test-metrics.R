test_that("pinball_loss computes expected values", {
  expect_equal(pinball_loss(c(1, 2), c(1, 1), 0.5), 0.25)
})

test_that("VaR exceedance rate is computed", {
  expect_equal(var_exceedance_rate(c(1, 3, 4), c(2, 2, 5)), 1 / 3)
})

test_that("CVaR integrates available upper-tail quantiles", {
  expect_equal(
    cvar_from_quantiles(c(1, 2, 3), c(0.95, 0.975, 0.99), 0.95),
    1.875,
    tolerance = 1e-6
  )
})
