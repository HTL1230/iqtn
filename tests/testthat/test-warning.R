test_that("early_warning_levels returns four-level labels", {
  out <- early_warning_levels(
    var = 1:10,
    cvar = 2:11,
    train_index = 1:6
  )
  expect_equal(nrow(out), 10)
  expect_true(all(out$warning_level %in% 0:3))
})
