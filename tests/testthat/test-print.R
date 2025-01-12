test_that("estimate_slopes - print summary", {
  set.seed(333)
  # Generate data
  data <- bayestestR::simulate_correlation(r = 0.85, n = 1000, names = c("y", "x"), mean = c(100, 0), sd = c(15, 1))
  model_lm <- lm(y ~ x, data = data)

  deriv <- estimate_slopes(model_lm, trend = "x", by = "x")
  expect_snapshot(deriv)
  expect_snapshot(summary(deriv))
})
