skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")

test_that("estimate_slopes - print summary", {
  skip_if_not_installed("MASS")
  set.seed(333)
  # Generate data
  data <- bayestestR::simulate_correlation(r = 0.85, n = 1000, names = c("y", "x"), mean = c(100, 0), sd = c(15, 1))
  model_lm <- lm(y ~ x, data = data)

  deriv <- estimate_slopes(model_lm, trend = "x", by = "x", backend = "emmeans")
  expect_snapshot(deriv, variant = "windows")
  expect_snapshot(summary(deriv), variant = "windows")

  deriv2 <- estimate_slopes(model_lm, trend = "x", by = "x", backend = "marginaleffects")
  expect_snapshot(deriv2, variant = "windows")
  expect_snapshot(summary(deriv2), variant = "windows")
})

test_that("estimate_slopes - print regular", {
  data(iris)
  model <- lm(Sepal.Length ~ Petal.Length * Species, data = iris)
  slopes <- estimate_slopes(model, trend = "Petal.Length", backend = "emmeans")
  expect_snapshot(print(slopes), variant = "windows")
  slopes2 <- estimate_slopes(model, trend = "Petal.Length", backend = "marginaleffects")
  expect_snapshot(print(slopes2), variant = "windows")
})
