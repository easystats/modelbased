skip_on_os(c("mac", "linux"))
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")
skip_if_not_installed("ggeffects")

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


test_that("estimate_means - print multiple by's", {
  data(efc, package = "ggeffects")

  # make categorical
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex"))
  fit <- lm(neg_c_7 ~ c12hour * barthtot * c161sex * c172code, data = efc)
  out <- estimate_means(
    fit,
    c("c12hour", "c172code", "c161sex"),
    length = 4,
    backend = "marginaleffects"
  )
  expect_snapshot(print(out, table_width = Inf), variant = "windows")

  fit <- lm(neg_c_7 ~ c12hour * barthtot * c161sex * c172code * e16sex, data = efc)
  out <- estimate_means(
    fit,
    c("c12hour", "barthtot = [sd]", "c161sex", "c172code", "e16sex"),
    backend = "marginaleffects",
    length = 3
  )
  expect_snapshot(print(out, table_width = Inf), variant = "windows")
})


test_that("estimate_means - full labels", {
  data(efc, package = "ggeffects")
  # make categorical
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "nur_pst"))

  fit <- lm(neg_c_7 ~ c161sex * c172code * e16sex * nur_pst, data = efc)
  pr <- estimate_means(fit, c("c161sex", "c172code", "e16sex", "nur_pst"), backend = "marginaleffects")
  expect_snapshot(print(pr, table_width = Inf), variant = "windows")
  expect_snapshot(print(pr, full_labels = FALSE, table_width = Inf), variant = "windows")

  fit <- lm(neg_c_7 ~ c161sex * c172code * e16sex * nur_pst * negc7d, data = efc)
  pr <- estimate_means(fit, c("c161sex", "c172code", "e16sex", "nur_pst", "negc7d"), backend = "marginaleffects")
  expect_snapshot(print(pr, table_width = Inf), variant = "windows")
  expect_snapshot(print(pr, full_labels = FALSE, table_width = Inf), variant = "windows")
})
