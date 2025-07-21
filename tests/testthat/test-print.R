skip_on_os(c("mac", "linux"))
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")
skip_if_not_installed("withr")

test_that("estimate_slopes - print summary", {
  skip_if_not_installed("MASS")
  set.seed(333)
  # Generate data
  data <- bayestestR::simulate_correlation(r = 0.85, n = 1000, names = c("y", "x"), mean = c(100, 0), sd = c(15, 1))
  model_lm <- lm(y ~ x, data = data)

  deriv <- estimate_slopes(model_lm, trend = "x", by = "x", backend = "emmeans")
  expect_snapshot(estimate_slopes(model_lm, trend = "x", by = "x", backend = "emmeans"), variant = "windows")
  expect_snapshot(summary(deriv), variant = "windows")

  deriv2 <- estimate_slopes(model_lm, trend = "x", by = "x", backend = "marginaleffects")
  expect_snapshot(estimate_slopes(model_lm, trend = "x", by = "x", backend = "marginaleffects"), variant = "windows")
  expect_snapshot(summary(deriv2), variant = "windows")
})


test_that("estimate_slopes - print regular", {
  data(iris)
  model <- lm(Sepal.Length ~ Petal.Length * Species, data = iris)
  expect_snapshot(print(estimate_slopes(model, trend = "Petal.Length", backend = "emmeans")), variant = "windows")
  expect_snapshot(print(estimate_slopes(model, trend = "Petal.Length", backend = "marginaleffects")), variant = "windows")
})


test_that("estimate_means - print multiple by's", {
  data(efc, package = "modelbased")

  # make categorical
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex"))
  fit <- lm(neg_c_7 ~ c12hour * barthtot * c161sex * c172code, data = efc)
  expect_snapshot(print(estimate_means(fit, c("c12hour", "c172code", "c161sex"), length = 4, backend = "marginaleffects"), table_width = Inf), variant = "windows") # nolint

  fit <- lm(neg_c_7 ~ c12hour * barthtot * c161sex * c172code * e16sex, data = efc)
  expect_snapshot(print(estimate_means(fit, c("c12hour", "barthtot = [sd]", "c161sex", "c172code", "e16sex"), backend = "marginaleffects", length = 3), table_width = Inf), variant = "windows") # nolint
})


test_that("estimate_means - using display() to print multiple by's", {
  skip_if_not_installed("tinytable")
  data(efc, package = "modelbased")

  # make categorical
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex"))
  fit <- lm(neg_c_7 ~ c12hour * barthtot * c161sex * c172code, data = efc)
  expect_snapshot(display(estimate_means(fit, c("c12hour = c(50, 100)", "c172code", "c161sex")), format = "tt"))
})


test_that("estimate_means - full labels", {
  data(efc, package = "modelbased")
  # make categorical
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "nur_pst"))

  fit <- lm(neg_c_7 ~ c161sex * c172code * e16sex * nur_pst, data = efc)
  expect_snapshot(print(estimate_means(fit, c("c161sex", "c172code", "e16sex", "nur_pst"), backend = "marginaleffects"), table_width = Inf), variant = "windows") # nolint
  expect_snapshot(print(estimate_means(fit, c("c161sex", "c172code", "e16sex", "nur_pst"), backend = "marginaleffects"), full_labels = FALSE, table_width = Inf), variant = "windows") # nolint

  fit <- lm(neg_c_7 ~ c161sex * c172code * e16sex * nur_pst * negc7d, data = efc)
  expect_snapshot(print(estimate_means(fit, c("c161sex", "c172code", "e16sex", "nur_pst", "negc7d"), backend = "marginaleffects"), table_width = Inf), variant = "windows") # nolint
  expect_snapshot(print(estimate_means(fit, c("c161sex", "c172code", "e16sex", "nur_pst", "negc7d"), backend = "marginaleffects"), full_labels = FALSE, table_width = Inf), variant = "windows") # nolint
})


test_that("estimate_means - protect integers", {
  data(efc, package = "modelbased")
  efc$c161sex <- datawizard::to_factor(efc$c161sex)
  efc$e16sex <- datawizard::to_factor(efc$e16sex)
  model <- lm(neg_c_7 ~ barthtot + c160age * c161sex + e16sex, data = efc)

  expect_snapshot(estimate_expectation(model, by = c("c160age=[fivenum]", "c161sex")), variant = "windows")
  expect_snapshot(estimate_means(model, by = c("c160age=[fivenum]", "c161sex"), backend = "marginaleffects"), variant = "windows")
})


withr::with_options(
  list(marginaleffects_safe = FALSE),
  test_that("estimate_contrasts - by with special character", {
    data(efc, package = "modelbased")
    # make categorical
    efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex"))
    levels(efc$c172code) <- c("low", "mid", "high")
    fit <- lm(neg_c_7 ~ barthtot * c172code, data = efc)
    expect_identical(
      dim(estimate_contrasts(fit, "c172code", "barthtot = [sd]", backend = "marginaleffects", p_adjust = "holm")),
      c(9L, 10L)
    )
    expect_snapshot(print(estimate_contrasts(fit, "c172code", "barthtot = [sd]", backend = "marginaleffects", p_adjust = "holm"), table_width = Inf, zap_small = TRUE)) # nolint
    expect_snapshot(print(estimate_contrasts(fit, c("c172code", "barthtot = [sd]"), backend = "marginaleffects", p_adjust = "holm"), table_width = Inf, zap_small = TRUE)) # nolint
    expect_snapshot(print(estimate_contrasts(fit, "c172code", "barthtot = [sd]", backend = "marginaleffects"), table_width = Inf, zap_small = TRUE)) # nolint
    expect_snapshot(print(estimate_contrasts(fit, c("c172code", "barthtot = [sd]"), backend = "marginaleffects"), table_width = Inf, zap_small = TRUE)) # nolint
  })
)


test_that("estimate_means - by is list", {
  data(efc, package = "modelbased")
  # make categorical
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex"))
  levels(efc$c172code) <- c("low", "mid", "high")
  fit <- lm(neg_c_7 ~ e16sex + c161sex * c172code, data = efc)
  expect_snapshot(print(estimate_means(fit, list(c172code = c("low", "high"), c161sex = c("Female", "Male")), backend = "marginaleffects"), table_width = Inf, zap_small = TRUE)) # nolint
  expect_snapshot(print(estimate_means(fit, c("c172code = c('low', 'high')", "c161sex = c('Female', 'Male')"), backend = "marginaleffects"), table_width = Inf, zap_small = TRUE)) # nolint
})


test_that("estimate_epectation - don't print empty RE columns", {
  skip_if_not_installed("glmmTMB")
  data(Salamanders, package = "glmmTMB")
  m <- glmmTMB::glmmTMB(
    count ~ spp + mined + (1 | site),
    ziformula = ~ spp + mined,
    family = poisson(),
    data = Salamanders
  )
  expect_snapshot(print(estimate_expectation(m, by = "spp", predict = "conditional"), zap_small = TRUE))
})


test_that("print - layouts and include data grid", {
  data(iris)
  model <- lm(Petal.Length ~ Species, data = iris)
  out <- estimate_means(model, "Species")
  expect_snapshot(print(out))
  expect_snapshot(print(out, select = "minimal"))
  out <- estimate_contrasts(model, "Species")
  expect_snapshot(print(out, select = "minimal"))
  expect_snapshot(print(out, select = "{estimate}{stars}|{ci}"))

  m <- lm(wt ~ qsec + mpg, dat = mtcars)
  expect_snapshot(print(estimate_relation(m, by = "qsec")))
  expect_snapshot(print(estimate_relation(m, by = "qsec"), include_grid = TRUE))
})
