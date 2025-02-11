test_that("estimate_expectation", {
  skip_if_not_installed("lme4")
  model <- lme4::lmer(mpg ~ wt + factor(am) + (1 | cyl), data = mtcars)
  estim <- estimate_expectation(model)
  expect_identical(dim(estim), c(32L, 8L))
  expect_named(
    estim,
    c("wt", "am", "cyl", "Predicted", "SE", "CI_low", "CI_high", "Residuals")
  )

  m <- lm(mpg ~ 1, data = mtcars)
  estim <- estimate_expectation(m)
  expect_identical(dim(estim), c(32L, 5L))
  expect_named(
    estim,
    c("Predicted", "SE", "CI_low", "CI_high", "Residuals")
  )

  m <- lm(mpg ~ am, data = mtcars)
  estim <- estimate_expectation(m)
  expect_identical(dim(estim), c(32L, 6L))
  expect_named(
    estim,
    c("am", "Predicted", "SE", "CI_low", "CI_high", "Residuals")
  )
})


test_that("estimate_expectation - data-grid", {
  skip_if_not_installed("lme4")
  model <- lme4::lmer(mpg ~ wt + factor(am) + (1 | cyl), data = mtcars)
  estim <- estimate_expectation(model, data = "grid")
  expect_identical(dim(estim), c(12L, 7L))
  expect_named(
    estim,
    c("wt", "am", "cyl", "Predicted", "SE", "CI_low", "CI_high")
  )

  m <- lm(mpg ~ 1, data = mtcars)
  estim <- estimate_expectation(m, data = "grid")
  expect_identical(dim(estim), c(10L, 5L))
  expect_named(
    estim,
    c("Predicted", "SE", "CI_low", "CI_high", "Residuals")
  )

  m <- lm(mpg ~ cyl, data = mtcars)
  estim <- estimate_expectation(m, data = "grid")
  expect_identical(dim(estim), c(3L, 5L))
  expect_named(
    estim,
    c("cyl", "Predicted", "SE", "CI_low", "CI_high")
  )
  estim2 <- estimate_expectation(m, by = "cyl")
  expect_equal(estim$cyl, estim2$cyl, tolerance = 1e-4)
  expect_equal(estim$Predicted, estim2$Predicted, tolerance = 1e-4)

  m <- lm(mpg ~ factor(cyl), data = mtcars)
  estim <- estimate_expectation(m, data = "grid")
  expect_identical(dim(estim), c(3L, 5L))
  expect_named(
    estim,
    c("cyl", "Predicted", "SE", "CI_low", "CI_high")
  )
})


test_that("estimate_expectation - error", {
  m <- lm(mpg ~ cyl, data = mtcars)
  expect_error(
    estimate_expectation(m, data = mtcars, by = "cyl"),
    regex = "You can only"
  )
})


test_that("estimate_relation and estimate specific", {
  skip_if_not_installed("marginaleffects")
  data(efc, package = "modelbased")
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
  fit <- lm(neg_c_7 ~ c12hour + barthtot + c161sex + e42dep + c172code, data = efc)
  out1 <- estimate_means(fit, "e42dep", estimate = "specific", backend = "marginaleffects")
  out2 <- estimate_relation(fit, by = "e42dep")
  expect_equal(out1$Mean, out2$Predicted, tolerance = 1e-4)
})
