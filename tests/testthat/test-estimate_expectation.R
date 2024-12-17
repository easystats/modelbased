test_that("estimate_expectation", {
  skip_if_not_installed("lme4")
  model <- lme4::lmer(mpg ~ wt + factor(am) + (1 | cyl), data = mtcars)
  estim <- estimate_expectation(model)
  expect_equal(dim(estim), c(32, 8))
  expect_equal(
    colnames(estim),
    c("wt", "am", "cyl", "Predicted", "SE", "CI_low", "CI_high", "Residuals")
  )

  m <- lm(mpg ~ 1, data = mtcars)
  estim <- estimate_expectation(m)
  expect_equal(dim(estim), c(32, 5))
  expect_equal(
    colnames(estim),
    c("Predicted", "SE", "CI_low", "CI_high", "Residuals")
  )

  m <- lm(mpg ~ am, data = mtcars)
  estim <- estimate_expectation(m)
  expect_equal(dim(estim), c(32, 6))
  expect_equal(
    colnames(estim),
    c("am", "Predicted", "SE", "CI_low", "CI_high", "Residuals")
  )
})


test_that("estimate_expectation - data-grid", {
  skip_if_not_installed("lme4")
  model <- lme4::lmer(mpg ~ wt + factor(am) + (1 | cyl), data = mtcars)
  estim <- estimate_expectation(model, data = "grid")
  expect_equal(dim(estim), c(12, 7))
  expect_equal(
    colnames(estim),
    c("wt", "am", "cyl", "Predicted", "SE", "CI_low", "CI_high")
  )

  m <- lm(mpg ~ 1, data = mtcars)
  estim <- estimate_expectation(m, data = "grid")
  expect_equal(dim(estim), c(10, 5))
  expect_equal(
    colnames(estim),
    c("Predicted", "SE", "CI_low", "CI_high", "Residuals")
  )

  m <- lm(mpg ~ cyl, data = mtcars)
  estim <- estimate_expectation(m, data = "grid")
  expect_equal(dim(estim), c(10, 5))
  expect_equal(
    colnames(estim),
    c("cyl", "Predicted", "SE", "CI_low", "CI_high")
  )
  estim2 <- estimate_expectation(m, by = "cyl")
  expect_equal(estim$cyl, estim2$cyl, tolerance = 1e-4)
  expect_equal(estim$Predicted, estim2$Predicted, tolerance = 1e-4)

  m <- lm(mpg ~ factor(cyl), data = mtcars)
  estim <- estimate_expectation(m, data = "grid")
  expect_equal(dim(estim), c(3, 5))
  expect_equal(
    colnames(estim),
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
