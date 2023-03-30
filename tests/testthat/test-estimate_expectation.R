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

  m <- lm(mpg ~ factor(cyl), data = mtcars)
  estim <- estimate_expectation(m, data = "grid")
  expect_equal(dim(estim), c(3, 5))
  expect_equal(
    colnames(estim),
    c("cyl", "Predicted", "SE", "CI_low", "CI_high")
  )
})

