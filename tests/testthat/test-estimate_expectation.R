if (require("testthat") && require("modelbased") && require("lme4") && require("insight")) {
  test_that("estimate_expectation", {
    model <- lmer(mpg ~ wt + factor(am) + (1 | cyl), data = mtcars)
    estim <- estimate_expectation(model)
    expect_equal(dim(estim), c(32, 9))
    expect_equal(
      colnames(estim),
      c("mpg", "wt", "am", "cyl", "Predicted", "SE", "CI_low", "CI_high", "Residuals")
    )

    m <- lm(mpg ~ 1, data = mtcars)
    estim <- estimate_expectation(m)
    expect_equal(dim(estim), c(32, 6))
    expect_equal(
      colnames(estim),
      c("mpg", "Predicted", "SE", "CI_low", "CI_high", "Residuals")
    )

    m <- lm(mpg ~ am, data = mtcars)
    estim <- estimate_expectation(m)
    expect_equal(dim(estim), c(32, 7))
    expect_equal(
      colnames(estim),
      c("mpg", "am", "Predicted", "SE", "CI_low", "CI_high", "Residuals")
    )
  })


  test_that("estimate_expectation - data-grid", {
    model <- lmer(mpg ~ wt + factor(am) + (1 | cyl), data = mtcars)
    estim <- estimate_expectation(model, data = "grid")
    expect_equal(dim(estim), c(12, 7))
    expect_equal(
      colnames(estim),
      c("wt", "am", "cyl", "Predicted", "SE", "CI_low", "CI_high")
    )

    m <- lm(mpg ~ 1, data = mtcars)
    estim <- estimate_expectation(m, data = "grid")
    expect_equal(dim(estim), c(10, 6))
    expect_equal(
      colnames(estim),
      c("mpg", "Predicted", "SE", "CI_low", "CI_high", "Residuals")
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
}
