if (require("testthat") &&
  require("modelbased") &&
  require("rstanarm") &&
  require("insight") &&
  require("emmeans")) {
  test_that("estimate_slopes", {
    model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

    estim <- estimate_slopes(model)
    expect_equal(dim(estim), c(1, 8))

    estim <- estimate_slopes(model, at = "Species")
    expect_equal(dim(estim), c(3, 9))

    estim <- estimate_slopes(model, at = "Petal.Length")
    expect_equal(dim(estim), c(10, 9))

    estim <- estimate_slopes(model, at = c("Species", "Petal.Length"))
    expect_equal(dim(estim), c(30, 10))

    model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)

    estim <- estimate_slopes(model, at = "Sepal.Width")
    expect_equal(dim(estim), c(10, 9))
    estim <- estimate_slopes(model, at = "Sepal.Width", length = 5)
    expect_equal(dim(estim), c(5, 9))
    estim <- estimate_slopes(model, at = "Sepal.Width = c(1, 2, 3)")
    expect_equal(dim(estim), c(3, 9))
  })
}
