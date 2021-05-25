if (require("testthat") && require("modelbased") && require("rstanarm") && require("insight")) {
  test_that("estimate_slopes", {
    model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
    estim <- estimate_slopes(model)
    expect_equal(dim(estim), c(3, 9))
  })
}
