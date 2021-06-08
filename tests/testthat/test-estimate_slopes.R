if (require("testthat") && require("modelbased") && require("rstanarm") && require("insight")) {
  test_that("estimate_slopes", {
    model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

    estim <- estimate_slopes(model)
    expect_equal(dim(estim), c(3, 9))

    estim <- estimate_slopes(model, levels = "Species")
    expect_equal(dim(estim), c(3, 9))

    # estim <- estimate_slopes(model, levels = "Petal.Length", modulate = "Petal.Length")
    # expect_equal(dim(estim), c(3, 9))

    model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)

    estim <- estimate_slopes(model, modulate = "Sepal.Width")
    expect_equal(dim(estim), c(10, 9))
    estim <- estimate_slopes(model, modulate = "Sepal.Width", length = 5)
    expect_equal(dim(estim), c(5, 9))
    estim <- estimate_slopes(model, modulate = "Sepal.Width = c(1, 2, 3)")
    expect_equal(dim(estim), c(3, 9))
  })
}
