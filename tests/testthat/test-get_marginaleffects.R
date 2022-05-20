if (require("testthat") && require("modelbased") && require("marginaleffects") && require("lme4")) {
  test_that("get_marginaleffects", {
    model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

    expect_equal(dim(get_marginaleffects(model, trend = "Petal.Length", at = "Species")), c(3, 11))

    # get_marginaleffects(model, trend = "Petal.Length", at = "Species", length = 10)
  })
}
