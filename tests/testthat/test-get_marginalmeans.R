if (require("marginaleffects") && require("lme4")) {
  test_that("get_marginaleffects", {
    model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

    estimated <- estimate_means(model, backend = "marginaleffects")
    expect_equal(dim(estimated), c(3, 5))

    # get_marginaleffects(model, trend = "Petal.Length", at = "Species", length = 10)
  })
}
