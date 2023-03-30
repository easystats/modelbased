test_that("get_marginaleffects", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("lme4")

  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

  expect_equal(nrow(get_marginaleffects(model, trend = "Petal.Length", at = "Species")), 3L)

  # get_marginaleffects(model, trend = "Petal.Length", at = "Species", length = 10)
})