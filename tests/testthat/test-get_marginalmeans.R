test_that("get_marginaleffects", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("lme4")
  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

  estimated <- suppressMessages(estimate_means(model, backend = "marginaleffects"))

  expect_equal(nrow(estimated), 3)
  expect_true(ncol(estimated) >= 5)

  # get_marginaleffects(model, trend = "Petal.Length", at = "Species", length = 10)
})
