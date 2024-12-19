test_that("get_marginaleffects", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("lme4")

  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  out <- get_marginaleffects(model, trend = "Petal.Length", by = "Species")
  expect_identical(nrow(out), 3L)
})
