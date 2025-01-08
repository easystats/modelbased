test_that("get_marginaleffects", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("emmeans")

  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  out <- get_marginaleffects(model, trend = "Petal.Length", by = "Species")
  expect_identical(nrow(out), 3L)

  out2 <- estimate_slopes(model, trend = "Petal.Length", by = "Species")
  expect_equal(out$estimate, out2$Coefficient, tolerance = 1e-3)
})


test_that("get_marginaleffects, overall mean", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("emmeans")

  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  out1 <- as.data.frame(get_emmeans(model, by = NULL))
  out2 <- as.data.frame(get_marginalmeans(model, by = NULL))
  expect_equal(out1$emmean, out2$estimate, tolerance = 0.2)
})
