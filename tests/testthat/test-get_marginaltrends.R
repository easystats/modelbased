skip_if_not_installed("marginaleffects")
skip_if_not_installed("emmeans")

test_that("get_marginaltrends", {
  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  out <- get_marginaltrends(model, trend = "Petal.Length", by = "Species")
  expect_identical(nrow(out), 3L)

  out2 <- estimate_slopes(model, trend = "Petal.Length", by = "Species")
  expect_equal(out$estimate, out2$Coefficient, tolerance = 1e-3)
})


test_that("get_marginaltrends", {
  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

  # TODO: fix this
  e1 <- as.data.frame(get_emtrends(model, by=NULL, trend = "Petal.Length"))
  e2 <- as.data.frame(get_marginaltrends(model, by=NULL, trend = "Petal.Length"))
  # expect_equal(e1$Petal.Length.trend, e2$estimate)

  e1 <- as.data.frame(get_emtrends(model, by = "Species", trend = "Petal.Length"))
  e2 <- as.data.frame(get_marginaltrends(model, by = "Species", trend = "Petal.Length"))
  expect_equal(e1$Petal.Length.trend, e2$estimate)

  e1 <- as.data.frame(get_emtrends(model, by = "Petal.Length", trend = "Petal.Length"))
  e2 <- as.data.frame(get_marginaltrends(model, by = "Petal.Length", trend = "Petal.Length"))
  expect_equal(e1$Petal.Length.trend, e2$estimate)
})
