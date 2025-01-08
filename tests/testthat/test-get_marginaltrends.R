test_that("get_marginaltrends", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("emmeans")

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
