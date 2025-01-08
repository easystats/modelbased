test_that("get_marginaltrends", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("emmeans")

  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

  # TODO: fix this
  e1 <- get_emtrends(model, by=NULL, trend = "Petal.Length") |> as.data.frame()
  e2 <- get_marginaltrends(model, by=NULL, trend = "Petal.Length") |> as.data.frame()
  # expect_equal(e1$Petal.Length.trend, e2$estimate)

  e1 <- get_emtrends(model, by="Species", trend = "Petal.Length") |> as.data.frame()
  e2 <- get_marginaltrends(model, by="Species", trend = "Petal.Length") |> as.data.frame()
  expect_equal(e1$Petal.Length.trend, e2$estimate)

  e1 <- get_emtrends(model, by="Petal.Length", trend = "Petal.Length") |> as.data.frame()
  e2 <- get_marginaltrends(model, by="Petal.Length", trend = "Petal.Length") |> as.data.frame()
  expect_equal(e1$Petal.Length.trend, e2$estimate)
})
