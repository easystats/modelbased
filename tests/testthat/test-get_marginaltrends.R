skip_on_cran()
skip_if_not_installed("marginaleffects")
skip_if_not_installed("emmeans")

test_that("get_marginaltrends", {
  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  out <- get_marginaltrends(model, trend = "Petal.Length", by = "Species")
  expect_identical(nrow(out), 3L)

  out2 <- estimate_slopes(model, trend = "Petal.Length", by = "Species")
  expect_equal(out$estimate, out2$Slope, tolerance = 1e-3)

  e1 <- as.data.frame(get_emtrends(model, by = NULL, trend = "Petal.Length"))
  e2 <- as.data.frame(get_marginaltrends(model, by = NULL, trend = "Petal.Length"))
  expect_equal(e1$Petal.Length.trend, e2$estimate, tolerance = 1e-4)

  e1 <- as.data.frame(get_emtrends(model, by = "Species", trend = "Petal.Length"))
  e2 <- as.data.frame(get_marginaltrends(model, by = "Species", trend = "Petal.Length"))
  expect_equal(e1$Petal.Length.trend, e2$estimate, tolerance = 1e-4)

  ## TODO: find out why these two slightly differ
  e1 <- as.data.frame(get_emtrends(model, by = "Petal.Length", trend = "Petal.Length"))
  e2 <- as.data.frame(get_marginaltrends(model, by = "Petal.Length", trend = "Petal.Length"))
  expect_equal(e1$Petal.Length.trend, e2$estimate, tolerance = 0.2)
})

test_that("get_marginaltrends, warnings", {
  data(iris)
  model <- lm(Sepal.Width ~ Petal.Width * Petal.Length, data = iris)
  expect_message(
    get_marginaltrends(model, trend = c("Petal.Width", "Petal.Length")),
    regex = "More than one numeric variable"
  )
})
