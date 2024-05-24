test_that("estimate_slopes", {
  skip_if_not_installed("emmeans")

  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

  estim <- suppressMessages(estimate_slopes(model))
  expect_equal(dim(estim), c(1, 8))

  estim <- suppressMessages(estimate_slopes(model, by = "Species"))
  expect_equal(dim(estim), c(3, 9))

  estim <- suppressMessages(estimate_slopes(model, by = "Petal.Length"))
  expect_equal(dim(estim), c(10, 9))

  estim <- suppressMessages(estimate_slopes(model, by = c("Species", "Petal.Length")))
  expect_equal(dim(estim), c(30, 10))

  model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)

  estim <- suppressMessages(estimate_slopes(model, by = "Sepal.Width"))
  expect_equal(dim(estim), c(10, 9))
  estim <- suppressMessages(estimate_slopes(model, by = "Sepal.Width", length = 5))
  expect_equal(dim(estim), c(5, 9))
  estim <- suppressMessages(estimate_slopes(model, by = "Sepal.Width = c(1, 2, 3)"))
  expect_equal(dim(estim), c(3, 9))
})
