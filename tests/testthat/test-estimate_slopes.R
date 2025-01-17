skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")

test_that("estimate_slopes", {
  data(iris)
  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

  expect_message(estimate_slopes(model, backend = "emmeans"), regex = "No numeric")
  estim1 <- suppressMessages(estimate_slopes(model, backend = "emmeans"))
  estim2 <- suppressMessages(estimate_slopes(model, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(1L, 8L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 1e-4)

  estim1 <- suppressMessages(estimate_slopes(model, by = "Species", backend = "emmeans"))
  estim2 <- suppressMessages(estimate_slopes(model, by = "Species", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(3L, 9L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 1e-4)

  estim1 <- suppressMessages(estimate_slopes(model, by = "Petal.Length", backend = "emmeans"))
  estim2 <- suppressMessages(estimate_slopes(model, by = "Petal.Length", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(10L, 9L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 0.2)

  estim1 <- suppressMessages(estimate_slopes(model, by = c("Species", "Petal.Length"), backend = "emmeans"))
  expect_identical(dim(estim1), c(30L, 10L))

  estim2 <- suppressMessages(estimate_slopes(model, by = c("Species", "Petal.Length"), preserve_range = FALSE, backend = "marginaleffects"))
  expect_identical(dim(estim2), c(30L, 10L))
  expect_equal(estim1$Slope, estim2$Slope[order(estim2$Petal.Length, estim2$Species)], tolerance = 1e-3)

  model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)

  estim1 <- suppressMessages(estimate_slopes(model, by = "Sepal.Width", backend = "emmeans"))
  estim2 <- suppressMessages(estimate_slopes(model, by = "Sepal.Width", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(10L, 9L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 0.2)
  estim1 <- suppressMessages(estimate_slopes(model, by = "Sepal.Width", length = 5, backend = "emmeans"))
  estim2 <- suppressMessages(estimate_slopes(model, by = "Sepal.Width", length = 5, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(5L, 9L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 0.2)
  estim1 <- suppressMessages(estimate_slopes(model, by = "Sepal.Width = c(1, 2, 3)", backend = "emmeans"))
  estim2 <- suppressMessages(estimate_slopes(model, by = "Sepal.Width = c(1, 2, 3)", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(3L, 9L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 0.2)
})
