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
  expect_identical(dim(estim2), c(30L, 8L))
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


test_that("estimate_slopes, johnson-neyman p-adjust", {
  data(iris)
  model <- lm(Sepal.Width ~ Petal.Width * Petal.Length, data = iris)
  out <- estimate_slopes(model, "Petal.Width", by = "Petal.Length")
  expect_equal(
    out$CI_low,
    c(
      -0.83863, -0.66978, -0.50396, -0.34188, -0.18459, -0.03252,
      0.11384, 0.25426, 0.38899, 0.51855
    ),
    tolerance = 1e-2
  )
  expect_equal(
    out$p,
    c(
      0.00664, 0.03956, 0.20019, 0.70529, 0.52501, 0.08553, 0.00496,
      0.00013, 0, 0
    ),
    tolerance = 1e-2
  )

  out <- estimate_slopes(model, "Petal.Width", by = "Petal.Length", p_adjust = "esarey")
  expect_equal(
    out$CI_low,
    c(
      -0.89944, -0.72628, -0.55667, -0.39145, -0.23184, -0.07835,
      0.06843, 0.20825, 0.3414, 0.46848
    ),
    tolerance = 1e-2
  )
  expect_equal(
    out$p,
    c(
      0.03438, 0.14813, 0.50393, 0.83427, 0.24689, 0.03326, 0.00219,
      9e-05, 0, 0
    ),
    tolerance = 1e-2
  )

})
