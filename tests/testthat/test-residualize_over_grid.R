skip_on_os(c("mac", "solaris", "linux"))
skip_if_not_installed("ggplot2")
skip_if_not_installed("see")
skip_if_not_installed("vdiffr")
skip_if_not_installed("marginaleffects")
skip_on_cran()


test_that("residualize_over_grid", {
  data(iris)
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
  pr <- estimate_means(model, c("Sepal.Width", "Species"))
  out <- residualize_over_grid(pr, model)
  expect_identical(dim(out), c(150L, 3L))
  expect_equal(sum(out$Mean), 875.7863, tolerance = 1e-2)

  pr <- estimate_relation(model, by = c("Sepal.Width", "Species"), preserve_range = FALSE)
  out <- residualize_over_grid(pr, model)
  expect_identical(dim(out), c(150L, 3L))
  expect_equal(sum(out$Predicted), 875.7863, tolerance = 1e-2)

  pr <- estimate_relation(model, by = c("Sepal.Width", "Species"))
  expect_error(
    residualize_over_grid(pr, model),
    regex = "Grid for partial"
  )
})


test_that("plots show_resisuals", {
  set.seed(1234)
  x <- rnorm(300, mean = 10)
  z <- rnorm(300)
  v <- rnorm(300)
  y <- (4 * z + 2) * x - 40 * z + 5 * v + rnorm(300, sd = 3)

  d <- data.frame(x, y, z)
  m <- lm(y ~ x + z, data = d)

  est <- estimate_means(m, c("x", "z=[sd]"))
  vdiffr::expect_doppelganger(
    "plot-show_residuals-1",
    plot(est, show_residuals = TRUE)
  )

  est <- estimate_means(m, c("x", "z"))
  vdiffr::expect_doppelganger(
    "plot-show_residuals-2",
    plot(est, show_residuals = TRUE)
  )

  data(iris)
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

  pr <- estimate_means(model, c("Sepal.Width", "Species"))
  vdiffr::expect_doppelganger(
    "plot-show_residuals-3",
    plot(pr, show_residuals = TRUE)
  )
})
