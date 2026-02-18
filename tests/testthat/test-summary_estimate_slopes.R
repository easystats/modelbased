skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
skip_if_not_installed("mgcv")
skip_on_cran()

test_that("summary.estimate_slopes", {
  set.seed(333)
  model <- mgcv::gam(Sepal.Width ~ s(Petal.Length, by = Species), data = iris)
  slopes <- estimate_slopes(model,
    trend = "Petal.Length",
    by = c("Petal.Length", "Species"), length = 20
  )
  expect_snapshot(summary(slopes))

  set.seed(333)
  model <- mgcv::gam(Sepal.Width ~ s(Petal.Length, by = Species), data = iris)
  slopes <- estimate_slopes(model,
    trend = "Petal.Length",
    by = c("Petal.Length", "Species"), length = 100
  )
  expect_snapshot(summary(slopes))
})
