skip_on_os("windows")

test_that("estimate_means - brms", {
  skip_if_not_installed("brms")
  skip_if_not_installed("emmeans")
  model <- brms::brm(Sepal.Length ~ Species * Sepal.Width, data = iris, refresh = 0, iter = 1000)
  estim <- estimate_means(model)
  expect_identical(dim(estim), c(3L, 5L))
})

test_that("estimate_relation - brms", {
  skip_if_not_installed("brms")
  skip_if_not_installed("emmeans")
  model <- brms::brm(Sepal.Length ~ Species * Sepal.Width, data = iris, refresh = 0, iter = 1000)
  estim <- estimate_relation(model, preserve_range = FALSE)
  expect_identical(dim(estim), c(30L, 6L))

  # estim <- estimate_relation(model, preserve_range=FALSE, iterations = 10)
  # expect_equal(dim(estim), c(30, 6))
})

test_that("estimate_means - brms", {
  skip_if_not_installed("brms")
  skip_if_not_installed("emmeans")
  model <- brms::brm(Sepal.Length ~ Species * Sepal.Width, data = iris, refresh = 0, iter = 1000)
  estim <- estimate_slopes(model, by = "Species")
  expect_identical(dim(estim), c(3L, 5L))
})
