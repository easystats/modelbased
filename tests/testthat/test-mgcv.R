test_that("estimate_means - mgcv gam", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("gamm4")
  skip_if_not_installed("emmeans")
  model <- mgcv::gam(Sepal.Length ~ Species + s(Sepal.Width, by = Species), data = iris)

  estim <- suppressMessages(estimate_means(model))
  expect_equal(dim(estim), c(3, 5))
})

test_that("estimate_contrasts - mgcv gam", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("gamm4")
  skip_if_not_installed("emmeans")
  model <- mgcv::gam(Sepal.Length ~ Species + s(Sepal.Width, by = Species), data = iris)

  estim <- suppressMessages(estimate_contrasts(model))
  expect_equal(dim(estim), c(3, 9))
})

test_that("estimate_expectation - mgcv gam", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("gamm4")
  skip_if_not_installed("emmeans")
  model <- mgcv::gam(Sepal.Length ~ Species + s(Sepal.Width, by = Species), data = iris)

  estim <- suppressMessages(estimate_expectation(model))
  expect_equal(dim(estim), c(150, 7))
})

test_that("estimate_link - mgcv gam", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("gamm4")
  skip_if_not_installed("emmeans")
  model <- mgcv::gam(Sepal.Length ~ Species + s(Sepal.Width, by = Species), data = iris)

  estim <- suppressMessages(estimate_link(model, preserve_range = FALSE))
  expect_equal(dim(estim), c(30, 6))
})

# model <- mgcv::gam(Sepal.Length ~ Petal.Length + s(Sepal.Width) + s(Species, bs = "fs"), data = iris)
# estim <- estimate_link(model)

test_that("estimate_expectation - mgcv gamm", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("gamm4")
  skip_if_not_installed("emmeans")
  model <- mgcv::gamm(Sepal.Length ~ Petal.Length + s(Sepal.Width), random = list(Species = ~1), data = iris)

  estim <- suppressMessages(estimate_expectation(model))
  expect_equal(dim(estim), c(150, 8))
})

test_that("estimate_link - mgcv gamm", {
  skip_on_os("mac")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("gamm4")
  skip_if_not_installed("emmeans")

  model <- mgcv::gamm(Sepal.Length ~ Petal.Length + s(Sepal.Width), random = list(Species = ~1), data = iris)

  estim <- estimate_link(model, length = 4, verbose = FALSE)
  expect_identical(dim(estim), as.integer(c(16, 6)))
})


# Gamm4 -------------------------------------------------------------------

# model <- gamm4::gamm4(Sepal.Length ~ Petal.Length + s(Sepal.Width), random=~(1|Species), data = iris)
#
# test_that("estimate_expectation - gamm4", {
#   estim <- estimate_expectation(model)
#   expect_equal(dim(estim), c(150, 5))
# })
#
# test_that("estimate_link - gamm4", {
#   estim <- estimate_link(model, length=4)
#   expect_equal(dim(estim), c(16, 5))
# })
