skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")
skip_if_not_installed("mgcv")
skip_if_not_installed("gamm4")


test_that("estimate_means - mgcv gam", {
  model <- mgcv::gam(Sepal.Length ~ Species + s(Sepal.Width, by = Species), data = iris)
  estim <- suppressMessages(estimate_means(model, backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 5L))
  estim2 <- suppressMessages(estimate_means(model, backend = "marginaleffects"))
  expect_identical(dim(estim2), c(3L, 7L))
  expect_named(estim2, c("Species", "Mean", "SE", "CI_low", "CI_high", "t", "df"))
  expect_equal(estim$Mean, estim2$Mean, tolerance = 1e-4)
})


test_that("estimate_contrasts - mgcv gam", {
  model <- mgcv::gam(Sepal.Length ~ Species + s(Sepal.Width, by = Species), data = iris)
  estim <- suppressMessages(estimate_contrasts(model, backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 9L))
  estim2 <- suppressMessages(estimate_contrasts(model, backend = "marginaleffects"))
  expect_identical(dim(estim2), c(3L, 8L))
  expect_named(estim2, c("Species", "Difference", "SE", "CI_low", "CI_high", "t", "df"))
  expect_equal(estim$Difference, estim2$Difference, tolerance = 1e-4)
})


test_that("estimate_expectation - mgcv gam", {
  model <- mgcv::gam(Sepal.Length ~ Species + s(Sepal.Width, by = Species), data = iris)
  estim <- suppressMessages(estimate_expectation(model))
  expect_identical(dim(estim), c(150L, 7L))
})


test_that("estimate_link - mgcv gam", {
  model <- mgcv::gam(Sepal.Length ~ Species + s(Sepal.Width, by = Species), data = iris)
  estim <- suppressMessages(estimate_link(model, preserve_range = FALSE))
  expect_equal(dim(estim), c(30, 6))
})


# model <- mgcv::gam(Sepal.Length ~ Petal.Length + s(Sepal.Width) + s(Species, bs = "fs"), data = iris)
# estim <- estimate_link(model)

test_that("estimate_expectation - mgcv gamm", {
  model <- mgcv::gamm(Sepal.Length ~ Petal.Length + s(Sepal.Width), random = list(Species = ~1), data = iris)
  estim <- suppressMessages(estimate_expectation(model))
  expect_equal(dim(estim), c(150, 8))
})


test_that("estimate_link - mgcv gamm", {
  skip_on_os("mac")
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
