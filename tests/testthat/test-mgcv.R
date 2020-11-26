if (require("testthat") && require("modelbased") && require("mgcv") && require("emmeans")) {

  model <- mgcv::gam(Sepal.Length ~ Species + s(Sepal.Width, by=Species), data = iris)


  test_that("estimate_means - mgcv", {
    estim <- estimate_means(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 5))
  })

  test_that("estimate_contrasts - mgcv", {
    estim <- estimate_contrasts(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 10))
  })

  test_that("estimate_response - mgcv", {
    estim <- estimate_response(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(150, 5))
  })

  test_that("estimate_link - mgcv", {
    estim <- estimate_link(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(52, 5))
  })

  model <- mgcv::gam(Sepal.Length ~ Petal.Length + s(Sepal.Width) + s(Species, bs="fs"), data = iris)

}
