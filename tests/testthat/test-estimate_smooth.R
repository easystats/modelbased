if (require("testthat") && require("modelbased") && require("rstanarm")) {
  test_that("estimate_smooth", {
    testthat::skip_on_cran()
    set.seed(333)

    model <- suppressWarnings(rstanarm::stan_gamm4(Sepal.Width ~ s(Petal.Length), data = iris, refresh = 0, iter = 200, chains = 2, seed = 333))
    estim <- estimate_smooth(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(8, 6))

    model <- suppressWarnings(rstanarm::stan_glm(Sepal.Width ~ poly(Petal.Length, 2), data = iris, refresh = 0, iter = 200, chains = 2, seed = 333))
    estim <- estimate_smooth(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(2, 6))

    model <- suppressWarnings(rstanarm::stan_glm(Sepal.Width ~ Species * poly(Petal.Length, 2), data = iris, refresh = 0, iter = 200, chains = 2, seed = 333))
    estim <- estimate_smooth(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(6, 6))
    estim <- estimate_smooth(model, levels = "Species")
    testthat::expect_equal(ncol(estim), 7)
  })
}
