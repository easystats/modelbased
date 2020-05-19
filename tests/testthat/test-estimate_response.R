if (require("testthat") && require("modelbased") && require("rstanarm") && require("insight")) {

  test_that("estimate_response - Bayesian", {
    estim <- estimate_response(insight::download_model("stanreg_lm_5"), seed = 333)
    testthat::expect_equal(nrow(estim), nrow(mtcars))

    estim <- estimate_response(insight::download_model("stanreg_lm_6"), data = "grid", seed = 333)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(43, 5))

    estim <- estimate_response(insight::download_model("stanreg_lm_7"), seed = 333)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(32, 5))

    model <- stan_glm(Sepal.Width ~ Petal.Width, data = iris, refresh = 0, iter = 500, chains = 2)
    estim <- estimate_link(model, keep_draws = TRUE)
    draws <- reshape_draws(estim)
    testthat::expect_equal(c(nrow(draws), ncol(draws)), c(12500, 7))

    # Polr
    model <- stan_polr(Species ~ Petal.Width + Petal.Length, data = iris, refresh = 0, iter = 500, chains = 2, prior = R2(0.2, "mean"))
    estim <- estimate_link(model, length = 5)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(25, 5))

    # Non-sampling algorithms
    library(rstanarm)
    model <- stan_glm(mpg ~ drat, data = mtcars, algorithm="meanfield")
    estim <- estimate_link(model, keep_draws = TRUE)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(25, 1004))
    library(brms)
    model <- brms::brm(mpg ~ drat, data = mtcars, algorithm="meanfield")
    estim <- estimate_link(model, keep_draws = TRUE)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(25, 1004))
  })



  test_that("estimate_response - Frequentist", {
    estim <- estimate_response(insight::download_model("lm_2"))
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(32, 5))

    estim <- estimate_link(insight::download_model("glm_2"), target = "wt")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(25, 5))

    estim <- estimate_link(insight::download_model("lmerMod_1"))
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(25, 4))

    estim <- estimate_link(insight::download_model("merMod_1"))
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(25, 4))

    model <- MASS::polr(Species ~ Sepal.Width, data = iris)
    estim <- estimate_link(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(25, 4))
  })
}