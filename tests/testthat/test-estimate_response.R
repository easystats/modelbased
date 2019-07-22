context("estimate_response")



test_that("estimate_response", {
  library(insight)
  library(rstanarm)

  estim <- estimate_response(insight::download_model("stanreg_lm_5"))
  testthat::expect_equal(nrow(estim), nrow(mtcars))

  estim <- estimate_response(insight::download_model("stanreg_lm_6"), data = "grid")
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(75, 5))

  estim <- estimate_response(insight::download_model("stanreg_lm_7"))
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(32, 5))

  model <- stan_glm(Sepal.Width ~ Petal.Width, data = iris, refresh = 0, iter = 500, chains = 2)
  estim <- estimate_link(model, keep_draws = TRUE)
  draws <- reshape_draws(estim)
  testthat::expect_equal(c(nrow(draws), ncol(draws)), c(12500, 7))
})
