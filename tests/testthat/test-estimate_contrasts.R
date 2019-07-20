context("estimate_contrasts")



test_that("estimate_contrasts", {
  library(insight)
  library(rstanarm)

  estim <- estimate_contrasts(insight::download_model("stanreg_lm_6"))
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 8))

  testthat::expect_error(estimate_contrasts(insight::download_model("stanreg_lm_4")))

  data <- iris
  data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")

  model <- stan_glm(Sepal.Width ~ Species * Petal.Length_factor, data = data, refresh = 0, iter = 200, chains = 2)
  estim <- estimate_contrasts(model)
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(15, 8))
  estim <- estimate_contrasts(model, fixed = "Petal.Length_factor")
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(6, 9))

  model <- stan_glm(Sepal.Width ~ Species * Petal.Width, data = iris, refresh = 0, iter = 200, chains = 2)
  estim <- estimate_contrasts(model)
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 8))
  estim <- estimate_contrasts(model, fixed = "Petal.Width")
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 9))
  estim <- estimate_contrasts(model, modulate = "Petal.Width", length = 4)
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(12, 9))
})
