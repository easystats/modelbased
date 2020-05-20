if (require("testthat") && require("modelbased") && require("rstanarm") && require("insight") && require("lme4")) {
  test_that("estimate_contrasts", {

    if(require("rstanarm")){
      testthat::expect_error(estimate_contrasts(insight::download_model("stanreg_lm_4")))

      data <- iris
      data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")

      model <- stan_glm(Sepal.Width ~ Species * Petal.Length_factor, data = data, refresh = 0, iter = 200, chains = 2)
      estim <- estimate_contrasts(model)
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(15, 8))
      # estim <- estimate_contrasts(model, fixed = "Petal.Length_factor")
      # testthat::expect_equal(c(nrow(estim), ncol(estim)), c(6, 9))

      model <- stan_glm(Sepal.Width ~ Species * Petal.Width, data = iris, refresh = 0, iter = 200, chains = 2)
      estim <- estimate_contrasts(model)
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 8))
      estim <- estimate_contrasts(model, fixed = "Petal.Width")
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 9))
      estim <- estimate_contrasts(model, modulate = "Petal.Width", length = 4)
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(12, 9))
    }




    model <- lm(Sepal.Width ~ Species, data = iris)
    estim <- estimate_contrasts(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 10))

    model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
    estim <- estimate_contrasts(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 10))
    estim <- estimate_contrasts(model, fixed = "Petal.Width")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 11))
    estim <- estimate_contrasts(model, modulate = "Petal.Width", length = 4)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(12, 11))

    data <- iris
    data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")

    model <- lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)
    # estim <- estimate_contrasts(model)
    # testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 10))
  })
}