if (require("testthat") && require("modelbased") && require("rstanarm") && require("insight") && require("lme4")) {
  test_that("estimate_contrasts", {
    if (require("rstanarm")) {
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



    # Frequentist ------------------------------------------------------------

    # One factor
    model <- lm(Sepal.Width ~ Species, data = iris)

    estim <- estimate_contrasts(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 10))
    estim <- estimate_contrasts(model, levels = "Species=c('versicolor', 'virginica')")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(1, 10))

    # Two factors
    data <- iris
    data$fac <- ifelse(data$Sepal.Length < 5.8, "A", "B")

    model <- lm(Sepal.Width ~ Species * fac, data = data)

    estim <- estimate_contrasts(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(15, 10))
    estim <- estimate_contrasts(model, levels = "Species")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 10))
    estim <- estimate_contrasts(model, fixed = "fac")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 11))

    # One factor and one continuous
    model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
    estim <- estimate_contrasts(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 10))
    estim <- estimate_contrasts(model, fixed = "Petal.Width")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 11))
    estim <- estimate_contrasts(model, modulate = "Petal.Width", length = 4)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(12, 11))


    # Contrast between continuous
    model <- lm(Sepal.Width ~ Petal.Length, data = iris)

    estim <- estimate_contrasts(model, levels = "Petal.Width=c(2.3, 3)")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(1, 10))
    estim <- estimate_contrasts(model, levels = "Petal.Width=c(2, 3, 4)")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 10))

    # Mixed models
    if (require("lme4")) {
      data <- iris
      data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")

      model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)
      estim <- estimate_contrasts(model)
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 10))
    }
  })
}
