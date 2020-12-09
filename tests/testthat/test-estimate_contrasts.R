if (require("testthat") && require("modelbased") && require("rstanarm") && require("insight") && require("lme4")) {
  test_that("estimate_contrasts", {


    # Bayesian ----------------------------------------------------------------


    if (require("rstanarm")) {
      testthat::expect_error(estimate_contrasts(rstanarm::stan_glm(mpg ~ wt + poly(cyl, 2), data = mtcars, iter = 200, refresh = 0)))

      data <- iris
      data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")

      model <- rstanarm::stan_glm(Sepal.Width ~ Species * Petal.Length_factor, data = data, refresh = 0, iter = 200, chains = 2)
      estim <- estimate_contrasts(model)
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(15, 8))
      estim <- estimate_contrasts(model, fixed = "Petal.Length_factor")
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 9))

      model <- rstanarm::stan_glm(Sepal.Width ~ Species * Petal.Width, data = iris, refresh = 0, iter = 200, chains = 2)
      estim <- estimate_contrasts(model)
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 8))
      estim <- estimate_contrasts(model, fixed = "Petal.Width")
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 9))
      estim <- estimate_contrasts(model, modulate = "Petal.Width", length = 4)
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(12, 9))

      # GLM
      df <- iris
      df$y <- as.numeric(as.factor(ifelse(df$Sepal.Width > 3, "A", "B"))) - 1
      model <- rstanarm::stan_glm(y ~ Species,
        family = "binomial", data = df, refresh = 0,
        prior = rstanarm::normal(scale = 0.5)
      )

      estim <- estimate_contrasts(model)
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 8))
      estim <- estimate_contrasts(model, transform = "response")
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 7))

      library(logspline)
      estim <- estimate_contrasts(model, test = "bf")
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 7))
      estim <- estimate_contrasts(model, transform = "response", test = "bf")
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 6))
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

    estim <- estimate_contrasts(model, levels = "Petal.Length=c(2.3, 3)")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(1, 10))
    estim <- estimate_contrasts(model, levels = "Petal.Length=c(2, 3, 4)")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 10))


    # Three factors
    data <- mtcars
    data[c("gear", "vs", "am")] <- sapply(data[c("gear", "vs", "am")], as.factor)
    model <- lm(mpg ~ gear * vs * am, data = data)

    estim <- estimate_contrasts(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(66, 10))
    estim <- estimate_contrasts(model, fixed = "gear")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(6, 11))
    estim <- estimate_contrasts(model, fixed = "gear='5'")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(6, 11))


    data <- iris
    data$factor1 <- ifelse(data$Sepal.Width > 3, "A", "B")
    data$factor2 <- ifelse(data$Petal.Length > 3.5, "C", "D")
    data$factor3 <- ifelse(data$Sepal.Length > 5, "E", "F")

    model <- lm(Petal.Width ~ factor1 * factor2 * factor3, data = data)

    estim <- estimate_contrasts(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(28, 10))
    estim <- estimate_contrasts(model, fixed = "factor3")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(6, 11))
    estim <- estimate_contrasts(model, fixed = "factor3='F'")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(6, 11))
    estim <- estimate_contrasts(model, modulate = "factor3")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(12, 11))


    # Mixed models
    if (require("lme4")) {
      data <- iris
      data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")

      model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)
      estim <- estimate_contrasts(model)
      testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 10))
    }


    # GLM - binomial
    df <- iris
    df$y <- as.factor(ifelse(df$Sepal.Width > 3, "A", "B"))
    model <- glm(y ~ Species, family = "binomial", data = df)

    estim <- estimate_contrasts(model)
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 10))
    estim <- estimate_contrasts(model, transform = "response")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 9))

    # GLM - poisson
    data <- data.frame(
      counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12),
      treatment = gl(3, 3)
    )
    model <- glm(counts ~ treatment, data = data, family = poisson())

    estim <- estimate_contrasts(model, transform = "response")
    testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 9))
  })
}
