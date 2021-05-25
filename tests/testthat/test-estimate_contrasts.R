if (require("testthat") && require("modelbased") && require("logspline") && require("rstanarm") && require("insight") && require("lme4")) {

  test_that("estimate_contrasts - Frequentist", {

    # One factor
    model <- lm(Sepal.Width ~ Species, data = iris)

    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(3, 9))
    estim <- estimate_contrasts(model, levels = "Species=c('versicolor', 'virginica')")
    expect_equal(dim(estim), c(1, 9))

    # Two factors
    data <- iris
    data$fac <- ifelse(data$Sepal.Length < 5.8, "A", "B")

    model <- lm(Sepal.Width ~ Species * fac, data = data)

    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(15, 9))
    estim <- estimate_contrasts(model, levels = "Species")
    expect_equal(dim(estim), c(3, 9))
    estim <- estimate_contrasts(model, fixed = "fac")
    expect_equal(dim(estim), c(3, 10))

    # One factor and one continuous
    model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(3, 9))
    estim <- estimate_contrasts(model, fixed = "Petal.Width")
    expect_equal(dim(estim), c(3, 10))
    estim <- estimate_contrasts(model, modulate = "Petal.Width", length = 4)
    expect_equal(dim(estim), c(12, 10))


    # Contrast between continuous
    model <- lm(Sepal.Width ~ Petal.Length, data = iris)

    estim <- estimate_contrasts(model, levels = "Petal.Length=c(2.3, 3)")
    expect_equal(dim(estim), c(1, 9))
    estim <- estimate_contrasts(model, levels = "Petal.Length=c(2, 3, 4)")
    expect_equal(dim(estim), c(3, 9))


    # Three factors
    data <- mtcars
    data[c("gear", "vs", "am")] <- sapply(data[c("gear", "vs", "am")], as.factor)
    model <- lm(mpg ~ gear * vs * am, data = data)

    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(66, 9))
    estim <- estimate_contrasts(model, fixed = "gear")
    expect_equal(dim(estim), c(6, 10))
    estim <- estimate_contrasts(model, fixed = "gear='5'")
    expect_equal(dim(estim), c(6, 10))


    data <- iris
    data$factor1 <- ifelse(data$Sepal.Width > 3, "A", "B")
    data$factor2 <- ifelse(data$Petal.Length > 3.5, "C", "D")
    data$factor3 <- ifelse(data$Sepal.Length > 5, "E", "F")

    model <- lm(Petal.Width ~ factor1 * factor2 * factor3, data = data)

    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(28, 9))
    estim <- estimate_contrasts(model, fixed = "factor3")
    expect_equal(dim(estim), c(6, 10))
    estim <- estimate_contrasts(model, fixed = "factor3='F'")
    expect_equal(dim(estim), c(6, 10))
    estim <- estimate_contrasts(model, modulate = "factor3")
    expect_equal(dim(estim), c(12, 10))


    # Mixed models
    data <- iris
    data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")

    model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)
    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(3, 9))


    # GLM - binomial
    df <- iris
    df$y <- as.factor(ifelse(df$Sepal.Width > 3, "A", "B"))
    model <- glm(y ~ Species, family = "binomial", data = df)

    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(3, 9))
    estim <- estimate_contrasts(model, transform = "response")
    expect_equal(dim(estim), c(3, 9))

    # GLM - poisson
    data <- data.frame(
      counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12),
      treatment = gl(3, 3)
    )
    model <- glm(counts ~ treatment, data = data, family = poisson())

    estim <- estimate_contrasts(model, transform = "response")
    expect_equal(dim(estim), c(3, 9))
  })


  test_that("estimate_contrasts - Bayesian", {


    model <- suppressWarnings(rstanarm::stan_glm(mpg ~ wt + poly(cyl, 2), data = mtcars, iter = 200, refresh = 0))
    expect_error(estimate_contrasts(model))

    data <- iris
    data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")

    model <- suppressWarnings(rstanarm::stan_glm(Sepal.Width ~ Species * Petal.Length_factor, data = data, refresh = 0, iter = 200, chains = 2))
    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(15, 7))
    estim <- estimate_contrasts(model, fixed = "Petal.Length_factor")
    expect_equal(dim(estim), c(3, 8))

    model <- suppressWarnings(rstanarm::stan_glm(Sepal.Width ~ Species * Petal.Width, data = iris, refresh = 0, iter = 200, chains = 2))
    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(3, 7))
    estim <- estimate_contrasts(model, fixed = "Petal.Width")
    expect_equal(dim(estim), c(3, 8))
    estim <- estimate_contrasts(model, modulate = "Petal.Width", length = 4)
    expect_equal(dim(estim), c(12, 8))

    # GLM
    df <- iris
    df$y <- as.numeric(as.factor(ifelse(df$Sepal.Width > 3, "A", "B"))) - 1
    model <- suppressWarnings(rstanarm::stan_glm(y ~ Species,
      family = "binomial", data = df, refresh = 0,
      prior = rstanarm::normal(scale = 0.5)
    ))

    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(3, 7))
    estim <- estimate_contrasts(model, transform = "response")
    expect_equal(dim(estim), c(3, 7))

    estim <- estimate_contrasts(model, test = "bf")
    expect_equal(dim(estim), c(3, 6))
    estim <- estimate_contrasts(model, transform = "response", test = "bf")
    expect_equal(dim(estim), c(3, 6))

  })




  test_that("estimate_contrasts - p.adjust", {
    model <- lm(Petal.Width ~ Species, data = iris)

    p_none <- modelbased::estimate_contrasts(model, adjust = "none")
    p_tuk <- modelbased::estimate_contrasts(model, adjust = "tukey")

    expect_true(any(as.data.frame(p_none) != as.data.frame(p_tuk)))
  })

  # TODO: Not sure why the below test fails on GH; re-activate next time to try again

  # test_that("estimate_contrasts - dfs", {
  #   data <- iris
  #   data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
  #   model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)
  #
  #   estim1 <- estimate_contrasts(model, lmer.df = "satterthwaite")
  #   estim2 <- estimate_contrasts(model, lmer.df = "kenward-roger")
  #
  #   expect_true(any(as.data.frame(estim1) != as.data.frame(estim2)))
  # })


}
