if (requiet("logspline") && requiet("rstanarm") && requiet("lme4") && requiet("emmeans")) {
  test_that("estimate_contrasts - Frequentist", {
    # One factor
    dat <<- iris
    model <- lm(Sepal.Width ~ Species, data = dat)

    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(3, 9))

    estim <- estimate_contrasts(model, at = "Species=c('versicolor', 'virginica')")
    expect_equal(dim(estim), c(1, 9))

    # Two factors
    dat <- iris
    dat$fac <- ifelse(dat$Sepal.Length < 5.8, "A", "B")
    dat <<- dat

    model <- lm(Sepal.Width ~ Species * fac, data = dat)

    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(3, 9))
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
    estim <- estimate_contrasts(model, at = "Petal.Width", length = 4)
    expect_equal(dim(estim), c(12, 10))


    # Contrast between continuous
    model <- lm(Sepal.Width ~ Petal.Length, data = iris)

    estim <- estimate_contrasts(model, at = "Petal.Length=c(2.3, 3)")
    expect_equal(dim(estim), c(1, 9))
    estim <- estimate_contrasts(model, at = "Petal.Length=c(2, 3, 4)")
    expect_equal(dim(estim), c(3, 9))


    # Three factors
    dat <- mtcars
    dat[c("gear", "vs", "am")] <- sapply(dat[c("gear", "vs", "am")], as.factor)
    dat <<- dat
    model <- lm(mpg ~ gear * vs * am, data = dat)

    estim <- estimate_contrasts(model, at = "all")
    expect_equal(dim(estim), c(12, 11))
    estim <- estimate_contrasts(model, contrast = c("vs", "am"), fixed = "gear")
    expect_equal(dim(estim), c(6, 10))
    estim <- estimate_contrasts(model, contrast = c("vs", "am"), at = "gear='5'")
    expect_equal(dim(estim), c(1, 10))


    dat <- iris
    dat$factor1 <- ifelse(dat$Sepal.Width > 3, "A", "B")
    dat$factor2 <- ifelse(dat$Petal.Length > 3.5, "C", "D")
    dat$factor3 <- ifelse(dat$Sepal.Length > 5, "E", "F")
    dat <<- dat

    model <- lm(Petal.Width ~ factor1 * factor2 * factor3, data = dat)

    estim <- estimate_contrasts(model, contrast = c("factor1", "factor2", "factor3"), at = "all")
    expect_equal(dim(estim), c(28, 9))
    estim <- estimate_contrasts(model, contrast = c("factor1", "factor2"), fixed = "factor3")
    expect_equal(dim(estim), c(6, 10))
    estim <- estimate_contrasts(model, contrast = c("factor1", "factor2"), at = "factor3='F'")
    expect_equal(dim(estim), c(6, 10))
    estim <- estimate_contrasts(model, contrast = c("factor1", "factor2"), at = "factor3")
    expect_equal(dim(estim), c(12, 10))


    # Mixed models
    data <- iris
    data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")

    model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)
    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(3, 9))


    # GLM - binomial
    dat <- iris
    dat$y <- as.factor(ifelse(dat$Sepal.Width > 3, "A", "B"))
    dat <<- dat
    model <- glm(y ~ Species, family = "binomial", data = dat)

    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(3, 9))
    estim <- estimate_contrasts(model, transform = "response")
    expect_equal(dim(estim), c(3, 9))

    # GLM - poisson
    dat <- data.frame(
      counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12),
      treatment = gl(3, 3)
    )
    dat <<- dat
    model <- glm(counts ~ treatment, data = dat, family = poisson())

    estim <- estimate_contrasts(model, transform = "response")
    expect_equal(dim(estim), c(3, 9))
  })


  test_that("estimate_contrasts - Bayesian", {
    dat <- iris
    dat$Petal.Length_factor <- ifelse(dat$Petal.Length < 4.2, "A", "B")
    dat <<- dat

    model <- suppressWarnings(
      rstanarm::stan_glm(
        Sepal.Width ~ Species * Petal.Length_factor,
        data = dat,
        refresh = 0,
        iter = 200,
        chains = 2
      )
    )
    estim <- estimate_contrasts(model, contrast = "all")
    expect_equal(dim(estim), c(15, 7))
    estim <- estimate_contrasts(model, fixed = "Petal.Length_factor")
    expect_equal(dim(estim), c(3, 8))

    model <- suppressWarnings(
      rstanarm::stan_glm(
        Sepal.Width ~ Species * Petal.Width,
        data = iris,
        refresh = 0,
        iter = 200,
        chains = 2
      )
    )
    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(3, 7))
    estim <- estimate_contrasts(model, fixed = "Petal.Width")
    expect_equal(dim(estim), c(3, 8))
    estim <- estimate_contrasts(model, at = "Petal.Width", length = 4)
    expect_equal(dim(estim), c(12, 8))

    # GLM
    dat <- iris
    dat$y <- as.numeric(as.factor(ifelse(dat$Sepal.Width > 3, "A", "B"))) - 1
    dat <<- dat
    model <- suppressWarnings(rstanarm::stan_glm(y ~ Species,
      family = "binomial", data = dat, refresh = 0,
      prior = rstanarm::normal(scale = 0.5)
    ))

    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(3, 7))
    estim <- estimate_contrasts(model, transform = "response")
    expect_equal(dim(estim), c(3, 7))

    estim <- suppressWarnings(estimate_contrasts(model, test = "bf"))
    expect_equal(dim(estim), c(3, 6))
    estim <- suppressWarnings(estimate_contrasts(model, transform = "response", test = "bf"))
    expect_equal(dim(estim), c(3, 6))
  })




  test_that("estimate_contrasts - p.adjust", {
    model <- lm(Petal.Width ~ Species, data = iris)

    p_none <- estimate_contrasts(model, p_adjust = "none")
    p_tuk <- estimate_contrasts(model, p_adjust = "tukey")

    expect_true(any(as.data.frame(p_none) != as.data.frame(p_tuk)))
  })

  test_that("estimate_contrasts - dfs", {
    data <- iris
    data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
    model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)

    estim1 <- estimate_contrasts(model, lmer.df = "satterthwaite")
    estim2 <- estimate_contrasts(model, lmer.df = "kenward-roger")

    # TODO: check out why this test is failing
    # expect_true(any(estim1$CI_low != estim2$CI_low))
  })
}
