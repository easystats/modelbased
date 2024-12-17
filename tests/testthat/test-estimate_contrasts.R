test_that("estimate_contrasts - Frequentist", {
  skip_if_not_installed("logspline")
  skip_if_not_installed("lme4")
  skip_if_not_installed("emmeans")

  # One factor
  dat <<- iris
  model <- lm(Sepal.Width ~ Species, data = dat)

  estim <- suppressMessages(estimate_contrasts(model))
  expect_identical(dim(estim), c(3L, 9L))
  expect_equal(estim$Difference, c(0.658, 0.454, -0.204), tolerance = 1e-4)

  estim <- suppressMessages(estimate_contrasts(model, by = "Species=c('versicolor', 'virginica')"))
  expect_identical(dim(estim), c(1L, 9L))

  # Two factors
  dat <- iris
  dat$fac <- ifelse(dat$Sepal.Length < 5.8, "A", "B")
  dat <<- dat

  model <- lm(Sepal.Width ~ Species * fac, data = dat)

  estim <- suppressMessages(estimate_contrasts(model))
  expect_identical(dim(estim), c(3L, 9L))
  estim <- suppressMessages(estimate_contrasts(model, levels = "Species"))
  expect_identical(dim(estim), c(3L, 9L))
  estim <- suppressMessages(estimate_contrasts(model, by = c("Species", "fac='A'")))
  expect_identical(dim(estim), c(3L, 10L))

  # One factor and one continuous
  model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
  estim <- suppressMessages(estimate_contrasts(model))
  expect_identical(dim(estim), c(3L, 9L))
  estim <- suppressMessages(estimate_contrasts(model, by = c("Species", "Petal.Width=0")))
  expect_identical(dim(estim), c(3L, 10L))
  estim <- suppressMessages(estimate_contrasts(model, by = "Petal.Width", length = 4))
  expect_identical(dim(estim), c(12L, 10L))


  # Contrast between continuous
  model <- lm(Sepal.Width ~ Petal.Length, data = iris)

  estim <- suppressMessages(estimate_contrasts(model, by = "Petal.Length=c(2.3, 3)"))
  expect_identical(dim(estim), c(1L, 9L))
  estim <- suppressMessages(estimate_contrasts(model, by = "Petal.Length=c(2, 3, 4)"))
  expect_identical(dim(estim), c(3L, 9L))


  # Three factors
  dat <- mtcars
  dat[c("gear", "vs", "am")] <- sapply(dat[c("gear", "vs", "am")], as.factor)
  dat <<- dat
  model <- lm(mpg ~ gear * vs * am, data = dat)

  estim <- suppressMessages(estimate_contrasts(model, by = "all"))
  expect_identical(dim(estim), c(12L, 11L))
  estim <- suppressMessages(estimate_contrasts(model, contrast = c("vs", "am"), by = "gear='5'"))
  expect_identical(dim(estim), c(1L, 10L))


  dat <- iris
  dat$factor1 <- ifelse(dat$Sepal.Width > 3, "A", "B")
  dat$factor2 <- ifelse(dat$Petal.Length > 3.5, "C", "D")
  dat$factor3 <- ifelse(dat$Sepal.Length > 5, "E", "F")
  dat <<- dat

  model <- lm(Petal.Width ~ factor1 * factor2 * factor3, data = dat)

  estim <- suppressMessages(estimate_contrasts(model, contrast = c("factor1", "factor2", "factor3"), by = "all"))
  expect_identical(dim(estim), c(28L, 9L))
  estim <- suppressMessages(estimate_contrasts(model, contrast = c("factor1", "factor2"), by = "factor3='F'"))
  expect_identical(dim(estim), c(6L, 10L))
  estim <- suppressMessages(estimate_contrasts(model, contrast = c("factor1", "factor2"), by = "factor3"))
  expect_identical(dim(estim), c(12L, 10L))


  # Mixed models
  data <- iris
  data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")

  model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)
  estim <- suppressMessages(estimate_contrasts(model))
  expect_identical(dim(estim), c(3L, 9L))


  # GLM - binomial
  dat <- iris
  dat$y <- as.factor(ifelse(dat$Sepal.Width > 3, "A", "B"))
  dat <<- dat
  model <- glm(y ~ Species, family = "binomial", data = dat)

  estim <- suppressMessages(estimate_contrasts(model))
  expect_identical(dim(estim), c(3L, 9L))
  estim <- suppressMessages(estimate_contrasts(model, transform = "response"))
  expect_identical(dim(estim), c(3L, 9L))

  # GLM - poisson
  dat <- data.frame(
    counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12),
    treatment = gl(3, 3)
  )
  dat <<- dat
  model <- glm(counts ~ treatment, data = dat, family = poisson())

  estim <- suppressMessages(estimate_contrasts(model, transform = "response"))
  expect_identical(dim(estim), c(3L, 9L))
})


test_that("estimate_contrasts - Bayesian", {
  skip_if_not_installed("logspline")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("lme4")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("coda")

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
  estim <- suppressMessages(estimate_contrasts(model, contrast = "all"))
  expect_identical(dim(estim), c(15L, 7L))
  estim <- suppressMessages(estimate_contrasts(model, by = c("Species", "Petal.Length_factor='A'")))
  expect_identical(dim(estim), c(3L, 8L))

  model <- suppressWarnings(
    rstanarm::stan_glm(
      Sepal.Width ~ Species * Petal.Width,
      data = iris,
      refresh = 0,
      iter = 200,
      chains = 2
    )
  )
  estim <- suppressMessages(estimate_contrasts(model))
  expect_identical(dim(estim), c(3L, 7L))
  estim <- suppressMessages(estimate_contrasts(model, by = c("Species", "Petal.Width=0")))
  expect_identical(dim(estim), c(3L, 8L))
  estim <- suppressMessages(estimate_contrasts(model, by = "Petal.Width", length = 4))
  expect_identical(dim(estim), c(12L, 8L))

  # GLM
  dat <- iris
  dat$y <- as.numeric(as.factor(ifelse(dat$Sepal.Width > 3, "A", "B"))) - 1
  dat <<- dat
  model <- suppressWarnings(rstanarm::stan_glm(y ~ Species,
    family = "binomial", data = dat, refresh = 0,
    prior = rstanarm::normal(scale = 0.5)
  ))

  estim <- suppressMessages(estimate_contrasts(model))
  expect_identical(dim(estim), c(3L, 7L))
  estim <- suppressMessages(estimate_contrasts(model, transform = "response"))
  expect_identical(dim(estim), c(3L, 7L))

  estim <- suppressWarnings(suppressMessages(estimate_contrasts(model, test = "bf")))
  expect_identical(dim(estim), c(3L, 6L))
  estim <- suppressWarnings(suppressMessages(estimate_contrasts(model, transform = "response", test = "bf")))
  expect_identical(dim(estim), c(3L, 6L))
})


test_that("estimate_contrasts - p.adjust", {
  skip_if_not_installed("emmeans")

  model <- lm(Petal.Width ~ Species, data = iris)

  p_none <- suppressMessages(estimate_contrasts(model, p_adjust = "none"))
  p_tuk <- suppressMessages(estimate_contrasts(model, p_adjust = "tukey"))

  expect_true(any(as.data.frame(p_none) != as.data.frame(p_tuk)))
})


test_that("estimate_contrasts - dfs", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("emmeans")

  data <- iris
  data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
  model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)

  estim1 <- suppressMessages(estimate_contrasts(model, lmer.df = "satterthwaite"))
  estim2 <- suppressMessages(estimate_contrasts(model, lmer.df = "kenward-roger"))

  # TODO: check out why this test is failing
  # expect_true(any(estim1$CI_low != estim2$CI_low))
})


test_that("estimate_contrasts - marginaleffects", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("ggeffects")
  skip_if_not_installed("Formula")

  data(coffee_data, package = "ggeffects")
  m <- lm(alertness ~ time * coffee + sex, data = coffee_data)

  out <- estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects", p_adjust = "none")
  expect_snapshot(print(out, zap_small = TRUE))

  out <- estimate_contrasts(
    m,
    c("time", "coffee"),
    backend = "marginaleffects",
    p_adjust = "none",
    method = ratio ~ reference | coffee
  )
  expect_snapshot(print(out, zap_small = TRUE))

  out <- estimate_contrasts(
    m,
    c("time", "coffee"),
    backend = "marginaleffects",
    p_adjust = "none",
    method = "(b2-b1)=(b4-b3)"
  )
  expect_snapshot(print(out, zap_small = TRUE))
})
