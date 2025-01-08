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

  estim1 <- suppressMessages(estimate_contrasts(model))
  expect_identical(dim(estim1), c(3L, 9L))
  estim2 <- suppressMessages(estimate_contrasts(model, predict = "link"))
  expect_identical(dim(estim2), c(3L, 9L))
  expect_true(all(estim1$Difference != estim2$Difference))

  estim3 <- suppressWarnings(suppressMessages(estimate_contrasts(model, backend = "marginaleffects")))
  expect_identical(estim3$Difference, estim1$Difference)
  estim4 <- suppressWarnings(suppressMessages(estimate_contrasts(model, predict = "link", backend = "marginaleffects")))
  expect_identical(estim4$Difference, estim2$Difference)

  # GLM - poisson
  dat <- data.frame(
    counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12),
    treatment = gl(3, 3)
  )
  dat <<- dat
  model <- glm(counts ~ treatment, data = dat, family = poisson())

  estim <- suppressMessages(estimate_contrasts(model, predict = "response"))
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
  estim <- suppressMessages(estimate_contrasts(model, predict = "link"))
  expect_identical(dim(estim), c(3L, 7L))

  estim <- suppressWarnings(suppressMessages(estimate_contrasts(model, test = "bf")))
  expect_identical(dim(estim), c(3L, 6L))
  estim <- suppressWarnings(suppressMessages(estimate_contrasts(model, predict = "link", test = "bf")))
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
  skip_if_not_installed("pbkrtest")
  skip_if_not_installed("lmerTest")

  data <- iris
  data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
  model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)

  estim1 <- suppressMessages(estimate_contrasts(model, lmer.df = "satterthwaite"))
  estim2 <- suppressMessages(estimate_contrasts(model, lmer.df = "kenward-roger"))

  expect_true(all(estim1$CI_low != estim2$CI_low))
  expect_equal(estim1$CI_low, c(-2.43, -2.25692, -2.89384), tolerance = 1e-4)
  expect_equal(estim2$CI_low, c(-2.62766, -2.53389, -2.98196), tolerance = 1e-4)
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


test_that("estimate_contrasts - marginaleffects", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("ggeffects")

  data(iris)
  dat <- iris
  dat$y <- as.factor(ifelse(dat$Sepal.Width > 3, "A", "B"))
  model <- glm(y ~ Species, family = "binomial", data = dat)

  expect_message(estimate_contrasts(model), regex = "No variable was")

  ## emmeans backend works and has proper default
  out1 <- suppressMessages(estimate_contrasts(model))
  out2 <- suppressMessages(estimate_contrasts(model, predict = "response"))
  pr <- ggeffects::predict_response(model, "Species")
  out3 <- ggeffects::test_predictions(pr)
  expect_equal(out1$Difference, out2$Difference, tolerance = 1e-4)
  expect_equal(out1$Difference, out3$Contrast, tolerance = 1e-4)

  ## marginaleffects backend works and has proper default
  out4 <- suppressMessages(estimate_contrasts(model, backend = "marginaleffects"))
  out5 <- suppressMessages(estimate_contrasts(model, predict = "response", backend = "marginaleffects"))
  expect_equal(out4$Difference, out5$Difference, tolerance = 1e-4)
  expect_equal(out4$Difference, out3$Contrast, tolerance = 1e-4)
  expect_equal(out4$CI_low, out3$conf.low, tolerance = 1e-2)

  # validate against emmeans
  out_emm <- emmeans::emmeans(model, "Species", type = "response")
  out_emm <- emmeans::regrid(out_emm)
  out6 <- as.data.frame(emmeans::contrast(out_emm, method = "pairwise"))
  expect_equal(out6$estimate, out1$Difference, tolerance = 1e-3)

  # validate against marginaleffects
  out7 <- marginaleffects::avg_predictions(model, by = "Species", hypothesis = "pairwise")
  expect_equal(out7$estimate, out4$Difference, tolerance = 1e-3)

  # test p-adjust
  expect_snapshot(estimate_contrasts(model))
  expect_snapshot(estimate_contrasts(model, backend = "marginaleffects"))
})


test_that("estimate_contrasts - on-the-fly factors", {
  data(mtcars)
  model <- lm(mpg ~ as.factor(cyl) + wt * hp, mtcars)
  out1 <- estimate_contrasts(model)
  out2 <- estimate_contrasts(model, contrast = "cyl", backend = "marginaleffects")

  expect_identical(nrow(out1), 3L)
  expect_identical(nrow(out2), 3L)
  expect_equal(out1$Difference, out2$Difference, tolerance = 1e-4)

  mtcars2 <- mtcars
  mtcars2$cyl <- as.factor(mtcars2$cyl)
  model <- lm(mpg ~ cyl + wt * hp, mtcars2)
  out3 <- estimate_contrasts(model)
  out4 <- estimate_contrasts(model, contrast = "cyl", backend = "marginaleffects")

  expect_identical(nrow(out3), 3L)
  expect_identical(nrow(out4), 3L)
  expect_equal(out3$Difference, out4$Difference, tolerance = 1e-4)
})
