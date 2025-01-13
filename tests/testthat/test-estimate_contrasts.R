skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")

test_that("estimate_contrasts - Frequentist", {
  skip_if_not_installed("logspline")
  skip_if_not_installed("lme4")

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
  model <- lm(Petal.Width ~ Species, data = iris)

  p_none <- suppressMessages(estimate_contrasts(model, p_adjust = "none"))
  p_tuk <- suppressMessages(estimate_contrasts(model, p_adjust = "tukey"))

  expect_true(any(as.data.frame(p_none) != as.data.frame(p_tuk)))
})


test_that("estimate_contrasts - dfs", {
  skip_if_not_installed("lme4")
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
  skip_if_not_installed("Formula")

  data(coffee_data, package = "ggeffects")
  m <- lm(alertness ~ time * coffee + sex, data = coffee_data)

  out <- estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects", p_adjust = "none")
  expect_snapshot(print(out, zap_small = TRUE, table_width = Inf))

  out <- estimate_contrasts(
    m,
    c("time", "coffee"),
    backend = "marginaleffects",
    p_adjust = "none",
    comparison = ratio ~ reference | coffee
  )
  expect_snapshot(print(out, zap_small = TRUE, table_width = Inf))

  out <- estimate_contrasts(
    m,
    c("time", "coffee"),
    backend = "marginaleffects",
    p_adjust = "none",
    comparison = "(b2-b1)=(b4-b3)"
  )
  expect_snapshot(print(out, zap_small = TRUE, table_width = Inf))
})


test_that("estimate_contrasts - marginaleffects", {
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


test_that("estimate_contrasts - works with slopes", {
  data(iris)
  fit <- lm(Sepal.Width ~ Petal.Length * Species, data = iris)

  out1 <- estimate_slopes(fit, trend = "Petal.Length", backend = "marginaleffects")
  out2 <- suppressMessages(as.data.frame(emmeans::emtrends(fit, specs = ~Petal.Length, var = "Petal.Length")))
  expect_equal(out1$Slope, out2$Petal.Length.trend, tolerance = 1e-3)

  out3 <- estimate_slopes(fit, trend = "Petal.Length", by = "Species", backend = "marginaleffects")
  out4 <- estimate_contrasts(fit, contrast = "Petal.Length", by = "Species", backend = "marginaleffects")
  out5 <- emmeans::emtrends(fit, specs = pairwise ~ Species, var = "Petal.Length")
  expect_equal(out3$Slope, as.data.frame(out5$emtrends)$Petal.Length.trend, tolerance = 1e-3)
  expect_equal(out4$Difference, as.data.frame(out5$contrasts)$estimate, tolerance = 1e-3)
})


test_that("estimate_contrasts - different options for comparison", {
  set.seed(123)
  dat <- data.frame(y = rpois(100, 3), fa = gl(4, 20, 100))
  dat_glm <- glm(y ~ fa, data = dat, family = poisson(link = "log"))

  # emmeans
  out <- estimate_contrasts(dat_glm, contrast = "fa", comparison = "eff")
  expect_named(
    out,
    c("Level", "Difference", "CI_low", "CI_high", "SE", "df", "z", "p")
  )
  expect_equal(out$Difference, c(0.2, 0.55, -0.6, -0.15), tolerance = 1e-3)
  out <- estimate_contrasts(dat_glm, contrast = "fa", comparison = "poly")
  expect_named(
    out,
    c("Level", "Difference", "CI_low", "CI_high", "SE", "df", "z", "p")
  )
  expect_equal(out$Difference, c(3.1, -2.2, 0.1), tolerance = 1e-3)

  # marginaleffects
  out <- estimate_contrasts(dat_glm, contrast = "fa", comparison = "pairwise", backend = "marginaleffects")
  expect_named(
    out,
    c("fa", "Difference", "SE", "CI_low", "CI_high", "z", "p")
  )
  expect_equal(out$Difference, c(-0.35, 0.8, 0.35, 1.15, 0.7, -0.45), tolerance = 1e-3)
  out <- estimate_contrasts(dat_glm, contrast = "fa", comparison = "reference", backend = "marginaleffects")
  expect_named(
    out,
    c("fa", "Difference", "SE", "CI_low", "CI_high", "z", "p")
  )
  expect_equal(out$Difference, c(0.35, -0.8, -0.35), tolerance = 1e-3)
})


test_that("estimate_contrasts - filtering works", {
  skip_if_not_installed("ggeffects")
  data(efc, package = "ggeffects")

  # make categorical
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex"))
  levels(efc$c172code) <- c("low", "mid", "high")
  fit <- lm(neg_c_7 ~ e16sex + c161sex + c172code, data = efc)
  out <- estimate_contrasts(fit, "c172code", backend = "marginaleffects")
  expect_snapshot(print(out, table_width = Inf))

  fit <- lm(neg_c_7 ~ e16sex + c161sex * c172code, data = efc)
  out <- estimate_contrasts(fit, c("c161sex", "c172code"), backend = "marginaleffects")
  expect_snapshot(print(out, table_width = Inf))
  out <- estimate_contrasts(fit, "c161sex", "c172code", backend = "marginaleffects")
  expect_snapshot(print(out, table_width = Inf))

  fit <- lm(neg_c_7 ~ barthtot + c161sex + c172code, data = efc)
  out <- estimate_slopes(fit, "barthtot", backend = "marginaleffects")
  expect_snapshot(print(out, table_width = Inf))
  # error
  expect_error(
    estimate_contrasts(fit, "barthtot", backend = "marginaleffects"),
    regex = "Please specify"
  )

  fit <- lm(neg_c_7 ~ e16sex + barthtot * c172code, data = efc)
  out <- estimate_slopes(fit, "barthtot", by = "c172code", backend = "marginaleffects")
  expect_snapshot(print(out, table_width = Inf))
  out <- estimate_contrasts(fit, "barthtot", "c172code", backend = "marginaleffects")
  expect_snapshot(print(out, table_width = Inf))
  # error
  expect_error(
    estimate_contrasts(fit, c("barthtot", "c172code"), backend = "marginaleffects"),
    regex = "Please specify"
  )
})


test_that("estimate_contrasts - simple contrasts and with - in levels works", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("ggeffects")

  model <- lm(Sepal.Length ~ Species + Sepal.Width, data = iris)
  out <- estimate_contrasts(model, "Species", backend = "marginaleffects")
  expect_snapshot(print(out, table_width = Inf))

  data(coffee_data, package = "ggeffects")
  m <- lm(alertness ~ time * coffee + sex, data = coffee_data)
  out <- estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects")
  expect_snapshot(print(out, zap_small = TRUE, table_width = Inf))

  out <- estimate_contrasts(m, contrast = "time", by = "coffee", backend = "marginaleffects")
  expect_snapshot(print(out, zap_small = TRUE, table_width = Inf))

  data(Salamanders, package = "glmmTMB")
  model <- glmmTMB::glmmTMB(count ~ mined * spp + cover + (1 | site), data = Salamanders, family = "poisson")
  out <- estimate_contrasts(model, contrast = c("mined", "spp"), backend = "marginaleffects")
  expect_snapshot(print(out, zap_small = TRUE, table_width = Inf))
  out <- estimate_contrasts(model, contrast = "mined", by = "spp", backend = "marginaleffects")
  expect_snapshot(print(out, zap_small = TRUE, table_width = Inf))
})
