skip_on_cran()
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")
skip_on_os("mac")

test_that("estimate_contrasts - Frequentist", {
  skip_if_not_installed("logspline")
  skip_if_not_installed("lme4")
  data(iris)

  # One factor
  dat <<- iris
  model <- lm(Sepal.Width ~ Species, data = dat)

  estim <- suppressMessages(estimate_contrasts(model, backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 9L))
  expect_equal(estim$Difference, c(0.658, 0.454, -0.204), tolerance = 1e-4)

  estim <- suppressMessages(estimate_contrasts(model, backend = "marginaleffects"))
  expect_identical(dim(estim), c(3L, 9L))
  expect_equal(estim$Difference, c(0.658, 0.454, -0.204), tolerance = 1e-4)

  # out <- marginaleffects::avg_predictions(
  #   model,
  #   by = "Species",
  #   newdata = insight::get_datagrid(model, "Species"),
  #   hypothesis = ~pairwise
  # )

  estim <- suppressMessages(estimate_contrasts(model, by = "Species=c('versicolor', 'virginica')", backend = "emmeans"))
  expect_identical(dim(estim), c(1L, 9L))

  estim <- suppressMessages(estimate_contrasts(model, contrast = "Species=c('versicolor', 'virginica')", backend = "marginaleffects"))
  expect_identical(dim(estim), c(1L, 9L))

  # Two factors
  dat <- iris
  dat$fac <- ifelse(dat$Sepal.Length < 5.8, "A", "B")
  dat <<- dat

  model <- lm(Sepal.Width ~ Species * fac, data = dat)

  estim <- suppressMessages(estimate_contrasts(model, backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 9L))
  estim <- suppressMessages(estimate_contrasts(model, levels = "Species", backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 9L))
  estim <- suppressMessages(estimate_contrasts(model, by = c("Species", "fac='A'"), backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 10L))
  estim <- suppressMessages(estimate_contrasts(model, contrast = "Species", backend = "marginaleffects"))
  expect_identical(dim(estim), c(3L, 9L))
  estim <- suppressMessages(estimate_contrasts(model, contrast = "Species", by = "fac='A'", backend = "marginaleffects"))
  expect_identical(dim(estim), c(3L, 9L))

  # One factor and one continuous
  model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
  estim <- suppressMessages(estimate_contrasts(model, backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 9L))
  estim <- suppressMessages(estimate_contrasts(model, by = c("Species", "Petal.Width=0"), backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 10L))
  estim <- suppressMessages(estimate_contrasts(model, by = "Petal.Width", length = 4, backend = "emmeans"))
  expect_identical(dim(estim), c(12L, 10L))
  estim <- estimate_contrasts(model, contrast = "Species", by = "Petal.Width=0", backend = "marginaleffects")
  expect_identical(dim(estim), c(3L, 9L))
  estim <- estimate_contrasts(model, contrast = "Petal.Width", by = "Species", length = 4, backend = "marginaleffects")
  expect_equal(estim$Difference, c(-0.21646, 0.20579, 0.42224), tolerance = 1e-4)


  # Contrast between continuous
  model <- lm(Sepal.Width ~ Petal.Length, data = iris)

  estim <- suppressMessages(estimate_contrasts(model, by = "Petal.Length=c(2.3, 3)", backend = "emmeans"))
  expect_identical(dim(estim), c(1L, 9L))
  estim <- suppressMessages(estimate_contrasts(model, by = "Petal.Length=c(2, 3, 4)", backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 9L))
  estim <- estimate_contrasts(model, contrast = "Petal.Length=c(2.3, 3)", backend = "marginaleffects")
  expect_identical(dim(estim), c(1L, 9L))
  estim <- estimate_contrasts(model, contrast = "Petal.Length=c(2, 3, 4)", backend = "marginaleffects")
  expect_identical(dim(estim), c(3L, 9L))


  # Three factors
  dat <- mtcars
  dat[c("gear", "vs", "am")] <- sapply(dat[c("gear", "vs", "am")], as.factor)
  dat <<- dat
  model <- lm(mpg ~ gear * vs * am, data = dat)

  estim <- suppressMessages(estimate_contrasts(model, by = "all", backend = "emmeans"))
  expect_identical(dim(estim), c(12L, 11L))
  estim <- suppressMessages(estimate_contrasts(model, contrast = c("vs", "am"), by = "gear='5'", backend = "emmeans"))
  expect_identical(dim(estim), c(1L, 10L))
  ## FIXME: doesn't work right nw
  # estim <- suppressMessages(estimate_contrasts(model, by = "all", backend = "marginaleffects"))
  # expect_identical(dim(estim), c(12L, 11L))
  estim <- suppressMessages(estimate_contrasts(model, contrast = c("vs", "am"), by = "gear='5'", backend = "marginaleffects"))
  expect_identical(dim(estim), c(6L, 9L))
  expect_named(estim, c("Level1", "Level2", "Difference", "SE", "CI_low", "CI_high", "t", "df", "p"))
  expect_equal(estim$Difference, c(-6.98333, -11.275, -4.29167, -18.25833, -11.275, -6.98333), tolerance = 1e-4)
  expect_snapshot(print(estimate_contrasts(model, contrast = c("vs", "am"), by = "gear='5'", backend = "marginaleffects"), zap_small = TRUE, table_width = Inf)) # nolint

  # duplicated levels
  dat <- mtcars
  dat[c("vs", "am")] <- sapply(dat[c("vs", "am")], as.factor)
  set.seed(123)
  dat$three <- factor(sample(0:1, nrow(dat), replace = TRUE))
  model <- lm(mpg ~ three * vs * am, data = dat)
  expect_snapshot(print(estimate_contrasts(model, contrast = c("three", "vs", "am"), backend = "marginaleffects"), zap_small = TRUE, table_width = Inf)) # nolint
  expect_snapshot(print(estimate_contrasts(model, contrast = "am", backend = "marginaleffects"), zap_small = TRUE, table_width = Inf)) # nolint


  dat <- iris
  dat$factor1 <- ifelse(dat$Sepal.Width > 3, "A", "B")
  dat$factor2 <- ifelse(dat$Petal.Length > 3.5, "C", "D")
  dat$factor3 <- ifelse(dat$Sepal.Length > 5, "E", "F")
  dat <<- dat

  model <- lm(Petal.Width ~ factor1 * factor2 * factor3, data = dat)

  estim <- suppressMessages(estimate_contrasts(model, contrast = c("factor1", "factor2", "factor3"), by = "all", backend = "emmeans"))
  expect_identical(dim(estim), c(28L, 9L))
  estim <- suppressMessages(estimate_contrasts(model, contrast = c("factor1", "factor2"), by = "factor3='F'", backend = "emmeans"))
  expect_identical(dim(estim), c(6L, 10L))
  estim <- suppressMessages(estimate_contrasts(model, contrast = c("factor1", "factor2"), by = "factor3", backend = "emmeans"))
  expect_identical(dim(estim), c(12L, 10L))


  # Mixed models
  data <- iris
  data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")

  model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)
  estim <- suppressMessages(estimate_contrasts(model, backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 9L))


  # GLM - binomial
  dat <- iris
  dat$y <- as.factor(ifelse(dat$Sepal.Width > 3, "A", "B"))
  dat <<- dat
  model <- glm(y ~ Species, family = "binomial", data = dat)

  estim1 <- suppressMessages(estimate_contrasts(model, backend = "emmeans"))
  expect_identical(dim(estim1), c(3L, 9L))
  estim2 <- suppressMessages(estimate_contrasts(model, predict = "link", backend = "emmeans"))
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

  estim <- suppressMessages(estimate_contrasts(model, predict = "response", backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 9L))
})


test_that("estimate_contrasts - Bayesian", {
  skip_on_cran()
  skip_if_not_installed("logspline")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("lme4")
  skip_if_not_installed("coda")
  data(iris)

  dat <- iris
  dat$Petal.Length_factor <- ifelse(dat$Petal.Length < 4.2, "A", "B")
  dat <<- dat

  set.seed(123)
  model <- suppressWarnings(
    rstanarm::stan_glm(
      Sepal.Width ~ Species * Petal.Length_factor,
      data = dat,
      refresh = 0,
      iter = 200,
      chains = 2,
      seed = 123
    )
  )
  estim <- suppressMessages(estimate_contrasts(model, contrast = "all", backend = "emmeans"))
  expect_identical(dim(estim), c(15L, 7L))
  estim <- suppressMessages(estimate_contrasts(model, by = c("Species", "Petal.Length_factor='A'"), backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 8L))
  estim <- estimate_contrasts(model, contrast = c("Species", "Petal.Length_factor"), backend = "marginaleffects")
  expect_identical(dim(estim), c(15L, 10L))
  expect_named(
    estim,
    c(
      "Level1", "Level2", "ROPE_CI", "Median", "CI_low",
      "CI_high", "pd", "ROPE_low", "ROPE_high", "ROPE_Percentage"
    )
  )
  expect_equal(
    estim$Median,
    c(
      0.05025, 0.89132, 0.83305, 0.52141, 0.47175, -0.36936, 0.27182,
      -0.00022, -0.60636, -0.20898, 0.45085, 0.45184, -0.43568, -0.068,
      0.18029
    ),
    tolerance = 1e-4
  )


  set.seed(123)
  model <- suppressWarnings(
    rstanarm::stan_glm(
      Sepal.Width ~ Species * Petal.Width,
      data = iris,
      refresh = 0,
      iter = 200,
      chains = 2,
      seed = 123
    )
  )
  estim <- suppressMessages(estimate_contrasts(model, backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 7L))
  estim <- suppressMessages(estimate_contrasts(model, by = c("Species", "Petal.Width=0"), backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 8L))
  estim <- suppressMessages(estimate_contrasts(model, by = "Petal.Width", length = 4, backend = "emmeans"))
  expect_identical(dim(estim), c(12L, 8L))

  # GLM
  dat <- iris
  dat$y <- as.numeric(as.factor(ifelse(dat$Sepal.Width > 3, "A", "B"))) - 1
  dat <<- dat
  model <- suppressWarnings(rstanarm::stan_glm(y ~ Species,
    family = "binomial", data = dat, refresh = 0,
    prior = rstanarm::normal(scale = 0.5)
  ))

  estim <- suppressMessages(estimate_contrasts(model, backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 7L))
  estim <- suppressMessages(estimate_contrasts(model, predict = "link", backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 7L))

  estim <- suppressWarnings(suppressMessages(estimate_contrasts(model, test = "bf", backend = "emmeans")))
  expect_identical(dim(estim), c(3L, 6L))
  estim <- suppressWarnings(suppressMessages(estimate_contrasts(model, predict = "link", test = "bf", backend = "emmeans")))
  expect_identical(dim(estim), c(3L, 6L))
})


test_that("estimate_contrasts - p.adjust", {
  data(iris)
  model <- lm(Petal.Width ~ Species, data = iris)

  p_none <- suppressMessages(estimate_contrasts(model, p_adjust = "none", backend = "emmeans"))
  p_tuk <- suppressMessages(estimate_contrasts(model, p_adjust = "tukey", backend = "emmeans"))
  expect_true(any(p_none$p != p_tuk$p))

  p_none <- suppressMessages(estimate_contrasts(model, p_adjust = "none", backend = "marginaleffects"))
  p_tuk <- suppressMessages(estimate_contrasts(model, p_adjust = "tukey", backend = "marginaleffects"))
  expect_true(any(p_none$p != p_tuk$p))
})


test_that("estimate_contrasts - dfs", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("pbkrtest")
  skip_if_not_installed("lmerTest")
  data(iris)

  data <- iris
  data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
  model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)

  estim1 <- suppressMessages(estimate_contrasts(model, lmer.df = "satterthwaite", p_adjust = "holm", backend = "emmeans"))
  estim2 <- suppressMessages(estimate_contrasts(model, lmer.df = "kenward-roger", p_adjust = "holm", backend = "emmeans"))

  expect_true(all(estim1$CI_low != estim2$CI_low))
  expect_equal(estim1$CI_low, c(-2.43, -2.25692, -2.89384), tolerance = 1e-4)
  expect_equal(estim2$CI_low, c(-2.62766, -2.53389, -2.98196), tolerance = 1e-4)

  estim1 <- suppressMessages(estimate_contrasts(model, lmer.df = "satterthwaite", backend = "emmeans"))
  estim2 <- suppressMessages(estimate_contrasts(model, lmer.df = "kenward-roger", backend = "emmeans"))

  expect_true(all(estim1$CI_low != estim2$CI_low))
  expect_equal(estim1$CI_low, c(-0.22624, -0.33383, -1.0109), tolerance = 1e-4)
  expect_equal(estim2$CI_low, c(-0.29193, -0.4364, -1.04019), tolerance = 1e-4)
})


test_that("estimate_contrasts - marginaleffects", {
  # ignore failing snapshot tests, should resolve with next marginaleffects version
  skip_if_not_installed("Formula")
  data(coffee_data, package = "modelbased")
  m <- lm(alertness ~ time * coffee + sex, data = coffee_data)
  expect_snapshot(print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects", p_adjust = "none"), zap_small = TRUE, table_width = Inf)) # nolint

  ## FIXME: doesn't work for marginaleffects > 0.24.0
  # expect_snapshot(print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects", p_adjust = "none", comparison = ratio ~ reference | coffee), zap_small = TRUE, table_width = Inf)) # nolint

  expect_snapshot(print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects", p_adjust = "none", comparison = "(b2-b1)=(b4-b3)"), zap_small = TRUE, table_width = Inf)) # nolint
  out1 <- estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects", p_adjust = "none", comparison = "(b2-b1)=(b4-b3)")
  out2 <- predict(m, newdata = insight::get_datagrid(m, c("time", "coffee")))
  expect_equal(out1$Difference, 5.78298, tolerance = 1e-4)
  expect_equal(out1$Difference, (out2[2] - out2[1]) - (out2[4] - out2[3]), tolerance = 1e-4)
  out1 <- estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects", p_adjust = "none", comparison = "b5=b3")
  expect_equal(out1$Difference, -1.927659, tolerance = 1e-4)
  expect_equal(out1$Difference, out2[5] - out2[3], tolerance = 1e-4)
  expect_snapshot(print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects", p_adjust = "none", comparison = "b5=b3"), zap_small = TRUE, table_width = Inf)) # nolint

  # validated against ggeffects::test_predictions()
  data(efc, package = "modelbased")
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
  fit <- lm(neg_c_7 ~ c12hour + barthtot + c161sex + e42dep * c172code, data = efc)
  expect_snapshot(estimate_contrasts(fit, c("e42dep", "c172code"), comparison = "b6-b3=0", backend = "marginaleffects"))
})


test_that("estimate_contrasts - marginaleffects", {
  data(iris)
  dat <- iris
  dat$y <- as.factor(ifelse(dat$Sepal.Width > 3, "A", "B"))
  model <- glm(y ~ Species, family = "binomial", data = dat)

  expect_message(estimate_contrasts(model, backend = "emmeans"), regex = "No variable was")

  ## emmeans backend works and has proper default
  out1 <- suppressMessages(estimate_contrasts(model, backend = "emmeans"))
  out2 <- suppressMessages(estimate_contrasts(model, predict = "response", backend = "emmeans"))
  expect_equal(out1$Difference, out2$Difference, tolerance = 1e-4)
  expect_equal(out1$Difference, c(-0.68, -0.5, 0.18), tolerance = 1e-4)

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
  expect_snapshot(estimate_contrasts(model, backend = "emmeans"))
  expect_snapshot(estimate_contrasts(model, backend = "marginaleffects"))
  expect_snapshot(estimate_contrasts(model, backend = "emmeans", p_adjust = "holm"))
  expect_snapshot(estimate_contrasts(model, backend = "marginaleffects", p_adjust = "holm"))
})


test_that("estimate_contrasts - on-the-fly factors", {
  data(mtcars)
  model <- lm(mpg ~ as.factor(cyl) + wt * hp, mtcars)
  out1 <- estimate_contrasts(model, backend = "emmeans")
  out2 <- estimate_contrasts(model, contrast = "cyl", backend = "marginaleffects")

  expect_identical(nrow(out1), 3L)
  expect_identical(nrow(out2), 3L)
  expect_equal(out1$Difference, out2$Difference, tolerance = 1e-4)

  mtcars2 <- mtcars
  mtcars2$cyl <- as.factor(mtcars2$cyl)
  model <- lm(mpg ~ cyl + wt * hp, mtcars2)
  out3 <- estimate_contrasts(model, backend = "emmeans")
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
  out <- estimate_contrasts(dat_glm, contrast = "fa", comparison = "eff", backend = "emmeans")
  expect_named(
    out,
    c("Level", "Difference", "CI_low", "CI_high", "SE", "df", "z", "p")
  )
  expect_equal(out$Difference, c(0.2, 0.55, -0.6, -0.15), tolerance = 1e-3)
  out <- estimate_contrasts(dat_glm, contrast = "fa", comparison = "poly", backend = "emmeans")
  expect_named(
    out,
    c("Level", "Difference", "CI_low", "CI_high", "SE", "df", "z", "p")
  )
  expect_equal(out$Difference, c(3.1, -2.2, 0.1), tolerance = 1e-3)

  # marginaleffects
  out <- estimate_contrasts(dat_glm, contrast = "fa", comparison = "pairwise", backend = "marginaleffects")
  expect_named(
    out,
    c("Level1", "Level2", "Difference", "SE", "CI_low", "CI_high", "z", "p")
  )
  expect_equal(out$Difference, c(-0.35, 0.8, 1.15, 0.35, 0.7, -0.45), tolerance = 1e-3)
  out <- estimate_contrasts(dat_glm, contrast = "fa", comparison = "reference", backend = "marginaleffects")
  expect_named(
    out,
    c("Level1", "Level2", "Difference", "SE", "CI_low", "CI_high", "z", "p")
  )
  expect_equal(out$Difference, c(0.35, -0.8, -0.35), tolerance = 1e-3)
})

skip_on_os(c("mac", "linux"))

test_that("estimate_contrasts - filtering works", {
  data(efc, package = "modelbased")

  # make categorical
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex"))
  levels(efc$c172code) <- c("low", "mid", "high")
  fit <- lm(neg_c_7 ~ e16sex + c161sex + c172code, data = efc)
  expect_snapshot(print(estimate_contrasts(fit, "c172code", backend = "marginaleffects"), table_width = Inf, zap_small = TRUE)) # nolint

  fit <- lm(neg_c_7 ~ e16sex + c161sex * c172code, data = efc)
  expect_snapshot(print(estimate_contrasts(fit, c("c161sex", "c172code"), backend = "marginaleffects"), table_width = Inf, zap_small = TRUE)) # nolint
  expect_snapshot(print(estimate_contrasts(fit, "c161sex", "c172code", backend = "marginaleffects"), table_width = Inf, zap_small = TRUE)) # nolint

  fit <- lm(neg_c_7 ~ barthtot + c161sex + c172code, data = efc)
  expect_snapshot(print(estimate_slopes(fit, "barthtot", backend = "marginaleffects"), table_width = Inf, zap_small = TRUE)) # nolint
  # error
  expect_error(
    estimate_contrasts(fit, "barthtot", backend = "marginaleffects"),
    regex = "Please specify"
  )

  fit <- lm(neg_c_7 ~ e16sex + barthtot * c172code, data = efc)
  expect_snapshot(print(estimate_slopes(fit, "barthtot", by = "c172code", backend = "marginaleffects"), table_width = Inf, zap_small = TRUE)) # nolint
  expect_snapshot(print(estimate_contrasts(fit, "barthtot", "c172code", backend = "marginaleffects"), table_width = Inf, zap_small = TRUE)) # nolint
  fit <- lm(neg_c_7 ~ e16sex * barthtot * c172code, data = efc)
  expect_snapshot(print(estimate_contrasts(fit, "barthtot", c("c172code", "e16sex"), backend = "marginaleffects"), table_width = Inf, zap_small = TRUE)) # nolint
  # error
  expect_error(
    estimate_contrasts(fit, c("barthtot", "c172code"), backend = "marginaleffects"),
    regex = "Please specify"
  )
})


test_that("estimate_contrasts - simple contrasts and with - in levels works", {
  skip_if_not_installed("glmmTMB")
  data(iris)

  model <- lm(Sepal.Length ~ Species + Sepal.Width, data = iris)
  expect_snapshot(print(estimate_contrasts(model, "Species", backend = "marginaleffects"), table_width = Inf)) # nolint

  data(coffee_data, package = "modelbased")
  m <- lm(alertness ~ time * coffee + sex, data = coffee_data)
  expect_snapshot(print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects"), zap_small = TRUE, table_width = Inf)) # nolint

  expect_snapshot(print(estimate_contrasts(m, contrast = "time", by = "coffee", backend = "marginaleffects"), zap_small = TRUE, table_width = Inf)) # nolint

  data(Salamanders, package = "glmmTMB")
  model <- glmmTMB::glmmTMB(count ~ mined * spp + cover + (1 | site), data = Salamanders, family = "poisson") # nolint
  expect_snapshot(print(estimate_contrasts(model, contrast = c("mined", "spp"), backend = "marginaleffects"), zap_small = TRUE, table_width = Inf)) # nolint
  expect_snapshot(print(estimate_contrasts(model, contrast = "mined", by = "spp", backend = "marginaleffects"), zap_small = TRUE, table_width = Inf)) # nolint
})


test_that("estimate_contrasts - contrasts for numeric by factor", {
  data(iris)
  model <- lm(Petal.Width ~ Petal.Length * Species, data = iris)
  out1 <- estimate_contrasts(model, contrast = "Petal.Length", by = "Species", backend = "marginaleffects") # nolint
  # validated against ggeffects::test_predictions()
  expect_equal(out1$Difference, c(-0.17076, 0.04095, 0.17076), tolerance = 1e-4)
  out2 <- marginaleffects::avg_slopes(
    model,
    variables = "Petal.Length",
    by = "Species",
    newdata = insight::get_datagrid(model, by = c("Petal.Length", "Species")),
    hypothesis = ~pairwise
  )
  expect_equal(out1$Difference, out2$estimate[4:6], tolerance = 1e-4)
})
