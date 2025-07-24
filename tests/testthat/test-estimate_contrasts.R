skip_on_cran()
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")
skip_on_os("mac")

test_that("estimate_contrasts - Frequentist, one factor", {
  data(iris)
  # One factor
  dat <<- iris
  model <- lm(Sepal.Width ~ Species, data = dat)

  estim <- suppressMessages(estimate_contrasts(model, backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 9L))
  expect_equal(estim$Difference, c(0.658, 0.454, -0.204), tolerance = 1e-4)

  estim <- suppressMessages(estimate_contrasts(model, backend = "marginaleffects"))
  expect_identical(dim(estim), c(3L, 9L))
  expect_equal(estim$Difference, c(-0.658, -0.454, 0.204), tolerance = 1e-4)

  # validate against new marginaleffects
  out <- marginaleffects::avg_predictions(
    model,
    by = "Species",
    newdata = insight::get_datagrid(model, "Species", factors = "all"),
    hypothesis = ~pairwise
  )
  expect_equal(out$estimate, estim$Difference, tolerance = 1e-4)

  estim <- suppressMessages(estimate_contrasts(model, by = "Species=c('versicolor', 'virginica')", backend = "emmeans"))
  expect_identical(dim(estim), c(1L, 9L))

  estim <- suppressMessages(estimate_contrasts(model, contrast = "Species=c('versicolor', 'virginica')", backend = "marginaleffects"))
  expect_identical(dim(estim), c(1L, 9L))
})


test_that("estimate_contrasts - Frequentist, two factors", {
  data(iris)
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
  expect_identical(dim(estim), c(3L, 10L))
})


test_that("estimate_contrasts - Frequentist, One factor and one continuous", {
  data(iris)
  # One factor and one continuous
  model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
  estim <- suppressMessages(estimate_contrasts(model, backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 9L))
  estim <- suppressMessages(estimate_contrasts(model, by = c("Species", "Petal.Width=0"), backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 10L))
  estim <- suppressMessages(estimate_contrasts(model, by = "Petal.Width", length = 4, backend = "emmeans"))
  expect_identical(dim(estim), c(12L, 10L))
  ## FIXME: currently errors
  # estim <- estimate_contrasts(model, contrast = "Species", by = "Petal.Width=0", backend = "marginaleffects")
  # expect_identical(dim(estim), c(3L, 9L))
  estim <- estimate_contrasts(model, contrast = "Petal.Width", by = "Species", length = 4, backend = "marginaleffects")
  expect_equal(estim$Difference, c(0.21646, -0.20579, -0.42224), tolerance = 1e-4)
})


test_that("estimate_contrasts - Frequentist, One factor and one continuous", {
  data(iris)
  # Contrast between continuous
  model <- lm(Sepal.Width ~ Petal.Length, data = iris)

  estim <- suppressMessages(estimate_contrasts(model, by = "Petal.Length=c(2.3, 3)", backend = "emmeans"))
  expect_identical(dim(estim), c(1L, 9L))
  estim <- suppressMessages(estimate_contrasts(model, by = "Petal.Length=c(2, 3, 4)", backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 9L))
  estim <- estimate_contrasts(model, contrast = "Petal.Length=c(2.3, 3)", backend = "marginaleffects")
  expect_identical(dim(estim), c(1L, 9L))
  expect_named(estim, c("Level1", "Level2", "Difference", "SE", "CI_low", "CI_high", "t", "df", "p"))
  expect_identical(as.character(estim$Level1), "3")
  estim <- estimate_contrasts(model, contrast = "Petal.Length=c(2, 3, 4)", backend = "marginaleffects")
  expect_named(estim, c("Level1", "Level2", "Difference", "SE", "CI_low", "CI_high", "t", "df", "p"))
  expect_identical(as.character(estim$Level1), c("3", "4", "4"))
  expect_identical(dim(estim), c(3L, 9L))
})


test_that("estimate_contrasts - Frequentist, Three factors 1", {
  data(mtcars)
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
  expect_identical(dim(estim), c(6L, 10L))
  expect_equal(estim$Difference, c(6.98333, 11.275, 18.25833, 4.29167, 11.275, 6.98333), tolerance = 1e-4)

  estim <- suppressMessages(estimate_contrasts(model, contrast = c("vs", "am"), by = "gear", backend = "marginaleffects"))
  expect_identical(dim(estim), c(18L, 10L))
  expect_named(estim, c("Level1", "Level2", "gear", "Difference", "SE", "CI_low", "CI_high", "t", "df", "p"))
  expect_equal(
    estim$Difference,
    c(
      6.98333, 5.28333, 12.26667, -1.7, 5.28333, 6.98333, 6.98333,
      7.03333, 14.01667, 0.05, 7.03333, 6.98333, 6.98333, 11.275, 18.25833,
      4.29167, 11.275, 6.98333
    ),
    tolerance = 1e-4
  )
  expect_snapshot(print(estimate_contrasts(model, contrast = c("vs", "am"), by = "gear", backend = "marginaleffects"), zap_small = TRUE, table_width = Inf)) # nolint
})


test_that("estimate_contrasts - Frequentist, Three factors 2", {
  data(efc, package = "modelbased")
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
  levels(efc$c172code) <- c("low", "mid", "high")
  fit <- lm(neg_c_7 ~ barthtot + e16sex * c172code * c161sex, data = efc)
  estim <- estimate_contrasts(fit, contrast = c("c161sex", "c172code"), by = "e16sex='male'", backend = "marginaleffects")

  # validate against marginaleffects
  out <- marginaleffects::avg_predictions(
    fit,
    newdata = datawizard::data_arrange(
      as.data.frame(insight::get_datagrid(fit, by = c("c161sex", "c172code", "e16sex"), factors = "all")),
      c("c161sex", "c172code", "e16sex")
    ),
    by = c("c161sex", "c172code", "e16sex"),
    hypothesis = ~ pairwise | e16sex
  )
  expect_equal(out$estimate[out$e16sex == "male"], estim$Difference, tolerance = 1e-4)

  expect_identical(dim(estim), c(15L, 10L))
  expect_equal(
    estim$Difference,
    c(
      2.70334, 2.23511, 1.55845, 2.48322, 2.48543, -0.46823, -1.14489,
      -0.22012, -0.21791, -0.67666, 0.24811, 0.25032, 0.92477, 0.92698,
      0.00221
    ),
    tolerance = 1e-4
  )
  expect_true(all(estim$e16sex == "male"))
  expect_identical(
    as.character(estim$Level1),
    c(
      "Male, mid", "Male, high", "Female, low", "Female, mid", "Female, high",
      "Male, high", "Female, low", "Female, mid", "Female, high", "Female, low",
      "Female, mid", "Female, high", "Female, mid", "Female, high",
      "Female, high"
    )
  )
  expect_identical(
    as.character(estim$Level2),
    c(
      "Male, low", "Male, low", "Male, low", "Male, low", "Male, low",
      "Male, mid", "Male, mid", "Male, mid", "Male, mid", "Male, high",
      "Male, high", "Male, high", "Female, low", "Female, low", "Female, mid"
    )
  )
})


test_that("estimate_contrasts - Frequentist, duplicated levels", {
  data(mtcars)
  # duplicated levels
  dat <- mtcars
  dat[c("vs", "am")] <- sapply(dat[c("vs", "am")], as.factor)
  set.seed(123)
  dat$three <- factor(sample(0:1, nrow(dat), replace = TRUE))
  model <- lm(mpg ~ three * vs * am, data = dat)
  expect_snapshot(print(estimate_contrasts(model, contrast = c("three", "vs", "am"), backend = "marginaleffects"), digits = 1, zap_small = TRUE, table_width = Inf), variant = "windows") # nolint
  expect_snapshot(print(estimate_contrasts(model, contrast = "am", backend = "marginaleffects"), zap_small = TRUE, table_width = Inf), variant = "windows") # nolint


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
})


test_that("estimate_contrasts - Frequentist, Mixed models", {
  skip_if_not_installed("logspline")
  skip_if_not_installed("lme4")
  data(iris)
  # Mixed models
  data <- iris
  data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")

  model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)
  estim <- suppressMessages(estimate_contrasts(model, backend = "emmeans"))
  expect_identical(dim(estim), c(3L, 9L))
})


test_that("estimate_contrasts - Frequentist, GLM", {
  data(iris)
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
  expect_identical(estim3$Difference, estim1$Difference * -1)
  estim4 <- suppressWarnings(suppressMessages(estimate_contrasts(model, predict = "link", backend = "marginaleffects")))
  expect_identical(estim4$Difference, estim2$Difference * -1)

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
      -0.05025, -0.89132, -0.52141, -0.27182, -0.45085, -0.83305,
      -0.47175, 0.00022, -0.45184, 0.36936, 0.60636, 0.43568, 0.20898,
      0.068, -0.18029
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

  model <- lm(Petal.Width ~ Species, data = iris)
  expect_error(
    estimate_contrasts(model, p_adjust = "scheffe"),
    regex = "is only available when"
  )
  expect_silent(estimate_contrasts(
    model,
    contrast = "Species",
    p_adjust = "scheffe",
    backend = "emmeans"
  ))

  skip_if_not_installed("mvtnorm")
  dat <- iris
  dat$fac <- ifelse(dat$Sepal.Length < 5.8, "A", "B")
  model <- lm(Sepal.Width ~ Species * fac, data = dat)
  set.seed(123)
  out <- estimate_contrasts(model, c("Species", "fac"), p_adjust = "sup-t")
  expect_equal(
    out$p,
    c(
      0.44686, 0, 0, 0.00048, 0, 0.00086, 0.0086, 0.00328, 0.02718,
      0.10619, 0.99998, 0.00037, 0.71917, 0.51122, 0.28497
    ),
    tolerance = 1e-3
  )
  expect_equal(
    out$CI_low,
    c(
      -0.3461, -1.02306, -0.76301, -1.36376, -0.60638, -2.30875,
      -2.06719, -2.46281, -1.93229, -0.0281, -0.60143, 0.12295, -0.82718,
      -0.08844, -0.15023
    ),
    tolerance = 1e-3
  )

  skip_if_not_installed("glmmTMB")
  d <- glmmTMB::Salamanders
  model <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    ziformula = ~mined,
    family = poisson,
    data = d
  ))
  set.seed(123)
  out <- head(estimate_contrasts(model, "spp", by = "mined", p_adjust = "sup-t"))
  expect_equal(
    out$p,
    c(0.00259, 0.59628, 0.18012, 0.00475, 0.00674, 0.99467),
    tolerance = 1e-3
  )
  expect_equal(
    out$CI_low,
    c(-0.29028, -0.04581, -0.21504, 0.04106, 0.03308, -0.0816),
    tolerance = 1e-3
  )
})


test_that("estimate_contrasts - ratios", {
  data(iris)
  model <- lm(Petal.Width ~ Species, data = iris)
  estim <- estimate_contrasts(model, "Species", comparison = ratio ~ pairwise, backend = "marginaleffects")
  expect_equal(estim$Ratio, c(5.39024, 8.23577, 1.5279), tolerance = 1e-4)
  expect_identical(dim(estim), c(3L, 9L))
  estim2 <- marginaleffects::avg_predictions(model, by = "Species", hypothesis = ratio ~ pairwise)
  expect_equal(estim$Ratio, estim2$estimate, tolerance = 1e-4, ignore_attr = TRUE)
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


test_that("estimate_contrasts - marginaleffects, comparisons, validate against predict", {
  skip_if_not_installed("Formula")
  data(coffee_data, package = "modelbased")
  m <- lm(alertness ~ time * coffee + sex, data = coffee_data)
  expect_snapshot(print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects", p_adjust = "none"), zap_small = TRUE, table_width = Inf)) # nolint
  expect_snapshot(print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects", p_adjust = "none", comparison = ratio ~ reference | coffee), zap_small = TRUE, table_width = Inf)) # nolint
  expect_snapshot(print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects", p_adjust = "none", comparison = "(b2-b1)=(b4-b3)"), zap_small = TRUE, table_width = Inf)) # nolint
  out1 <- estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects", p_adjust = "none", comparison = "(b2-b1)=(b4-b3)")
  out2 <- predict(m, newdata = insight::get_datagrid(m, c("time", "coffee")))
  expect_equal(out1$Difference, 5.78298, tolerance = 1e-4)
  expect_equal(out1$Difference, ((out2[2] - out2[1]) - (out2[4] - out2[3])), tolerance = 1e-4, ignore_attr = TRUE)
  out1 <- estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects", p_adjust = "none", comparison = "b5=b3")
  expect_equal(out1$Difference, -1.927659, tolerance = 1e-4)
  expect_equal(out1$Difference, (out2[5] - out2[3]), tolerance = 1e-4, ignore_attr = TRUE)
  expect_snapshot(print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects", p_adjust = "none", comparison = "b5=b3"), zap_small = TRUE, table_width = Inf)) # nolint

  # validated against ggeffects::test_predictions()
  data(efc, package = "modelbased")
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
  fit <- lm(neg_c_7 ~ c12hour + barthtot + c161sex + e42dep * c172code, data = efc)
  expect_snapshot(estimate_contrasts(fit, c("e42dep", "c172code"), comparison = "b6-b3=0", backend = "marginaleffects"))
  out <- estimate_contrasts(fit, c("e42dep", "c172code"), comparison = "b6-b3=0", backend = "marginaleffects")
  expect_equal(out$Difference, -0.3352769, tolerance = 1e-4)
})


test_that("estimate_contrasts - marginaleffects vs emmeans", {
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

  # validate against emmeans
  out_emm <- emmeans::emmeans(model, "Species", type = "response")
  out_emm <- emmeans::regrid(out_emm)
  out6 <- as.data.frame(emmeans::contrast(out_emm, method = "pairwise"))
  expect_equal(out6$estimate, out1$Difference, tolerance = 1e-3)

  # validate against marginaleffects
  out7 <- marginaleffects::avg_predictions(model, by = "Species", hypothesis = ~pairwise)
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
  expect_equal(out1$Difference, out2$Difference * -1, tolerance = 1e-4) # swicthed sign

  mtcars2 <- mtcars
  mtcars2$cyl <- as.factor(mtcars2$cyl)
  model <- lm(mpg ~ cyl + wt * hp, mtcars2)
  out3 <- estimate_contrasts(model, backend = "emmeans")
  out4 <- estimate_contrasts(model, contrast = "cyl", backend = "marginaleffects")

  expect_identical(nrow(out3), 3L)
  expect_identical(nrow(out4), 3L)
  expect_equal(out3$Difference, out4$Difference * -1, tolerance = 1e-4) # switched sign
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
  expect_equal(out4$Difference * -1, as.data.frame(out5$contrasts)$estimate, tolerance = 1e-3)
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
  expect_equal(out$Difference, c(0.35, -0.8, -0.35, -1.15, -0.7, 0.45), tolerance = 1e-3)
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


test_that("estimate_contrasts - contrasts for numeric by factor 1", {
  data(iris)
  model <- lm(Petal.Width ~ Petal.Length * Species, data = iris)
  out1 <- estimate_contrasts(model, contrast = "Petal.Length", by = "Species", backend = "marginaleffects") # nolint
  # validated against ggeffects::test_predictions()
  expect_equal(out1$Difference, c(0.12981, -0.04095, -0.17076), tolerance = 1e-4)
  out2 <- marginaleffects::avg_slopes(
    model,
    variables = "Petal.Length",
    by = "Species",
    newdata = insight::get_datagrid(model, by = c("Petal.Length", "Species")),
    hypothesis = ~pairwise
  )
  expect_equal(out1$Difference, out2$estimate, tolerance = 1e-4)
})


test_that("estimate_contrasts - contrasts for numeric by factor 2", {
  data(efc, package = "modelbased")
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex"))
  levels(efc$c172code) <- c("low", "mid", "high")
  fit <- lm(neg_c_7 ~ e16sex + c161sex * c172code, data = efc)
  # all should return the same output
  out1 <- estimate_contrasts(fit, "c161sex", by = "c172code", backend = "marginaleffects")
  out2 <- estimate_contrasts(fit, c("c161sex", "c172code"), comparison = ~ pairwise | c172code, backend = "marginaleffects")
  out3 <- estimate_contrasts(fit, "c161sex", by = "c172code", comparison = ~pairwise, backend = "marginaleffects")
  expect_named(
    out1,
    c("Level1", "Level2", "c172code", "Difference", "SE", "CI_low", "CI_high", "t", "df", "p")
  )
  expect_named(
    out2,
    c("Level1", "Level2", "c172code", "Difference", "SE", "CI_low", "CI_high", "t", "df", "p")
  )
  expect_named(
    out3,
    c("Level1", "Level2", "c172code", "Difference", "SE", "CI_low", "CI_high", "t", "df", "p")
  )
  expect_identical(dim(out1), c(3L, 10L))
  expect_identical(dim(out2), c(3L, 10L))
  expect_identical(dim(out3), c(3L, 10L))
  expect_equal(out1$Difference, c(1.04585, 0.56041, 0.95798), tolerance = 1e-4)
  expect_equal(out2$Difference, c(1.04585, 0.56041, 0.95798), tolerance = 1e-4)
  expect_equal(out3$Difference, c(1.04585, 0.56041, 0.95798), tolerance = 1e-4)
})


test_that("estimate_contrasts - by with _ in variable name works", {
  data(efc, package = "modelbased")
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex"))
  levels(efc$c172code) <- c("low", "mid", "high")
  efc$edu_cation <- efc$c172code
  m1 <- lm(neg_c_7 ~ e16sex + c161sex * edu_cation, data = efc)
  m2 <- lm(neg_c_7 ~ e16sex + c161sex * c172code, data = efc)
  out1 <- estimate_contrasts(m1, "c161sex", by = "edu_cation", backend = "marginaleffects")
  out2 <- estimate_contrasts(m2, "c161sex", by = "c172code", backend = "marginaleffects")
  expect_named(
    out1,
    c("Level1", "Level2", "edu_cation", "Difference", "SE", "CI_low", "CI_high", "t", "df", "p")
  )
  expect_named(
    out2,
    c("Level1", "Level2", "c172code", "Difference", "SE", "CI_low", "CI_high", "t", "df", "p")
  )
  expect_identical(dim(out1), dim(out2))
  expect_equal(out1$Difference, out2$Difference, tolerance = 1e-4)
})


test_that("estimate_contrasts - row order in data grid doesn't matter", {
  # see https://github.com/vincentarelbundock/marginaleffects/issues/1374
  set.seed(123)
  n <- 200
  d <- data.frame(
    score = rnorm(n),
    grp = as.factor(sample(c("treatment", "control"), n, TRUE)),
    time = as.factor(sample(1:3, n, TRUE))
  )

  model2 <- lm(score ~ grp * time, data = d)
  out1 <- estimate_contrasts(model2, "grp", by = "time", backend = "marginaleffects")
  out2 <- estimate_contrasts(model2, "time", by = "grp", backend = "marginaleffects")

  expect_identical(dim(out1), c(3L, 10L))
  expect_identical(dim(out2), c(6L, 10L))
  expect_identical(as.character(out1$Level1), c("treatment", "treatment", "treatment"))
  expect_identical(as.character(out2$Level1), c("2", "3", "3", "2", "3", "3"))
  expect_equal(out1$Difference, c(-0.41835, -0.05537, -0.05027), tolerance = 1e-4)
  expect_equal(out2$Difference, c(0.2036, -0.07087, -0.27447, 0.56658, 0.29721, -0.26937), tolerance = 1e-4)
})


test_that("estimate_contrasts - filtering in `by` works", {
  data(efc, package = "modelbased")
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
  levels(efc$c172code) <- c("low", "mid", "high")
  fit <- lm(neg_c_7 ~ c12hour + barthtot + c161sex + e42dep * c172code, data = efc)

  out1 <- estimate_contrasts(fit, "c172code", by = "e42dep")
  out2 <- estimate_contrasts(fit, "c172code", by = "e42dep='slightly dependent'")
  expect_identical(dim(out1), c(12L, 10L))
  expect_identical(dim(out2), c(3L, 10L))
  expect_equal(out1$Difference[4:6], out2$Difference, tolerance = 1e-4)
})


test_that("estimate_contrasts - examples from docs work as intendec", {
  model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
  expect_snapshot(print(estimate_contrasts(model, contrast = "Petal.Width", by = "Species"), zap_small = TRUE, table_width = Inf)) # nolint
  expect_snapshot(print(estimate_contrasts(model, contrast = c("Species", "Petal.Width"), length = 2), zap_small = TRUE, table_width = Inf)) # nolint
  expect_snapshot(print(estimate_contrasts(model, contrast = c("Species", "Petal.Width=c(1, 2)")), zap_small = TRUE, table_width = Inf)) # nolint
  expect_snapshot(print(estimate_contrasts(model, by = "Petal.Width", length = 4), zap_small = TRUE, table_width = Inf)) # nolint
})


test_that("estimate_contrasts - test all combinations of contrast and by, with filtering", {
  # see https://github.com/vincentarelbundock/marginaleffects/issues/1374
  set.seed(123)
  n <- 1000
  d <- data.frame(
    score = rnorm(n),
    grp = as.factor(sample(c("treatment", "control"), n, TRUE)),
    time = as.factor(sample(1:2, n, TRUE)),
    x = as.factor(sample(letters[1:2], n, TRUE))
  )
  model2 <- lm(score ~ grp * time * x, data = d)

  expect_snapshot(print(estimate_contrasts(model2, c("grp", "time", "x")), zap_small = TRUE, table_width = Inf)) # nolint
  expect_snapshot(print(estimate_contrasts(model2, c("grp", "time"), by = "x"), zap_small = TRUE, table_width = Inf)) # nolint
  expect_snapshot(print(estimate_contrasts(model2, "grp", by = c("time", "x")), zap_small = TRUE, table_width = Inf)) # nolint
  expect_snapshot(print(estimate_contrasts(model2, "grp", by = "time"), zap_small = TRUE, table_width = Inf)) # nolint
  expect_snapshot(print(estimate_contrasts(model2, c("grp", "time", "x='a'")), zap_small = TRUE, table_width = Inf)) # nolint
  expect_snapshot(print(estimate_contrasts(model2, c("grp", "time=1"), by = "x"), zap_small = TRUE, table_width = Inf)) # nolint
  expect_snapshot(print(estimate_contrasts(model2, "grp", by = c("time", "x='a'")), zap_small = TRUE, table_width = Inf)) # nolint

  set.seed(123)
  n <- 1000
  d <- data.frame(
    score = rnorm(n),
    grp = as.factor(sample(c("treatment", "control"), n, TRUE)),
    time = as.factor(sample(1:3, n, TRUE))
  )
  model2 <- lm(score ~ grp * time, data = d)

  expect_snapshot(print(estimate_contrasts(model2, "time=c(1,2)", by = "grp"), zap_small = TRUE, table_width = Inf)) # nolint
  expect_snapshot(print(estimate_contrasts(model2, c("grp", "time=2")), zap_small = TRUE, table_width = Inf)) # nolint
})


test_that("estimate_contrast, full averaging", {
  data(efc, package = "modelbased")
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
  levels(efc$c172code) <- c("low", "mid", "high")
  m <- lm(neg_c_7 ~ c12hour + barthtot + e42dep + c161sex * c172code, data = efc)

  out <- estimate_contrasts(m, "c161sex", by = "c172code", estimate = "average")
  expect_equal(out$Difference, c(1.09591, 0.68736, 0.92224), tolerance = 1e-4)
})


test_that("estimate_contrast, slopes with emmeans", {
  data(iris)
  model <- lm(Petal.Width ~ Petal.Length * Species, data = iris)
  out <- estimate_contrasts(
    model,
    contrast = "Petal.Length",
    by = "Species",
    backend = "emmeans"
  )
  expect_identical(dim(out), c(3L, 9L))
  expect_equal(out$Difference, c(-0.12981, 0.04095, 0.17076), tolerance = 1e-4)
  expect_identical(as.character(out$Level1), c("setosa", "setosa", "versicolor"))
})


test_that("estimate_contrast, slopes with emmeans", {
  set.seed(123)
  dat <- data.frame(
    outcome = rbinom(n = 100, size = 1, prob = 0.35),
    var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.2)),
    var_cont = rnorm(n = 100, mean = 10, sd = 7)
  )
  dat$var_cont <- datawizard::standardize(dat$var_cont)

  m1 <- glm(
    outcome ~ var_binom + var_cont,
    data = dat,
    family = binomial(link = "logit")
  )

  # range of values
  out <- estimate_contrasts(
    m1,
    c("var_binom", "var_cont"),
    predict = "link",
    transform = exp,
    length = 3
  )
  expect_snapshot(print(out, table_width = Inf))
  expect_identical(
    as.character(out$Level1),
    c(
      "0, 0.725", "0, 3.463", "1, -2.012", "1, 0.725", "1, 3.463",
      "0, 3.463", "1, -2.012", "1, 0.725", "1, 3.463", "1, -2.012",
      "1, 0.725", "1, 3.463", "1, 0.725", "1, 3.463", "1, 3.463"
    )
  )

  out <- estimate_contrasts(
    m1,
    c("var_binom", "var_cont=[sd]"),
    predict = "link",
    transform = exp
  )
  expect_identical(
    as.character(out$Level1),
    c(
      "var_binom 0, var_cont 0", "var_binom 0, var_cont 1", "var_binom 1, var_cont -1",
      "var_binom 1, var_cont 0", "var_binom 1, var_cont 1", "var_binom 0, var_cont 1",
      "var_binom 1, var_cont -1", "var_binom 1, var_cont 0", "var_binom 1, var_cont 1",
      "var_binom 1, var_cont -1", "var_binom 1, var_cont 0", "var_binom 1, var_cont 1",
      "var_binom 1, var_cont 0", "var_binom 1, var_cont 1", "var_binom 1, var_cont 1"
    )
  )
})


test_that("estimate_contrast, filter by numeric values", {
  skip_if_not_installed("lme4")
  data(iris)
  mod <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
  out1 <- estimate_contrasts(mod, contrast = "Species=", by = "Petal.Width=c(1,2,3)", backend = "marginaleffects")
  out2 <- estimate_contrasts(mod, contrast = "Species=", by = "Petal.Width=c(1,2,3)", backend = "emmeans")
  expect_identical(dim(out1), c(9L, 10L))
  expect_identical(dim(out2), c(9L, 10L))
  expect_equal(
    out1$Difference,
    c(-0.23635, 0.2129, 0.44924, 0.25985, -0.06644, -0.32629, 0.75604, -0.34579, -1.10183),
    tolerance = 1e-4
  )
  expect_equal(
    out2$Difference,
    c(0.23635, -0.25985, -0.75604, -0.2129, 0.06644, 0.34579, -0.44924, 0.32629, 1.10183),
    tolerance = 1e-4
  )

  out1 <- estimate_contrasts(mod, contrast = "Species=c('versicolor','setosa')", by = "Petal.Width=c(1,2,3)", backend = "marginaleffects")
  out2 <- estimate_contrasts(mod, contrast = "Species=c('versicolor','setosa')", by = "Petal.Width=c(1,2,3)", backend = "emmeans")
  expect_identical(dim(out1), c(3L, 10L))
  expect_identical(dim(out2), c(3L, 10L))
  expect_equal(out1$Difference, -1 * out2$Difference, tolerance = 1e-4)

  data(CO2)
  mod <- suppressWarnings(lme4::lmer(uptake ~ conc * Plant + (1 | Type), data = CO2))
  out1 <- estimate_contrasts(mod, contrast = "Plant", by = "conc=c(100,200)", backend = "marginaleffects")
  out2 <- estimate_contrasts(mod, contrast = "Plant", by = "conc=c(100,200)", backend = "emmeans")
  expect_identical(dim(out1), c(132L, 10L))
  expect_identical(dim(out2), c(132L, 10L))

  out1 <- estimate_contrasts(mod, contrast = "Plant=c('Qn1','Qn2','Qn3')", by = "conc=c(100,200)", backend = "marginaleffects")
  out2 <- estimate_contrasts(mod, contrast = "Plant=c('Qn1','Qn2','Qn3')", by = "conc=c(100,200)", backend = "emmeans")
  expect_identical(dim(out1), c(6L, 10L))
  expect_identical(dim(out2), c(6L, 10L))
  expect_equal(out1$Difference[c(1, 6)], -1 * out2$Difference[c(1, 6)], tolerance = 1e-4)

  out1 <- estimate_contrasts(mod, contrast = "Plant=c('Qn1','Qn2','Qn3')", backend = "marginaleffects")
  out2 <- estimate_contrasts(mod, contrast = "Plant=c('Qn1','Qn2','Qn3')", comparison = "b1=b2", backend = "marginaleffects")
  expect_equal(out1$Difference[1], -1 * out2$Difference, tolerance = 1e-4)

  out1 <- estimate_contrasts(mod, contrast = "conc", by = "Plant=c('Mc2','Mn1','Qn3')")
  expect_equal(out1$Difference, c(0.01746, 0.01782, 0.00036), tolerance = 1e-3)

  out <- estimate_contrasts(mod, contrast = "conc", by = "Plant=c('Mc2','Mn1','Qn3')", comparison = "b1=b2")
  expect_equal(out$Difference, -0.01745914, tolerance = 1e-4)
})


test_that("estimate_contrast, filterin in `by` and `contrast`", {
  data(efc, package = "modelbased")
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
  levels(efc$c172code) <- c("low", "mid", "high")
  m <- lm(neg_c_7 ~ barthtot + c172code * e42dep + c161sex, data = efc)

  out <- estimate_contrasts(m, c("e42dep", "c172code"))
  expect_identical(dim(out), c(66L, 9L))

  out <- estimate_contrasts(
    m,
    "e42dep=c('independent','slightly dependent','moderately dependent')",
    by = "c172code"
  )
  expect_identical(dim(out), c(9L, 10L))
  expect_equal(
    out$Difference,
    c(
      -0.77851, 0.12142, 0.89993, 0.87674, 1.97996, 1.10322, 2.69591,
      2.59613, -0.09978
    ),
    tolerance = 1e-4
  )

  out <- estimate_contrasts(
    m,
    "e42dep=c('independent','slightly dependent','moderately dependent')",
    by = "c172code",
    comparison = "b1=b4"
  )
  expect_equal(out$Difference, 1.163197, tolerance = 1e-4)

  out <- estimate_contrasts(m, "e42dep", by = "c172code=c('low','mid')")
  expect_identical(dim(out), c(12L, 10L))
})


test_that("estimate_contrast, don't calculate slopes for integers", {
  data(mtcars)
  m <- lm(mpg ~ hp + gear, data = mtcars)
  expect_silent(estimate_contrasts(m, "gear"))
  out <- estimate_contrasts(m, "gear")
  expect_identical(dim(out), c(3L, 9L))

  expect_error(
    estimate_contrasts(m, "hp"),
    regex = "Please specify"
  )
  out <- estimate_contrasts(m, "hp", by = "gear")
  expect_identical(dim(out), c(3L, 9L))
})


test_that("estimate_contrast, informative error when `by` and `contrast` are the same", {
  data(iris)
  m <- lm(Petal.Length ~ Species, data = iris)
  expect_error(
    estimate_contrasts(m, "Species = 'versicolor'", by = "Species = 'setosa'"),
    regex = "You cannot"
  )
})


test_that("estimate_contrast, works with aov (when no statistic is extracted)", {
  skip_if(getRversion() < "4.5.0")
  data(penguins)
  fit <- aov(
    formula = body_mass ~ species,
    data = penguins
  )

  out1 <- marginaleffects::avg_predictions(
    fit,
    by = "species",
    hypothesis = ~pairwise
  )

  out2 <- estimate_contrasts(
    model = fit,
    contrast = "species",
    backend = "marginaleffects"
  )

  expect_equal(out1$estimate, out2$Difference, tolerance = 1e-4)
  expect_identical(out2$df, c(339L, 339L, 339L))

  out3 <- estimate_contrasts(
    model = fit,
    contrast = "species",
    df = Inf,
    backend = "marginaleffects"
  )
  expect_equal(out1$p.value, out3$p, tolerance = 1e-4)
})


test_that("estimate_contrast, marginal effects inequalities", {
  skip_if(getRversion() < "4.5.0")
  skip_if_not_installed("datawizard")
  data(penguins)
  penguins$long_bill <- factor(datawizard::categorize(penguins$bill_len), labels = c("short", "long"))

  m <- glm(long_bill ~ species + island + bill_dep, data = penguins, family = "binomial")

  out <- estimate_contrasts(m, "species", comparison = "inequality")
  expect_equal(out[["Mean Difference"]], 0.6381, tolerance = 1e-4)
  expect_identical(attributes(out)$table_title, c("Marginal Inequality Analysis", "blue"))

  expect_error(
    estimate_contrasts(m, "species", comparison = "inequality_pairwise"),
    regex = "Pairwise comparisons require"
  )

  out <- estimate_contrasts(m, c("species", "island"), comparison = "inequality")
  expect_equal(out[["Mean Difference"]], c(0.23043, 0.6381), tolerance = 1e-4)
  expect_identical(out$Parameter, c("island", "species"))

  out <- estimate_contrasts(m, "species", by = "island", comparison = "inequality")
  expect_equal(out[["Mean Difference"]], c(0.66259, 0.60411, 0.64052), tolerance = 1e-4)
  expect_named(out, c("island", "Mean Difference", "SE", "CI_low", "CI_high", "z", "p"))

  out <- estimate_contrasts(m, c("species", "island"), comparison = "inequality_pairwise")
  expect_equal(out[["Mean Difference"]], -0.4076682, tolerance = 1e-4, ignore_attr = TRUE)
  expect_identical(out$Parameter, "island - species")

  out <- estimate_contrasts(m, "species", by = "island", comparison = "inequality_pairwise")
  expect_equal(out[["Mean Difference"]], c(0.05848, 0.02207, -0.03641), tolerance = 1e-4, ignore_attr = TRUE)
  expect_named(out, c("Parameter", "Mean Difference", "SE", "CI_low", "CI_high", "z", "p"))
  expect_identical(out$Parameter, c("Biscoe - Dream", "Biscoe - Torgersen", "Dream - Torgersen"))

  expect_error(
    estimate_contrasts(m, c("species", "bill_dep"), comparison = "inequality"),
    regex = "All variables specified"
  )

  m <- glm(long_bill ~ species + island + sex + bill_dep, data = penguins, family = "binomial")
  expect_error(
    estimate_contrasts(m, "species", by = c("island", "sex"), comparison = "inequality"),
    regex = "can only contain one variable"
  )
})


test_that("estimate_contrast, slopes with different estimate options", {
  skip_if(getRversion() < "4.5.0")
  skip_if_not_installed("datawizard")
  data(penguins)
  penguins$long_bill <- factor(datawizard::categorize(penguins$bill_len), labels = c("short", "long"))

  m <- glm(long_bill ~ species + island * bill_dep, data = penguins, family = "binomial")

  out <- estimate_contrasts(m, "bill_dep", by = "island")
  expect_equal(out$Difference, c(0.08507, -0.00071, -0.08578), tolerance = 1e-4)

  out <- estimate_contrasts(m, "bill_dep", by = "island", estimate = "average")
  expect_equal(out$Difference, c(-0.05295, -0.07655, -0.0236), tolerance = 1e-4)
})
