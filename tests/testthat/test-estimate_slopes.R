skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

test_that("estimate_slopes", {
  data(iris)
  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

  expect_message(estimate_slopes(model, backend = "emmeans"), regex = "No numeric")
  estim1 <- suppressMessages(estimate_slopes(model, backend = "emmeans"))
  estim2 <- suppressMessages(estimate_slopes(model, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(1L, 8L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 1e-4)

  # aliases
  estim1 <- estimate_slopes(model, slope = "Petal.Length")
  # "trend" is an alias, we expect no message or warning here
  expect_silent({
    estim2 <- estimate_slopes(model, trend = "Petal.Length")
  })
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 1e-4)
  # if both "slope" and "trend" are provided, warn
  expect_warning(
    {
      estim3 <- estimate_slopes(model, slope = "Petal.Length", trend = "Species")
    },
    regex = "Both `slope` and `trend` were provided",
    fixed = TRUE
  )
  expect_equal(estim1$Slope, estim3$Slope, tolerance = 1e-4)

  estim1 <- suppressMessages(estimate_slopes(model, by = "Species", backend = "emmeans"))
  estim2 <- suppressMessages(estimate_slopes(
    model,
    by = "Species",
    backend = "marginaleffects"
  ))
  expect_identical(dim(estim1), c(3L, 9L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 1e-4)

  estim1 <- suppressMessages(estimate_slopes(
    model,
    by = "Petal.Length",
    backend = "emmeans"
  ))
  estim2 <- suppressMessages(estimate_slopes(
    model,
    by = "Petal.Length",
    backend = "marginaleffects"
  ))
  expect_identical(dim(estim1), c(10L, 9L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 0.2)

  estim1 <- suppressMessages(estimate_slopes(
    model,
    by = c("Species", "Petal.Length"),
    backend = "emmeans"
  ))
  expect_identical(dim(estim1), c(30L, 10L))

  estim2 <- suppressMessages(estimate_slopes(
    model,
    by = c("Species", "Petal.Length"),
    preserve_range = FALSE,
    backend = "marginaleffects"
  ))
  expect_identical(dim(estim2), c(30L, 9L))
  expect_equal(
    estim1$Slope,
    estim2$Slope[order(estim2$Petal.Length, estim2$Species)],
    tolerance = 1e-3
  )

  # test different DF
  estim1 <- suppressMessages(estimate_slopes(
    model,
    by = "Petal.Length",
    backend = "marginaleffects"
  ))
  estim2 <- suppressMessages(estimate_slopes(
    model,
    by = "Petal.Length",
    df = Inf,
    backend = "marginaleffects"
  ))
  expect_named(
    estim1,
    c("Petal.Length", "Slope", "SE", "CI_low", "CI_high", "t", "df", "p")
  )
  expect_named(estim2, c("Petal.Length", "Slope", "SE", "CI_low", "CI_high", "z", "p"))

  # suppress CI
  out <- suppressMessages(estimate_slopes(model, ci = NULL))
  expect_named(out, "Slope")
  out <- suppressMessages(estimate_slopes(model, ci = NA))
  expect_named(out, "Slope")

  model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)

  estim1 <- suppressMessages(estimate_slopes(
    model,
    by = "Sepal.Width",
    backend = "emmeans"
  ))
  estim2 <- suppressMessages(estimate_slopes(
    model,
    by = "Sepal.Width",
    backend = "marginaleffects"
  ))
  expect_identical(dim(estim1), c(10L, 9L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 0.2)
  estim1 <- suppressMessages(estimate_slopes(
    model,
    by = "Sepal.Width",
    length = 5,
    backend = "emmeans"
  ))
  estim2 <- suppressMessages(estimate_slopes(
    model,
    by = "Sepal.Width",
    length = 5,
    backend = "marginaleffects"
  ))
  expect_identical(dim(estim1), c(5L, 9L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 0.2)
  estim1 <- suppressMessages(estimate_slopes(
    model,
    by = "Sepal.Width = c(1, 2, 3)",
    backend = "emmeans"
  ))
  estim2 <- suppressMessages(estimate_slopes(
    model,
    by = "Sepal.Width = c(1, 2, 3)",
    backend = "marginaleffects"
  ))
  # test alternative list-notation
  estim3 <- suppressMessages(estimate_slopes(
    model,
    by = list(Sepal.Width = c(1, 2, 3)),
    backend = "emmeans"
  ))
  expect_identical(dim(estim1), c(3L, 9L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 0.2)
  expect_equal(estim3$Slope, estim2$Slope, tolerance = 0.2)
})


test_that("estimate_slopes, errors for emmeans when trend is non-numeric", {
  skip_if_not_installed("emmeans")
  data(iris)
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
  expect_error(
    estimate_slopes(model, trend = "Species", by = "Sepal.Width", backend = "emmeans"),
    regex = "Variable `Species` is not numeric",
    fixed = TRUE
  )
  model <- lm(Sepal.Length ~ Species, data = iris)
  expect_error(
    estimate_slopes(model, backend = "emmeans"),
    regex = "Model contains no numeric predictor",
    fixed = TRUE
  )
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
  expect_message(
    estimate_slopes(model, backend = "emmeans"),
    regex = "No numeric variable was specified",
    fixed = TRUE
  )
})


test_that("estimate_slopes, johnson-neyman p-adjust", {
  data(iris)
  model <- lm(Sepal.Width ~ Petal.Width * Petal.Length, data = iris)
  out <- estimate_slopes(model, "Petal.Width", by = "Petal.Length")
  expect_equal(
    out$CI_low,
    c(
      -0.83863,
      -0.66978,
      -0.50396,
      -0.34188,
      -0.18459,
      -0.03252,
      0.11384,
      0.25426,
      0.38899,
      0.51855
    ),
    tolerance = 1e-2
  )
  expect_equal(
    out$p,
    c(0.00664, 0.03956, 0.20019, 0.70529, 0.52501, 0.08553, 0.00496, 0.00013, 0, 0),
    tolerance = 1e-2
  )

  out <- estimate_slopes(model, "Petal.Width", by = "Petal.Length", p_adjust = "esarey")
  expect_equal(
    out$CI_low,
    c(
      -0.89944,
      -0.72628,
      -0.55667,
      -0.39145,
      -0.23184,
      -0.07835,
      0.06843,
      0.20825,
      0.3414,
      0.46848
    ),
    tolerance = 1e-2
  )
  expect_equal(
    out$p,
    c(0.03438, 0.14813, 0.50393, 0.83427, 0.24689, 0.03326, 0.00219, 9e-05, 0, 0),
    tolerance = 1e-2
  )

  skip_if_not_installed("mvtnorm")
  set.seed(123)
  out <- estimate_slopes(model, "Petal.Width", by = "Petal.Length", p_adjust = "sup-t")
  expect_equal(
    out$CI_low,
    c(
      -0.90003,
      -0.72665,
      -0.55721,
      -0.39196,
      -0.23229,
      -0.07873,
      0.06797,
      0.20793,
      0.34088,
      0.468
    ),
    tolerance = 1e-2
  )
  expect_equal(
    out$p,
    c(0.01765, 0.08691, 0.36222, 0.9097, 0.76792, 0.17275, 0.01369, 5e-04, 1e-05, 0),
    tolerance = 1e-2
  )
})


test_that("estimate_slopes, custom comparison", {
  data(iris)
  m <- lm(Sepal.Width ~ Sepal.Length * Species, data = iris)
  out <- estimate_contrasts(
    m,
    "Sepal.Length",
    by = "Species",
    comparison = "(b1 - b2) = (b1 - b3)"
  )
  expect_identical(dim(out), c(1L, 8L))
  expect_equal(out$Difference, -0.08782885, tolerance = 1e-4)
})


test_that("estimate_slopes, p_adjust methods adjust the p-values", {
  d <- datasets::mtcars
  d$safe_var <- factor(d$cyl)
  calls <- list(
    numeric_by = function(m) {
      estimate_slopes(
        lm(mpg ~ wt * hp, data = d),
        "wt",
        by = "hp",
        length = 4,
        p_adjust = m
      )
    },
    factor_by = function(m) {
      estimate_slopes(lm(mpg ~ wt * safe_var, data = d), "wt", by = "safe_var", p_adjust = m)
    }
  )
  methods <- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr")
  for (call_name in names(calls)) {
    call <- calls[[call_name]]
    out0 <- call("none")
    p0 <- out0$p
    k <- nrow(out0)
    expect_gt(k, 1)
    for (m in methods) {
      out <- call(m)
      expect_equal(out$p, stats::p.adjust(p0, m), tolerance = 1e-10, label = paste(call_name, m))
    }
    out <- call("sidak")
    expect_equal(out$p, 1 - (1 - p0)^k, tolerance = 1e-10, label = paste(call_name, "sidak"))
    # slopes are tested against 0, not compared pairwise, so "tukey" is
    # replaced by "sidak"
    out <- suppressMessages(call("tukey"))
    expect_equal(out$p, 1 - (1 - p0)^k, tolerance = 1e-10, label = paste(call_name, "tukey"))
    expect_false(isTRUE(all.equal(out$p, p0)), label = paste(call_name, "tukey changed"))
  }
})


test_that("estimate_slopes, p_adjust = 'tukey' uses sidak for slopes", {
  skip_if_not_installed("emmeans")
  d <- datasets::mtcars
  d$cyl_f <- factor(d$cyl)
  m <- lm(mpg ~ wt * cyl_f, data = d)

  p0 <- estimate_slopes(m, "wt", by = "cyl_f", p_adjust = "none")$p
  expect_message(
    out <- estimate_slopes(m, "wt", by = "cyl_f", p_adjust = "tukey"),
    regexp = "Tukey.*Sidak"
  )
  expect_silent(estimate_slopes(m, "wt", by = "cyl_f", p_adjust = "tukey", verbose = FALSE))
  sidak <- estimate_slopes(m, "wt", by = "cyl_f", p_adjust = "sidak")

  expect_equal(out$p, sidak$p, tolerance = 1e-10)
  expect_false(isTRUE(all.equal(out$p, p0)))
  # closed form
  expect_equal(out$p, 1 - (1 - p0)^3, tolerance = 1e-6)
  # emmeans replaces "tukey" by "sidak" for tests that are not pairwise
  em <- suppressMessages(as.data.frame(
    emmeans::test(emmeans::emtrends(m, ~cyl_f, var = "wt"), adjust = "tukey")
  ))
  expect_equal(out$p, em$p.value, tolerance = 1e-6)

  # the exported getter adjusts the same way
  gm_tukey <- suppressMessages(
    get_marginaltrends(m, trend = "wt", by = "cyl_f", p_adjust = "tukey")
  )
  gm_sidak <- get_marginaltrends(m, trend = "wt", by = "cyl_f", p_adjust = "sidak")
  gm_none <- get_marginaltrends(m, trend = "wt", by = "cyl_f", p_adjust = "none")
  expect_equal(gm_tukey$p.value, gm_sidak$p.value, tolerance = 1e-10)
  expect_false(isTRUE(all.equal(gm_tukey$p.value, gm_none$p.value)))
})


test_that("estimate_slopes, p_adjust = 'tukey' keeps tukey for pairwise comparisons", {
  d <- datasets::mtcars
  d$cyl_f <- factor(d$cyl)
  m <- lm(mpg ~ wt * cyl_f, data = d)

  expect_no_message(
    out <- estimate_slopes(
      m,
      "wt",
      by = "cyl_f",
      comparison = ~pairwise,
      p_adjust = "tukey"
    ),
    message = "Sidak"
  )
  ref <- estimate_contrasts(m, contrast = "wt", by = "cyl_f", p_adjust = "tukey")
  expect_identical(nrow(out), nrow(ref))
  expect_equal(out$Slope, ref$Difference, tolerance = 1e-6)
  expect_equal(out$p, ref$p, tolerance = 1e-6)
})


test_that("estimate_slopes, p_adjust with a by-variable named p", {
  d <- datasets::mtcars
  d$safe_var <- factor(d$cyl)
  d$p <- factor(d$cyl)
  for (m in c("holm", "sidak")) {
    ref <- estimate_slopes(lm(mpg ~ wt * safe_var, data = d), "wt", by = "safe_var", p_adjust = m)
    out <- estimate_slopes(lm(mpg ~ wt * p, data = d), "wt", by = "p", p_adjust = m)
    expect_true(is.factor(out$p.1), label = m)
    expect_identical(levels(out$p.1), levels(d$p), label = m)
    expect_identical(as.character(out$p.1), as.character(ref$safe_var), label = m)
    expect_equal(out$p, ref$p, tolerance = 1e-10, label = m)
  }
})


test_that("estimate_slopes, p_adjust counts one test per row", {
  d <- datasets::mtcars
  d$cyl_f <- factor(d$cyl)
  m <- lm(mpg ~ wt * cyl_f, data = d)
  # a single custom hypothesis is one test, so sidak does not change it
  p0 <- estimate_slopes(m, "wt", by = "cyl_f", comparison = "b2=b1")$p
  out <- estimate_slopes(m, "wt", by = "cyl_f", comparison = "b2=b1", p_adjust = "sidak")
  expect_identical(nrow(out), 1L)
  expect_equal(out$p, p0, tolerance = 1e-10)

  # ordinal models have one row per response level and by-level
  skip_if_not_installed("MASS")
  d$gear_o <- factor(d$gear, ordered = TRUE)
  m <- MASS::polr(gear_o ~ wt + cyl_f, data = d, Hess = TRUE)
  p0 <- estimate_slopes(m, "wt", by = "cyl_f")$p
  out <- estimate_slopes(m, "wt", by = "cyl_f", p_adjust = "sidak")
  k <- nrow(out)
  expect_identical(k, 9L)
  expect_equal(out$p, 1 - (1 - p0)^k, tolerance = 1e-10)
})


test_that("p_adjust alerts when there are no p-values", {
  m <- lm(mpg ~ wt, data = datasets::mtcars)
  params <- data.frame(Slope = 1)
  expect_message(
    out <- modelbased:::.p_adjust(m, params, "holm"),
    "No p-values found"
  )
  expect_identical(out, params)
})

skip_on_cran()

test_that("estimate_slopes, works with lme4", {
  skip_if_not_installed("lme4")
  data(CO2)
  model_lme4 <- suppressWarnings(lme4::lmer(
    uptake ~ conc * Plant + Treatment + (1 | Type),
    data = CO2
  ))
  out <- estimate_slopes(model_lme4, trend = "conc", by = "Plant")
  expect_identical(dim(out), c(12L, 8L))
  expect_equal(
    out$Slope,
    c(
      0.01847,
      0.02553,
      0.02322,
      0.02119,
      0.02458,
      0.02548,
      0.01366,
      0.01519,
      0.02286,
      0.00541,
      0.00632,
      0.01085
    ),
    tolerance = 1e-3
  )
})


test_that("estimate_slopes, works with glmmTMB", {
  skip_if_not_installed("glmmTMB")
  data(CO2)
  model_glmmTMB <- suppressWarnings(glmmTMB::glmmTMB(
    uptake ~ conc * Plant + (1 | Type),
    data = CO2
  ))
  out <- estimate_slopes(model_glmmTMB, trend = "conc", by = "Plant")
  expect_identical(dim(out), c(12L, 7L))
  expect_equal(
    out$Slope,
    c(
      0.01847,
      0.02553,
      0.02322,
      0.02119,
      0.02458,
      0.02548,
      0.01366,
      0.01519,
      0.02286,
      0.00541,
      0.00632,
      0.01085
    ),
    tolerance = 1e-3
  )
})


test_that("estimate_slopes, works with glmmTMB and splines", {
  skip_if_not_installed("mgcv")
  data(iris)
  model <- mgcv::gam(Sepal.Width ~ s(Petal.Length, by = Species), data = iris)

  # marginal effects at different values of Petal.Length
  out <- estimate_slopes(model, trend = "Petal.Length", by = "Petal.Length", length = 10)
  expect_identical(dim(out), c(10L, 8L))
  expect_named(out, c("Petal.Length", "Slope", "SE", "CI_low", "CI_high", "t", "df", "p"))

  # marginal effects at very specific values of Petal.Length
  out <- estimate_slopes(model, trend = "Petal.Length", by = "Petal.Length=c(1, 3, 5)")
  expect_identical(dim(out), c(3L, 8L))
  expect_named(out, c("Petal.Length", "Slope", "SE", "CI_low", "CI_high", "t", "df", "p"))
  expect_equal(out$Slope, c(0.26937, 0.05088, -0.0195), tolerance = 1e-3)

  # average marginal effects of Petal.Length,
  # just for the trend within a certain range
  out <- estimate_slopes(model, trend = "Petal.Length=seq(2, 4, 0.01)")
  out2 <- estimate_slopes(model, trend = list(Petal.Length = seq(2, 4, 0.01)))
  expect_identical(dim(out), c(1L, 7L))
  expect_named(out, c("Slope", "SE", "CI_low", "CI_high", "t", "df", "p"))
  expect_equal(out$Slope, 0.06614, tolerance = 1e-3)
  expect_equal(out$Slope, out2$Slope, tolerance = 1e-3)
})


test_that("estimate_slopes, estimate-argument works", {
  skip_if(getRversion() < "4.5.0")
  skip_if_not_installed("datawizard")
  data(penguins, package = "datasets")
  penguins$long_bill <- factor(
    datawizard::categorize(penguins$bill_len),
    labels = c("short", "long")
  )
  m <- glm(
    long_bill ~ sex + species + island * bill_dep,
    data = penguins,
    family = "binomial"
  )

  out <- estimate_slopes(m, "bill_dep", by = "island")
  expect_equal(out$Slope, c(0.00607, 0.04194, 0.00529), tolerance = 1e-4)

  out1 <- estimate_slopes(m, "bill_dep", by = "island", estimate = "average")
  out2 <- marginaleffects::avg_slopes(m, variables = "bill_dep", by = "island")
  expect_equal(out1$Slope, out2$estimate, tolerance = 1e-4)

  out <- estimate_slopes(m, "bill_dep", by = "island = 'Dream'", estimate = "average")
  # test alternative list notation
  out3 <- estimate_slopes(
    m,
    "bill_dep",
    by = list(island = 'Dream'),
    estimate = "average"
  )
  expect_equal(out$Slope, out2$estimate[2], tolerance = 1e-4)
  expect_equal(out$Slope, out3$Slope, tolerance = 1e-4)
  expect_identical(dim(out), c(1L, 7L))
})


test_that("estimate_slopes, estimate-argument and no datagrid works", {
  # no error for estimate = "average" when no data grid is available
  set.seed(123)
  N <- 10000

  cat1 <- sample(c("A", "B", "C"), size = N, replace = TRUE)
  cat2 <- sample(c("Kontrolle", "Treatment"), size = N, replace = TRUE)

  cat1 <- factor(cat1, levels = c("A", "B", "C"))
  cat2 <- factor(cat2, levels = c("Kontrolle", "Treatment"))

  log_odds <- -1.5 +
    ifelse(cat1 == "B", 1.0, 0) +
    ifelse(cat1 == "C", 2.0, 0) +
    ifelse(cat2 == "Treatment", 2.5, 0)

  prob <- plogis(log_odds)
  y <- rbinom(N, size = 1, prob = prob)

  mod_conditional <- glm(y ~ cat1 + cat2, family = binomial(link = "logit"))
  out <- estimate_slopes(mod_conditional, "cat1", estimate = "average")

  expect_equal(out$Slope, c(0.16021, 0.32009), tolerance = 1e-4)
})
