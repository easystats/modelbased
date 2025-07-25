skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")

test_that("estimate_slopes", {
  data(iris)
  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

  expect_message(estimate_slopes(model, backend = "emmeans"), regex = "No numeric")
  estim1 <- suppressMessages(estimate_slopes(model, backend = "emmeans"))
  estim2 <- suppressMessages(estimate_slopes(model, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(1L, 8L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 1e-4)

  estim1 <- suppressMessages(estimate_slopes(model, by = "Species", backend = "emmeans"))
  estim2 <- suppressMessages(estimate_slopes(model, by = "Species", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(3L, 9L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 1e-4)

  estim1 <- suppressMessages(estimate_slopes(model, by = "Petal.Length", backend = "emmeans"))
  estim2 <- suppressMessages(estimate_slopes(model, by = "Petal.Length", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(10L, 9L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 0.2)

  estim1 <- suppressMessages(estimate_slopes(model, by = c("Species", "Petal.Length"), backend = "emmeans"))
  expect_identical(dim(estim1), c(30L, 10L))

  estim2 <- suppressMessages(estimate_slopes(model, by = c("Species", "Petal.Length"), preserve_range = FALSE, backend = "marginaleffects"))
  expect_identical(dim(estim2), c(30L, 9L))
  expect_equal(estim1$Slope, estim2$Slope[order(estim2$Petal.Length, estim2$Species)], tolerance = 1e-3)

  # test different DF
  estim1 <- suppressMessages(estimate_slopes(model, by = "Petal.Length", backend = "marginaleffects"))
  estim2 <- suppressMessages(estimate_slopes(model, by = "Petal.Length", df = Inf, backend = "marginaleffects"))
  expect_named(
    estim1,
    c("Petal.Length", "Slope", "SE", "CI_low", "CI_high", "t", "df", "p")
  )
  expect_named(
    estim2,
    c("Petal.Length", "Slope", "SE", "CI_low", "CI_high", "z", "p")
  )

  model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)

  estim1 <- suppressMessages(estimate_slopes(model, by = "Sepal.Width", backend = "emmeans"))
  estim2 <- suppressMessages(estimate_slopes(model, by = "Sepal.Width", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(10L, 9L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 0.2)
  estim1 <- suppressMessages(estimate_slopes(model, by = "Sepal.Width", length = 5, backend = "emmeans"))
  estim2 <- suppressMessages(estimate_slopes(model, by = "Sepal.Width", length = 5, backend = "marginaleffects"))
  expect_identical(dim(estim1), c(5L, 9L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 0.2)
  estim1 <- suppressMessages(estimate_slopes(model, by = "Sepal.Width = c(1, 2, 3)", backend = "emmeans"))
  estim2 <- suppressMessages(estimate_slopes(model, by = "Sepal.Width = c(1, 2, 3)", backend = "marginaleffects"))
  expect_identical(dim(estim1), c(3L, 9L))
  expect_equal(estim1$Slope, estim2$Slope, tolerance = 0.2)
})


test_that("estimate_slopes, johnson-neyman p-adjust", {
  data(iris)
  model <- lm(Sepal.Width ~ Petal.Width * Petal.Length, data = iris)
  out <- estimate_slopes(model, "Petal.Width", by = "Petal.Length")
  expect_equal(
    out$CI_low,
    c(
      -0.83863, -0.66978, -0.50396, -0.34188, -0.18459, -0.03252,
      0.11384, 0.25426, 0.38899, 0.51855
    ),
    tolerance = 1e-2
  )
  expect_equal(
    out$p,
    c(
      0.00664, 0.03956, 0.20019, 0.70529, 0.52501, 0.08553, 0.00496,
      0.00013, 0, 0
    ),
    tolerance = 1e-2
  )

  out <- estimate_slopes(model, "Petal.Width", by = "Petal.Length", p_adjust = "esarey")
  expect_equal(
    out$CI_low,
    c(
      -0.89944, -0.72628, -0.55667, -0.39145, -0.23184, -0.07835,
      0.06843, 0.20825, 0.3414, 0.46848
    ),
    tolerance = 1e-2
  )
  expect_equal(
    out$p,
    c(
      0.03438, 0.14813, 0.50393, 0.83427, 0.24689, 0.03326, 0.00219,
      9e-05, 0, 0
    ),
    tolerance = 1e-2
  )

  skip_if_not_installed("mvnorm")
  set.seed(123)
  out <- estimate_slopes(model, "Petal.Width", by = "Petal.Length", p_adjust = "sup-t")
  expect_equal(
    out$CI_low,
    c(
      -0.90003, -0.72665, -0.55721, -0.39196, -0.23229, -0.07873,
      0.06797, 0.20793, 0.34088, 0.468
    ),
    tolerance = 1e-2
  )
  expect_equal(
    out$p,
    c(
      0.01765, 0.08691, 0.36222, 0.9097, 0.76792, 0.17275, 0.01369,
      5e-04, 1e-05, 0
    ),
    tolerance = 1e-2
  )
})


test_that("estimate_slopes, custom comparison", {
  data(iris)
  m <- lm(Sepal.Width ~ Sepal.Length * Species, data = iris)
  out <- estimate_contrasts(m, "Sepal.Length", by = "Species", comparison = "(b1 - b2) = (b1 - b3)")
  expect_identical(dim(out), c(1L, 8L))
  expect_equal(out$Difference, -0.08782885, tolerance = 1e-4)
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
      0.01847, 0.02553, 0.02322, 0.02119, 0.02458, 0.02548, 0.01366,
      0.01519, 0.02286, 0.00541, 0.00632, 0.01085
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
      0.01847, 0.02553, 0.02322, 0.02119, 0.02458, 0.02548, 0.01366,
      0.01519, 0.02286, 0.00541, 0.00632, 0.01085
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
  expect_identical(dim(out), c(1L, 7L))
  expect_named(out, c("Slope", "SE", "CI_low", "CI_high", "t", "df", "p"))
  expect_equal(out$Slope, 0.06614, tolerance = 1e-3)
})


test_that("estimate_slopes, estimate-argument works", {
  skip_if(getRversion() < "4.5.0")
  skip_if_not_installed("datawizard")
  data(penguins)
  penguins$long_bill <- factor(datawizard::categorize(penguins$bill_len), labels = c("short", "long"))
  m <- glm(long_bill ~ sex + species + island * bill_dep, data = penguins, family = "binomial")

  out <- estimate_slopes(m, "bill_dep", by = "island")
  expect_equal(out$Slope, c(0.00607, 0.04194, 0.00529), tolerance = 1e-4)

  out1 <- estimate_slopes(m, "bill_dep", by = "island", estimate = "average")
  out2 <- marginaleffects::avg_slopes(m, variables = "bill_dep", by = "island")
  expect_equal(out1$Slope, out2$estimate, tolerance = 1e-4)

  out <- estimate_slopes(m, "bill_dep", by = "island = 'Dream'", estimate = "average")
  expect_equal(out$Slope, out2$estimate[2], tolerance = 1e-4)
  expect_identical(dim(out), c(1L, 7L))
})
