skip_on_cran()
skip_if_not_installed("emmeans")

test_that("estimate_contrasts, effectsize emmeans, effect sizes follow their contrasts", {
  # the levels are not in alphabetical order, so the contrast rows are sorted
  # in another order than the rows of emmeans
  d <- iris
  d$Species <- factor(d$Species, levels = c("virginica", "setosa", "versicolor"))
  g <- lm(Sepal.Length ~ Species, data = d)
  out <- estimate_contrasts(g, "Species", effectsize = "emmeans", backend = "emmeans")
  expect_identical(nrow(out), 3L)
  # e.g. versicolor - setosa: 0.930 / sigma(g) = 0.930 / 0.5148 = 1.807
  expect_equal(out$partial_d, out$Difference / sigma(g), tolerance = 1e-6)
})


test_that("estimate_contrasts, effectsize emmeans, effect sizes with a by variable", {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  d$am <- factor(d$am)
  m <- lm(mpg ~ cyl * am, data = d)
  out <- estimate_contrasts(m, "cyl", by = "am", effectsize = "emmeans", backend = "emmeans")
  expect_identical(nrow(out), 6L)
  # e.g. cyl 6 - cyl 4 at am 0: -3.775 / sigma(m) = -3.775 / 3.032 = -1.245
  expect_type(out$partial_d, "double")
  expect_equal(out$partial_d, out$Difference / sigma(m), tolerance = 1e-6)
})


test_that("estimate_contrasts, effectsize emmeans, polynomial contrasts and ci level", {
  g <- lm(Sepal.Length ~ Species, data = iris)
  # the polynomial contrasts are sorted as "linear", "quadratic"
  out <- estimate_contrasts(
    g,
    "Species",
    comparison = "poly",
    effectsize = "emmeans",
    backend = "emmeans"
  )
  expect_identical(nrow(out), 2L)
  expect_equal(out$partial_d, out$Difference / sigma(g), tolerance = 1e-6)

  # the effect-size limits are at the level of `ci`
  e <- emmeans::eff_size(
    emmeans::emmeans(g, "Species"),
    sigma = sigma(g),
    edf = df.residual(g)
  )
  e80 <- as.data.frame(confint(e, level = 0.8, adjust = "none"))
  out80 <- estimate_contrasts(g, "Species", effectsize = "emmeans", backend = "emmeans", ci = 0.8)
  # emmeans contrasts the levels in the other direction, so the rows are
  # matched by the absolute effect size, and the half-widths are compared
  idx <- match(round(abs(out80$partial_d), 6), round(abs(e80$effect.size), 6))
  expect_false(anyNA(idx))
  expect_equal(
    (out80$es_CI_high - out80$es_CI_low) / 2,
    ((e80$upper.CL - e80$lower.CL) / 2)[idx],
    tolerance = 1e-6
  )
  out95 <- estimate_contrasts(g, "Species", effectsize = "emmeans", backend = "emmeans")
  expect_true(all(out80$es_CI_high - out80$es_CI_low < out95$es_CI_high - out95$es_CI_low))
})
