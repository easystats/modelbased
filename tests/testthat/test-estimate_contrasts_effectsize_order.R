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
