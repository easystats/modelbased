skip_on_cran()
skip_if_not_installed("emmeans")

test_that("estimate_contrasts, effectsize emmeans, effect sizes follow their contrasts", {
  # levels not in alphabetical order
  d <- iris
  d$Species <- factor(d$Species, levels = c("virginica", "setosa", "versicolor"))
  g <- lm(Sepal.Length ~ Species, data = d)
  out <- estimate_contrasts(g, "Species", effectsize = "emmeans", backend = "emmeans")
  expect_identical(nrow(out), 3L)
  expect_equal(out$partial_d, out$Difference / sigma(g), tolerance = 1e-6)

  # with 6 levels, polynomial contrasts are sorted differently than in emmeans
  w <- lm(weight ~ feed, data = chickwts)
  out <- estimate_contrasts(w, "feed", comparison = "poly", effectsize = "emmeans", backend = "emmeans")
  expect_identical(nrow(out), 5L)
  expect_equal(out$partial_d, out$Difference / sigma(w), tolerance = 1e-6)
})


test_that("estimate_contrasts, effectsize emmeans, effect sizes with a by variable", {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  d$am <- factor(d$am)
  m <- lm(mpg ~ cyl * am, data = d)
  out <- estimate_contrasts(m, "cyl", by = "am", effectsize = "emmeans", backend = "emmeans")
  expect_identical(nrow(out), 6L)
  expect_equal(out$partial_d, out$Difference / sigma(m), tolerance = 1e-6)
})
