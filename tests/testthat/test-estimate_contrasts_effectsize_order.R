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


test_that("estimate_contrasts, effectsize boot, effect sizes follow their contrasts", {
  skip_if_not_installed("bootES")
  # bootES's point estimate for one contrast, which does not depend on the resamples
  boot_d <- function(data, response, group, weights) {
    out <- bootES::bootES(
      data = data,
      R = 200,
      data.col = response,
      group.col = group,
      contrast = weights,
      effect.type = "cohens.d"
    )
    as.data.frame(summary(out))[[1]]
  }

  # levels not in alphabetical order
  d <- iris[c("Sepal.Length", "Species")]
  d$Species <- factor(d$Species, levels = c("virginica", "setosa", "versicolor"))
  g <- lm(Sepal.Length ~ Species, data = d)
  set.seed(123)
  out <- estimate_contrasts(g, "Species", effectsize = "boot", backend = "emmeans")
  expect_identical(nrow(out), 3L)
  expected <- vapply(
    seq_len(nrow(out)),
    function(i) {
      # all levels get a weight, as in emmeans, so that all groups are pooled
      w <- stats::setNames(c(0, 0, 0), levels(d$Species))
      w[as.character(out$Level1[i])] <- 1
      w[as.character(out$Level2[i])] <- -1
      boot_d(d, "Sepal.Length", "Species", w)
    },
    numeric(1)
  )
  expect_equal(out$Cohens_d, expected, tolerance = 1e-6)

  # with 6 levels, polynomial contrasts are sorted differently than in emmeans
  w <- lm(weight ~ feed, data = chickwts)
  set.seed(123)
  # few resamples make boot warn about extreme order statistics
  out <- suppressWarnings(estimate_contrasts(
    w,
    "feed",
    comparison = "poly",
    effectsize = "boot",
    backend = "emmeans"
  ))
  expect_identical(nrow(out), 5L)
  poly_weights <- emmeans::poly.emmc(levels(chickwts$feed))
  expected <- vapply(
    as.character(out$Level),
    function(p) {
      boot_d(chickwts, "weight", "feed", stats::setNames(poly_weights[[p]], levels(chickwts$feed)))
    },
    numeric(1),
    USE.NAMES = FALSE
  )
  expect_equal(out$Cohens_d, expected, tolerance = 1e-6)
})
