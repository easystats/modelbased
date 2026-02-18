skip_on_cran()
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

test_that("estimate_means() - estimate = average", {
  data(mtcars)
  set.seed(123)
  mtcars$weight <- abs(rnorm(nrow(mtcars), 1, 0.3))
  mtcars$cyl <- as.factor(mtcars$cyl)
  m <- lm(mpg ~ wt + cyl + vs, data = mtcars, weights = weight)

  expect_warning(
    estimate_means(m, "cyl", weights = "weight"),
    regex = "Using weights is not possible"
  )

  out1 <- estimate_means(m, "cyl", weights = "weight", estimate = "average")
  out2 <- marginaleffects::avg_predictions(m, by = "cyl", wts = "weight")
  expect_equal(out1$Mean, out2$estimate, tolerance = 1e-4)
  out1 <- estimate_means(m, "cyl", estimate = "average")
  out2 <- marginaleffects::avg_predictions(m, by = "cyl")
  expect_equal(out1$Mean, out2$estimate, tolerance = 1e-4)
})


test_that("estimate_means() - estimate = population", {
  data(mtcars)
  set.seed(123)
  mtcars$weight <- abs(rnorm(nrow(mtcars), 1, 0.3))
  mtcars$cyl <- as.factor(mtcars$cyl)
  m <- lm(mpg ~ wt + cyl + vs, data = mtcars, weights = weight)
  out1 <- estimate_means(m, "cyl", weights = "weight", estimate = "population")
  out2 <- marginaleffects::avg_predictions(m, variables = "cyl", wts = "weight")
  expect_equal(out1$Mean, out2$estimate, tolerance = 1e-4)
  out1 <- estimate_means(m, "cyl", estimate = "population")
  out2 <- marginaleffects::avg_predictions(m, variables = "cyl")
  expect_equal(out1$Mean, out2$estimate, tolerance = 1e-4)
})
