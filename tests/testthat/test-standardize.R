skip_on_cran()
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")

test_that("standardize() - estimate_means()", {
  data(mtcars)

  dat <- mtcars
  dat$gear <- as.factor(dat$gear)
  dat$cyl <- as.factor(dat$cyl)

  # Simple
  model <- lm(mpg ~ cyl, data = dat)
  estim <- estimate_means(model, "cyl", backend = "marginaleffects")
  out1 <- standardize(estim)
  out2 <- unstandardize(out1)
  expect_equal(as.vector(out1$Mean), c(1.0906, -0.0577, -0.82805), tolerance = 1e-4)
  expect_equal(as.vector(out2$Mean), estim$Mean, tolerance = 1e-4)
})


test_that("standardize() - estimate_predicted", {
  data(mtcars)

  dat <- mtcars
  dat$gear <- as.factor(dat$gear)
  dat$cyl <- as.factor(dat$cyl)

  # Simple
  model <- lm(mpg ~ cyl, data = dat)
  estim <- estimate_relation(model, by = "cyl")
  out1 <- standardize(estim)
  out2 <- unstandardize(out1)
  expect_equal(as.vector(out1$Predicted), c(1.0906, -0.0577, -0.82805), tolerance = 1e-4)
  expect_equal(as.vector(out2$Predicted), estim$Predicted, tolerance = 1e-4)
})


test_that("standardize() - estimate_contrasts()", {
  data(mtcars)

  dat <- mtcars
  dat$gear <- as.factor(dat$gear)
  dat$cyl <- as.factor(dat$cyl)

  # Simple
  model <- lm(mpg ~ cyl, data = dat)
  estim <- estimate_contrasts(model, "cyl", backend = "marginaleffects")
  out1 <- standardize(estim)
  out2 <- unstandardize(out1)
  expect_equal(as.vector(out1$Difference), c(-1.14831, -1.91866, -0.77035), tolerance = 1e-4)
  expect_equal(as.vector(out2$Difference), estim$Difference, tolerance = 1e-4)
})
