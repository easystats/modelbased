skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

test_that("estimate_means, transform", {
  data(cars)
  m <- lm(log(dist) ~ speed, data = cars)
  out <- estimate_means(m, "speed")
  expect_named(out, c("speed", "Mean", "SE", "CI_low", "CI_high", "t", "df"))
  expect_equal(
    out$Mean,
    c(
      2.15918, 2.44097, 2.72276, 3.00454, 3.28633, 3.56811, 3.8499,
      4.13168, 4.41347, 4.69525
    ),
    tolerance = 1e-4
  )
  expect_snapshot(out)
  out1 <- estimate_means(m, "speed", transform = TRUE)
  expect_named(out1, c("speed", "Mean", "CI_low", "CI_high", "df"))
  expect_equal(
    out1$Mean,
    c(
      8.66407, 11.48417, 15.2222, 20.17694, 26.74442, 35.44958, 46.98822,
      62.28261, 82.55525, 109.42651
    ),
    tolerance = 1e-4
  )
  expect_snapshot(out1)
  out2 <- estimate_means(m, "speed", transform = exp)
  expect_equal(out1$Mean, out2$Mean, tolerance = 1e-4)
})


test_that("estimate_expectation, transform", {
  data(cars)
  m <- lm(log(dist) ~ speed, data = cars)
  out <- estimate_expectation(m, by = "speed")
  expect_named(out, c("speed", "Predicted", "SE", "CI_low", "CI_high"))
  expect_equal(
    out$Predicted,
    c(
      2.15918, 2.44097, 2.72276, 3.00454, 3.28633, 3.56811, 3.8499,
      4.13168, 4.41347, 4.69525
    ),
    tolerance = 1e-4
  )
  out1 <- estimate_expectation(m, by = "speed", transform = TRUE)
  expect_named(out1, c("speed", "Predicted", "CI_low", "CI_high"))
  expect_equal(
    out1$Predicted,
    c(
      8.66407, 11.48417, 15.2222, 20.17694, 26.74442, 35.44958, 46.98822,
      62.28261, 82.55525, 109.42651
    ),
    tolerance = 1e-4
  )
  out2 <- estimate_expectation(m, by = "speed", transform = exp)
  expect_equal(out1$Predicted, out2$Predicted, tolerance = 1e-4)
})


test_that("estimate_slopes, transform", {
  data(iris)
  mod <- lm(log(Sepal.Length) ~ Sepal.Width * Species, data = iris)

  out <- estimate_slopes(mod, trend = "Sepal.Width", by = "Species")
  expect_identical(dim(out), c(3L, 8L))
  expect_equal(out$Slope, c(0.13752, 0.14779, 0.13957), tolerance = 1e-3)

  out <- estimate_contrasts(mod, "Sepal.Width", by = "Species")
  expect_identical(dim(out), c(3L, 9L))
  expect_equal(out$Difference, c(0.01027, 0.00205, -0.00822), tolerance = 1e-3)

  out <- estimate_slopes(mod, trend = "Sepal.Width", by = "Species", transform = TRUE)
  expect_identical(dim(out), c(3L, 7L))
  expect_equal(out$Slope, c(1.14743, 1.15927, 1.14978), tolerance = 1e-3)

  out <- estimate_contrasts(mod, "Sepal.Width", by = "Species", transform = TRUE)
  expect_identical(dim(out), c(3L, 8L))
  expect_equal(out$Difference, c(1.01032, 1.00206, 0.99182), tolerance = 1e-3)
})
