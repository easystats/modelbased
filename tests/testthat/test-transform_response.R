test_that("estimate_means, transform", {
  data(cars)
  m <- lm(log(dist) ~ speed, data = cars)
  out <- estimate_means(m, "speed")
  expect_equal(
    out$Mean,
    c(
      2.15918, 2.44097, 2.72276, 3.00454, 3.28633, 3.56811, 3.8499,
      4.13168, 4.41347, 4.69525
    ),
    tolerance = 1e-4
  )
  out1 <- estimate_means(m, "speed", transform = TRUE)
  expect_equal(
    out1$Mean,
    c(
      8.66407, 11.48417, 15.2222, 20.17694, 26.74442, 35.44958, 46.98822,
      62.28261, 82.55525, 109.42651
    ),
    tolerance = 1e-4
  )
  out2 <- estimate_means(m, "speed", transform = exp)
  expect_equal(out1$Mean, out2$Mean, tolerance = 1e-4)
})


test_that("estimate_expectation, transform", {
  data(cars)
  m <- lm(log(dist) ~ speed, data = cars)
  out <- estimate_expectation(m, by = "speed")
  expect_equal(
    out$Predicted,
    c(
      2.15918, 2.44097, 2.72276, 3.00454, 3.28633, 3.56811, 3.8499,
      4.13168, 4.41347, 4.69525
    ),
    tolerance = 1e-4
  )
  out1 <- estimate_expectation(m, by = "speed", transform = TRUE)
  expect_equal(
    out1$Predicted,
    c(
      8.66407, 11.48417, 15.2222, 20.17694, 26.74442, 35.44958, 46.98822,
      62.28261, 82.55525, 109.42651
    ),
    tolerance = 1e-4
  )
  out2 <- estimate_expectation(m, by ="speed", transform = exp)
  expect_equal(out1$Predicted, out2$Predicted, tolerance = 1e-4)
})
