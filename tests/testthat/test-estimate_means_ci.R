skip_on_cran()
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

test_that("estimate_means() - ci frequentist", {
  data(iris)

  mod2 <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)

  # Estimate means with different CIs
  out <- estimate_means(mod2, by = "Species", backend = "marginaleffects", ci = 0.95)
  x <- capture.output(out)
  expect_identical(x[5], "setosa     | 4.75 | 0.09 | [4.58, 4.92] |  54.34")

  out <- estimate_means(mod2, by = "Species", backend = "marginaleffects", ci = 0.89)
  x <- capture.output(out)
  expect_identical(x[5], "setosa     | 4.75 | 0.09 | [4.61, 4.89] |  54.34")

  out <- estimate_contrasts(mod2, contrast = "Species", backend = "marginaleffects", ci = 0.95)
  x <- capture.output(out)
  expect_identical(x[5], "versicolor | setosa     |       1.43 | 0.12 | [1.19, 1.68] |  11.78 | < .001")

  out <- estimate_contrasts(mod2, contrast = "Species", backend = "marginaleffects", ci = 0.89)
  x <- capture.output(out)
  expect_identical(x[5], "versicolor | setosa     |       1.43 | 0.12 | [1.24, 1.63] |  11.78 | < .001")

  # Estimate slopes with different CIs
  out <- estimate_slopes(mod2, trend = "Sepal.Width", by = "Species", backend = "marginaleffects", ci = 0.95)
  x <- capture.output(out)
  expect_identical(x[5], "setosa     |  0.69 | 0.17 | [0.36, 1.02] |   4.17 | < .001")

  out <- estimate_slopes(mod2, trend = "Sepal.Width", by = "Species", backend = "marginaleffects", ci = 0.89)
  x <- capture.output(out)
  expect_identical(x[5], "setosa     |  0.69 | 0.17 | [0.42, 0.96] |   4.17 | < .001")
})


skip_if_not_installed("curl")
skip_if_offline()
skip_if_not_installed("brms")
skip_if_not_installed("BH")
skip_if_not_installed("RcppEigen")
skip_if_not_installed("httr2")

test_that("estimate_means() - ci frequentist", {
  mod2 <- insight::download_model("brms_4")
  skip_if(is.null(mod2))

  # Estimate means with different CIs
  out <- estimate_means(mod2, by = "Species", backend = "marginaleffects", ci = 0.95)
  x <- capture.output(out)
  expect_identical(x[5], "setosa     |   4.76 | [4.59, 4.93] | 100% | [-0.10, 0.10] |        0%")

  out <- estimate_means(mod2, by = "Species", backend = "marginaleffects", ci = 0.89)
  x <- capture.output(out)
  expect_identical(x[5], "setosa     |   4.76 | [4.62, 4.90] | 100% | [-0.10, 0.10] |        0%")

  out <- estimate_contrasts(mod2, contrast = "Species", backend = "marginaleffects", ci = 0.95)
  x <- capture.output(out)
  expect_identical(x[5], "versicolor | setosa     |   1.43 | [1.18, 1.67] | 100% | [-0.10, 0.10] |        0%")

  out <- estimate_contrasts(mod2, contrast = "Species", backend = "marginaleffects", ci = 0.89)
  x <- capture.output(out)
  expect_identical(x[5], "versicolor | setosa     |   1.43 | [1.23, 1.63] | 100% | [-0.10, 0.10] |        0%")

  # Estimate slopes with different CIs
  out <- estimate_slopes(mod2, trend = "Sepal.Width", by = "Species", backend = "marginaleffects", ci = 0.95)
  x <- capture.output(out)
  expect_identical(x[5], "setosa     |   0.67 | [0.35, 0.99] | 100% | [-0.10, 0.10] |        0%")

  out <- estimate_slopes(mod2, trend = "Sepal.Width", by = "Species", backend = "marginaleffects", ci = 0.89)
  x <- capture.output(out)
  expect_identical(x[5], "setosa     |   0.67 | [0.41, 0.93] | 100% | [-0.10, 0.10] |        0%")
})
