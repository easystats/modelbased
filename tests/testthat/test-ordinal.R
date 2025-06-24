skip_on_cran()
skip_if_not_installed("curl")
skip_if_offline()
skip_if_not_installed("brms")
skip_if_not_installed("BH")
skip_if_not_installed("RcppEigen")
skip_if_not_installed("marginaleffects")
skip_if_not_installed("httr2")
skip_if_not_installed("MASS")

test_that("estimate_relation prints ordinal models correctly", {
  m <- suppressWarnings(insight::download_model("brms_categorical_2_num"))
  out <- suppressWarnings(estimate_relation(m))
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")
  out <- suppressWarnings(estimate_means(m, by = "Sepal.Width"))
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")

  m <- MASS::polr(Species ~ Sepal.Width, data = iris)
  out <- estimate_relation(m, verbose = FALSE)
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")
  out <- estimate_means(m, by = "Sepal.Width")
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")

  # keep row column
  out <- suppressWarnings(estimate_relation(m, data = iris[1:3, ], verbose = FALSE))
  expect_named(out, c("Row", "Response", "Sepal.Width", "Predicted", "CI_low", "CI_high", "Residuals")) # nolint
  expect_identical(dim(out), c(9L, 7L))
})


test_that("estimate_means, print bracl", {
  skip_if_not_installed("brglm2")
  skip_if(getRversion() < "4.5.0")

  data(penguins)

  m <- brglm2::bracl(species ~ island + sex, data = penguins)
  out <- estimate_means(m, by = "island")
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")

  m <- nnet::multinom(species ~ island + sex, data = penguins)
  out <- estimate_means(m, by = "island")
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")
})
