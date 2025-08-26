skip_on_cran()
# skip_on_os("windows")
skip_if_not_installed("brms")
skip_if_not_installed("BH")
skip_if_not_installed("RcppEigen")
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects", minimum_version = "0.28.0.22")

test_that("estimate_means - brms", {
  model <- brms::brm(Sepal.Length ~ Species * Sepal.Width, data = iris, refresh = 0, iter = 1000)
  estim <- estimate_means(model, backend = "emmeans")
  expect_identical(dim(estim), c(3L, 5L))
})

test_that("estimate_relation - brms", {
  model <- brms::brm(Sepal.Length ~ Species * Sepal.Width, data = iris, refresh = 0, iter = 1000)
  estim <- estimate_relation(model, preserve_range = FALSE)
  expect_identical(dim(estim), c(30L, 6L))

  # estim <- estimate_relation(model, preserve_range=FALSE, iterations = 10)
  # expect_equal(dim(estim), c(30, 6))
})

test_that("estimate_slopes - brms", {
  model <- brms::brm(Sepal.Length ~ Species * Sepal.Width, data = iris, refresh = 0, iter = 1000)
  estim <- estimate_slopes(model, by = "Species", backend = "emmeans")
  expect_identical(dim(estim), c(3L, 5L))
})

test_that("estimate_means - brms, multivariate", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")

  m <- insight::download_model("brms_mv_1")
  skip_if(is.null(m))
  estim <- estimate_means(m, "wt")
  expect_identical(dim(estim), c(30L, 10L))
  expect_named(
    estim,
    c(
      "wt", "ROPE_CI", "Response", "Median", "CI_low", "CI_high",
      "pd", "ROPE_low", "ROPE_high", "ROPE_Percentage"
    )
  )
})


test_that("estimate_means - brms, Wiener", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("RWiener")

  m <- insight::download_model("m_ddm_1")
  skip_if(is.null(m))
  d <- insight::get_data(m)[1:5, ]

  set.seed(123)
  out <- estimate_prediction(m, data = d)
  expect_snapshot(print(out))

  set.seed(123)
  out <- estimate_prediction(m, data = d, keep_iterations = 3)
  expect_snapshot(print(out, table_width = Inf))
})
