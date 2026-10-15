skip_on_cran()
# skip_on_os("windows")
skip_if_not_installed("brms")
skip_if_not_installed("BH")
skip_if_not_installed("RcppEigen")
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

test_that("estimate_means - brms", {
  model <- brms::brm(
    Sepal.Length ~ Species * Sepal.Width,
    data = iris,
    refresh = 0,
    iter = 1000,
    seed = 123
  )
  estim <- estimate_means(model, backend = "emmeans")
  expect_identical(dim(estim), c(3L, 5L))

  estim1 <- estimate_means(model, backend = "marginaleffects")
  expect_null(attributes(estim1)$iterations)

  estim2 <- estimate_means(model, iterations = 200, backend = "marginaleffects")
  expect_identical(attributes(estim2)$iterations, 200)

  # a random subsample of 200 draws gives other medians than all draws. The
  # differences can be smaller than 0.001, so we only test that they differ.
  expect_false(isTRUE(all.equal(estim1$Median, estim2$Median, tolerance = 0)))
  estim3 <- estimate_means(
    model,
    iterations = 200,
    keep_iterations = TRUE,
    backend = "marginaleffects"
  )
  expect_length(grep("^iter_", colnames(estim3)), 200)
})

test_that("estimate_relation - brms", {
  model <- brms::brm(
    Sepal.Length ~ Species * Sepal.Width,
    data = iris,
    refresh = 0,
    iter = 1000
  )
  estim <- estimate_relation(model, preserve_range = FALSE)
  expect_identical(dim(estim), c(30L, 6L))

  # estim <- estimate_relation(model, preserve_range=FALSE, iterations = 10)
  # expect_equal(dim(estim), c(30, 6))
})

test_that("estimate_slopes - brms", {
  model <- brms::brm(
    Sepal.Length ~ Species * Sepal.Width,
    data = iris,
    refresh = 0,
    iter = 1000
  )
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
      "wt",
      "ROPE_CI",
      "Response",
      "Median",
      "CI_low",
      "CI_high",
      "pd",
      "ROPE_low",
      "ROPE_high",
      "ROPE_Percentage"
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

  est <- estimate_means(m, by = NULL)
  expect_identical(
    capture.output(print(est)),
    c(
      "Estimated Marginal Means",
      "",
      "Median |       95% CI |   pd |          ROPE | % in ROPE",
      "--------------------------------------------------------",
      "0.84   | [0.82, 0.86] | 100% | [-0.10, 0.10] |        0%",
      "",
      "Variable predicted: rt, response",
      "Predictions are on the response-scale."
    )
  )
})


test_that("estimate_means - brms, Wiener, printing", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("RWiener")

  m <- insight::download_model("brms_wiener_1")
  skip_if(is.null(m))

  old_opt <- getOption("wiener_backend")
  options(wiener_backend = "rtdists")
  on.exit(options(wiener_backend = old_opt))

  est <- estimate_means(m, by = "x", predict = "mu")
  expect_identical(
    capture.output(print(est)),
    c(
      "Estimated Marginal Means",
      "",
      "x |   Mu |       95% CI |   pd |          ROPE | % in ROPE",
      "----------------------------------------------------------",
      "a | 0.63 | [0.37, 0.87] | 100% | [-0.10, 0.10] |        0%",
      "b | 1.81 | [1.50, 2.14] | 100% | [-0.10, 0.10] |        0%",
      "",
      "Variable predicted: rt, resp",
      "Predictors modulated: x",
      "Predictions are on the mu-scale."
    )
  )

  cont <- estimate_contrasts(m, contrast = "x", predict = "mu")
  expect_identical(
    capture.output(print(cont)),
    c(
      "Marginal Contrasts Analysis",
      "",
      "Level1 | Level2 | Median |       95% CI |   pd |          ROPE | % in ROPE",
      "--------------------------------------------------------------------------",
      "b      | a      |   1.18 | [0.82, 1.54] | 100% | [-0.10, 0.10] |        0%",
      "",
      "Variable predicted: rt, resp",
      "Predictors contrasted: x",
      "Contrasts are on the mu-scale."
    )
  )
})
