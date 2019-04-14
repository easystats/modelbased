context("estimate_slopes")



test_that("estimate_slopes", {
  library(insight)
  library(rstanarm)

  testthat::expect_error(estimate_slopes(insight::download_model("stanreg_lm_1")))

  estim <- estimate_slopes(insight::download_model("stanreg_lm_4"))
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(1, 8))

  estim <- estimate_slopes(insight::download_model("stanreg_lm_6"))
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 8))
})
