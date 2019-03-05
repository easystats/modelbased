context("estimate_slopes")



test_that("estimate_slopes", {
  library(circus)
  library(rstanarm)

  estim <- estimate_slopes(circus::download_model("stanreg_lm_1"))  # Fix in hte case of only nums
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(1, 8))

  estim <- estimate_slopes(circus::download_model("stanreg_lm_5"))
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 8))

  estim <- estimate_slopes(circus::download_model("stanreg_lm_6"))
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 8))

  estim <- estimate_slopes(circus::download_model("stanreg_lm_7"))
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 8))

})