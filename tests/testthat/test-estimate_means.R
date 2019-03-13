context("estimate_means")



test_that("estimate_means", {
  library(circus)
  library(rstanarm)

  estim <- estimate_means(circus::download_model("stanreg_lm_6"))
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 6))

  estim <- estimate_means(circus::download_model("stanreg_lm_4"))
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(1, 6))

})