context("estimate_contrasts")



test_that("estimate_contrasts", {
  library(circus)
  library(rstanarm)

  estim <- estimate_contrasts(circus::download_model("stanreg_lm_6"))
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 9))

  testthat::expect_error(estimate_contrasts(circus::download_model("stanreg_lm_4")))

})