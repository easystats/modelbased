context("estimate_response")



test_that("estimate_response", {
  library(circus)
  library(rstanarm)

  estim <- estimate_response(circus::download_model("stanreg_lm_5"))
  testthat::expect_equal(nrow(estim), nrow(iris))

  estim <- estimate_response(circus::download_model("stanreg_lm_6"), data="grid")
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(30, 6))

  estim <- estimate_response(circus::download_model("stanreg_lm_7"))
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(150, 6))

})