context("estimate_means")



test_that("estimate_means", {
  library(insight)
  library(rstanarm)

  estim <- estimate_means(insight::download_model("stanreg_lm_6"))
  testthat::expect_equal(c(nrow(estim), ncol(estim)), c(3, 5))

})
