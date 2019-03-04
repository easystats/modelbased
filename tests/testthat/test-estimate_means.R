context("estimate_means")



test_that("estimate_means", {
  library(circus)
  library(rstanarm)

  model <- circus::download_model("stanreg_lm_4")
  testthat::expect_equal(nrow(estimate_means(model)), 3)
  testthat::expect_equal(nrow(estimate_means(model)), 3)

})