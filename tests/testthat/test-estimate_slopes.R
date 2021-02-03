if (require("testthat") && require("modelbased") && require("rstanarm") && require("insight")) {
  test_that("estimate_slopes", {
    # expect_error(estimate_slopes(insight::download_model("stanreg_lm_1")))
    #
    # model <- insight::download_model("stanreg_lm_4")
    # estim <- estimate_slopes(model)
    # expect_equal(c(nrow(estim), ncol(estim)), c(1, 7))
    #
    # model <- insight::download_model("stanreg_lm_6")
    # estim <- estimate_slopes(model)
    # expect_equal(c(nrow(estim), ncol(estim)), c(3, 7))
  })
}
