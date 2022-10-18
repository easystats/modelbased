if (require("glmmTMB") && require("emmeans")) {
  # skip_on_cran()

  data <- glmmTMB::Salamanders
  model <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ mined + (1 | site),
    ziformula = ~mined,
    family = poisson,
    data = data
  ))

  model2 <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ cover + mined + (1 | site),
    ziformula = ~ cover + mined,
    family = glmmTMB::truncated_poisson,
    data = data
  ))

  test_that("estimate_means - glmmTMB", {
    estim <- estimate_means(model)
    estim2 <- as.data.frame(emmeans::emmeans(model, ~mined, type = "response"))
    expect_equal(estim$rate, estim2$rate, tolerance = 1e-3)

    estim <- estimate_means(model, component = "zi")
    estim2 <- as.data.frame(emmeans::emmeans(model, ~mined, component = "zi", type = "response"))
    expect_equal(estim$rate, estim2$rate, tolerance = 1e-3)
  })

  test_that("estimate_contrasts - glmmTMB", {
    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(1, 9))
    expect_equal(estim$Difference, -1.141923, tolerance = 1e-1)
    expect_equal(c(estim$Level1[1], estim$Level2[1]), c("yes", "no"))

    estim <- estimate_contrasts(model, component = "zi")
    expect_equal(dim(estim), c(1, 9))
    expect_equal(estim$Difference, 1.736067, tolerance = 1e-1)
    expect_equal(c(estim$Level1[1], estim$Level2[1]), c("yes", "no"))
  })

  test_that("estimate_slope - glmmTMB", {
    estim <- estimate_slopes(model2, trend = "cover", at = "mined", regrid = "response")
    estim2 <- as.data.frame(emmeans::emtrends(model2, "mined", var = "cover", regrid = "response"))
    expect_equal(estim$Coefficient, estim2$cover.trend, tolerance = 1e-2)
  })


  test_that("estimate_response - glmmTMB", {
    estim <- estimate_expectation(model2)
    expect_equal(dim(estim), c(nrow(data), 8))
  })

  test_that("estimate_link - glmmTMB", {
    estim <- estimate_link(model2, preserve_range = FALSE)
    expect_equal(dim(estim), c(20, 7))
  })

  test_that("estimate_response - glmmTMB", {
    estim <- estimate_expectation(model2)
    expect_equal(dim(estim), c(644, 8))
  })
}
