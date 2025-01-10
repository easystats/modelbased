skip_if_not_installed("glmmTMB")
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")
skip_if_not_installed("ggeffects")

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
  ## emmeans for zero-inflated model, count component
  estim <- suppressMessages(estimate_means(model))
  estim2 <- as.data.frame(emmeans::emmeans(model, ~mined, type = "response"))
  expect_equal(estim$Rate, estim2$rate, tolerance = 1e-3)

  ## emmeans for zero-inflated model, zero-inflated component
  estim <- suppressMessages(estimate_means(model, component = "zi"))
  estim2 <- as.data.frame(emmeans::emmeans(model, ~mined, component = "zi", type = "response"))
  expect_equal(estim$Rate, estim2$rate, tolerance = 1e-3)

  ## marginaleffects for zero-inflated model, count component
  estim1 <- estimate_means(model, by = "mined", backend = "marginaleffects")
  estim2 <- suppressMessages(estimate_means(model, backend = "marginaleffects"))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-3)

  ## marginaleffects for zero-inflated model, zero-inflation probabilities
  estim1 <- estimate_means(model, by = "mined", backend = "marginaleffects", predict = "zprob")
  estim2 <- ggeffects::predict_response(model, "mined", type = "zi_prob")
  estim3 <- estimate_means(model, backend = "marginaleffects", predict = "zprob")
  expect_equal(estim1$Mean, estim2$predicted, tolerance = 1e-3)
  expect_equal(estim1$Mean, estim3$Mean, tolerance = 1e-3)
})


test_that("estimate_contrasts - glmmTMB", {
  ## contrasts emmeans for zero-inflated model, count component
  estim1 <- suppressMessages(estimate_contrasts(model))
  pr <- ggeffects::predict_response(model, "mined", verbose = FALSE)
  estim2 <- ggeffects::test_predictions(pr)
  expect_identical(dim(estim1), c(1L, 9L))
  expect_equal(estim1$Difference, -2.32874, tolerance = 1e-3)
  expect_equal(estim1$Difference, estim2$Contrast, tolerance = 1e-1)
  expect_identical(c(estim1$Level1[1], estim1$Level2[1]), c("yes", "no"))

  ## contrasts marginaleffects for zero-inflated model, count component
  estim3 <- suppressMessages(estimate_contrasts(model, contrast = "mined", backend = "marginaleffects"))
  expect_equal(estim3$Difference, -1.99344, tolerance = 1e-3)
  # select default for contrast automatically works
  estim4 <- suppressMessages(estimate_contrasts(model, backend = "marginaleffects"))
  expect_equal(estim3$Difference, estim4$Difference, tolerance = 1e-3)

  ## contrasts emmeans for zero-inflated model, zero-inflation probability component
  estim1 <- suppressMessages(estimate_contrasts(model, component = "zi"))
  pr <- ggeffects::predict_response(model, "mined", type = "zi_prob", verbose = FALSE)
  estim2 <- ggeffects::test_predictions(pr)
  expect_identical(dim(estim1), c(1L, 9L))
  expect_equal(estim1$Difference, 0.40247, tolerance = 1e-1)
  expect_identical(c(estim1$Level1[1], estim1$Level2[1]), c("yes", "no"))
  expect_equal(estim1$Difference, estim2$Contrast, tolerance = 1e-1)

  ## contrasts marginaleffects for zero-inflated model, zero-inflation probability component
  estim3 <- suppressMessages(estimate_contrasts(model, predict = "zprob", backend = "marginaleffects"))
  expect_equal(estim3$Difference, estim2$Contrast, tolerance = 1e-1)
})


test_that("estimate_slope - glmmTMB", {
  estim <- suppressMessages(estimate_slopes(model2,
    trend = "cover",
    by = "mined", regrid = "response"
  ))
  estim2 <- as.data.frame(emmeans::emtrends(model2, "mined", var = "cover", regrid = "response"))
  expect_equal(estim$Coefficient, estim2$cover.trend, tolerance = 1e-2)
})


test_that("estimate_expectation - glmmTMB", {
  estim <- suppressMessages(estimate_expectation(model2))
  expect_identical(dim(estim), c(nrow(data), 8L))
})


test_that("estimate_link - glmmTMB", {
  estim <- suppressMessages(estimate_link(model2, preserve_range = FALSE))
  expect_identical(dim(estim), c(20L, 7L))
})


test_that("estimate_expectation - glmmTMB", {
  estim <- suppressMessages(estimate_expectation(model2))
  expect_identical(dim(estim), c(644L, 8L))
})
