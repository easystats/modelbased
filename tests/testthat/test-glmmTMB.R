skip_on_cran()
skip_if_not_installed("glmmTMB")
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")

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
  estim <- suppressMessages(estimate_means(model, backend = "emmeans"))
  estim2 <- as.data.frame(emmeans::emmeans(model, ~mined, type = "response"))
  expect_equal(estim$Rate, estim2$rate, tolerance = 1e-3)

  ## emmeans for zero-inflated model, zero-inflated component
  estim <- suppressMessages(estimate_means(model, component = "zi", backend = "emmeans"))
  estim2 <- as.data.frame(emmeans::emmeans(model, ~mined, component = "zi", type = "response"))
  expect_equal(estim$Rate, estim2$rate, tolerance = 1e-3)

  ## marginaleffects for zero-inflated model, count component
  estim1 <- estimate_means(model, by = "mined", backend = "marginaleffects")
  estim2 <- suppressMessages(estimate_means(model, backend = "marginaleffects"))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-3)

  ## marginaleffects for zero-inflated model, zero-inflation probabilities
  estim1 <- estimate_means(model, by = "mined", backend = "marginaleffects", predict = "zprob")
  estim2 <- estimate_means(model, backend = "marginaleffects", predict = "zprob")
  expect_equal(estim1$Probability, estim2$predicted, tolerance = 1e-3)
  # validated against ggeffects
  expect_equal(estim1$Probability, c(0.75755, 0.35508), tolerance = 1e-3)
  estim_me <- marginaleffects::avg_predictions(
    model,
    newdata = insight::get_datagrid(model, by = "mined", factors = "all", include_random = TRUE),
    by = "mined",
    re.form = NULL,
    type = "zprob"
  )
  expect_equal(estim2$Probability, estim_me$estimate, tolerance = 1e-3)
})


test_that("estimate_contrasts - glmmTMB", {
  ## contrasts emmeans for zero-inflated model, count component
  estim1 <- suppressMessages(estimate_contrasts(model, backend = "emmeans"))
  expect_identical(dim(estim1), c(1L, 9L))
  # validated against ggeffects
  expect_equal(estim1$Difference, -2.32874, tolerance = 1e-3)
  expect_identical(c(estim1$Level1[1], estim1$Level2[1]), c("yes", "no"))

  ## contrasts marginaleffects for zero-inflated model, count component
  estim3 <- suppressMessages(estimate_contrasts(model, contrast = "mined", backend = "marginaleffects"))
  expect_equal(estim3$Difference, -1.99344, tolerance = 1e-3)
  estim_me <- marginaleffects::avg_predictions(
    model,
    newdata = insight::get_datagrid(model, by = "mined", factors = "all", include_random = TRUE),
    by = "mined",
    hypothesis = ~pairwise,
    re.form = NULL
  )
  expect_equal(estim3$Difference, estim_me$estimate[2], tolerance = 1e-3)

  # select default for contrast automatically works
  estim4 <- suppressMessages(estimate_contrasts(model, backend = "marginaleffects"))
  expect_equal(estim3$Difference, estim4$Difference, tolerance = 1e-3)

  ## contrasts emmeans for zero-inflated model, zero-inflation probability component
  estim1 <- suppressMessages(estimate_contrasts(model, component = "zi", backend = "emmeans"))
  estim2 <- predict(model, type = "zprob", newdata = insight::get_datagrid(model, "mined"))
  expect_identical(dim(estim1), c(1L, 9L))
  expect_equal(estim1$Difference, diff(estim_2), tolerance = 1e-1)
  expect_identical(c(estim1$Level1[1], estim1$Level2[1]), c("yes", "no"))

  ## contrasts marginaleffects for zero-inflated model, zero-inflation probability component
  estim3 <- suppressMessages(estimate_contrasts(model, predict = "zprob", backend = "marginaleffects"))
  expect_equal(estim3$Difference, diff(estim_2), tolerance = 1e-1)
})


test_that("estimate_slope - glmmTMB", {
  skip_on_os("mac")
  estim <- suppressMessages(estimate_slopes(model2, trend = "cover", by = "mined", regrid = "response", backend = "emmeans")) # nolint
  estim2 <- as.data.frame(emmeans::emtrends(model2, "mined", var = "cover", regrid = "response"))
  expect_equal(estim$Slope, estim2$cover.trend, tolerance = 1e-2)
  estim1 <- estimate_slopes(model2, trend = "cover", by = "mined", backend = "marginaleffects")
  datagrid <- insight::get_datagrid(model2, by = "mined")
  estim2 <- suppressWarnings(marginaleffects::avg_slopes(model2, newdata = datagrid, variables = "cover", by = "mined"))
  expect_equal(estim1$Slope, estim2$estimate, tolerance = 1e-2)
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
