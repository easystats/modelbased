skip_on_cran()
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
skip_if_not_installed("nanoparquet")
skip_on_os("mac")

test_that("estimate_contrasts - ATT and ATU", {
  skip_if_not_installed("Rdatasets")
  dat <- marginaleffects::get_dataset("lottery")
  dat <- subset(dat, win_big == 1 | win == 0)
  dat$win_big <- as.factor(dat$win_big)

  mod <- lm(
    earnings_post_avg ~ win_big * (
      tickets + man + work + age + education + college + year +
        earnings_pre_1 + earnings_pre_2 + earnings_pre_3),
    data = dat
  )

  out1 <- marginaleffects::avg_predictions(mod, variables = "win_big", by = "win_big")
  out2 <- estimate_means(mod, "win_big")
  expect_equal(out1$estimate, out2$Mean, tolerance = 1e-4)

  # ATE
  out1 <- marginaleffects::avg_comparisons(mod, variables = "win_big", newdata = dat)
  out2 <- estimate_contrasts(mod, "win_big")
  expect_equal(out1$estimate, out2$Difference, tolerance = 1e-4)

  # ATT
  out1 <- marginaleffects::avg_comparisons(mod, variables = "win_big", newdata = subset(dat, win_big == 1))
  out2 <- estimate_contrasts(mod, "win_big", newdata = subset(dat, win_big == 1), estimate = "population")
  expect_equal(out1$estimate, out2$Difference, tolerance = 1e-4)

  # ATU
  out1 <- marginaleffects::avg_comparisons(mod, variables = "win_big", newdata = subset(dat, win_big == 0))
  out2 <- estimate_contrasts(mod, "win_big", newdata = subset(dat, win_big == 0), estimate = "population")
  expect_equal(out1$estimate, out2$Difference, tolerance = 1e-4)

  # error
  expect_error(
    estimate_contrasts(mod, "win_big", newdata = subset(dat, win_big == 1)),
    regex = "It seems that not all"
  )
})
