skip_on_cran()
skip_if_not_installed("marginaleffects")
skip_if_not_installed("nanoparquet")
skip_on_os("mac")

test_that("estimate_contrasts - vcov", {
  dat <- marginaleffects::get_dataset("lottery")
  dat <- subset(dat, win_big == 1 | win == 0)
  dat$win_big <- as.factor(dat$win_big)

  mod <- lm(
    earnings_post_avg ~ win_big * (
      tickets + man + work + age + education + college + year +
        earnings_pre_1 + earnings_pre_2 + earnings_pre_3),
    data = dat
  )

  out1 <- marginaleffects::avg_predictions(mod, variables = "win_big", by = "win_big", vcov = "HC3")
  out2 <- estimate_means(mod, "win_big", vcov = "HC3")
  expect_equal(out1$std.error, out2$SE, tolerance = 1e-4)
})
