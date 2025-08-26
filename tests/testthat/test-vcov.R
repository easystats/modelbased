skip_on_cran()
skip_if_not_installed("marginaleffects", minimum_version = "0.28.0.22")
skip_if_not_installed("nanoparquet")
skip_if_not_installed("sandwich")
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

test_that("estimate_means - glmmTMB, vcov", {
  skip_if_not_installed("glmmTMB", minimum_version = "1.1.12")
  skip_if_not_installed("parameters")

  data("fish", package = "parameters")

  m1 <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ child + camper + (1 | persons),
    ziformula = ~ child + camper + (1 | persons),
    data = fish,
    family = glmmTMB::truncated_poisson()
  ))

  out <- estimate_means(m1, "child")
  expect_equal(out$SE, c(3.151255, 0.575173, 0.059253, 0.008286), tolerance = 1e-4)
  out <- estimate_means(m1, "child", vcov = "HC0")
  expect_equal(out$SE, c(3.445424, 0.553019, 0.05949, 0.0094), tolerance = 1e-4)
})
