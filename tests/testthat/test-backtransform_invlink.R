skip_on_cran()
skip_if_not_installed("marginaleffects")
skip_if_not_installed("glmmTMB")

test_that("estimate_means correct inverse link for glmmTMB", {
  data(mtcars)
  d <- mtcars
  d$count <- rep(c(0, 0, 0, 0, 1, 2, 4), length.out = nrow(mtcars))
  m <- glmmTMB::glmmTMB(
    count ~ cyl,
    data = datawizard::data_modify(d, cyl = as.factor(cyl)),
    family = "poisson"
  )
  out1 <- estimate_means(m, backend = "marginaleffects")
  out2 <- estimate_means(m, backend = "emmeans")
  expect_equal(out1$Mean, out2$Rate, tolerance = 1e-4)
  expect_equal(out1$CI_low, out2$CI_low, tolerance = 1e-4)
  expect_equal(out1$CI_high, out2$CI_high, tolerance = 1e-4)
})
