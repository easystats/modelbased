skip_if_not_installed("marginaleffects")
skip_if_not_installed("lme4")

test_that("multivariate response", {
  data(cbpp, package = "lme4")
  gm1 <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = "binomial"
  )

  out <- estimate_expectation(gm1, by = "period", include_random = TRUE)
  expect_identical(dim(out), c(4L, 6L))

  out <- estimate_expectation(gm1, include_random = TRUE)
  expect_identical(dim(out), c(56L, 6L))

  fm1 <- lm(cbind(mpg, wt) ~ cyl + disp, data = mtcars)
  out <- estimate_expectation(fm1)
  expect_identical(dim(out), c(64L, 5L))

  out <- estimate_expectation(fm1, by = "cyl")
  expect_identical(dim(out), c(6L, 4L))
})
