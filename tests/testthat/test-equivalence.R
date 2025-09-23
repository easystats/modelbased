skip_on_cran()
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

test_that("estimate_means() - equivalence test", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("datawizard")
  data(mtcars)
  mtcars$mpg <- datawizard::normalize(mtcars$mpg, include_bounds = FALSE)

  mod <- glmmTMB::glmmTMB(
    mpg ~ as.factor(gear) + (1 | cyl),
    data = mtcars,
    family = glmmTMB::beta_family()
  )
  out <- estimate_means(mod, "gear", equivalence = c(0.3, 0.5))
  expect_named(out, c("gear", "Proportion", "SE", "CI_low", "CI_high", "z", "p_Equivalence"))
  expect_equal(out$p_Equivalence, c(0.26701, 0.50868, 0.39303), tolerance = 1e-4)
  expect_identical(attributes(out)$equivalence, c(0.3, 0.5))

  mod <- glmmTMB::glmmTMB(mpg ~ wt + (1 | cyl), data = mtcars, family = glmmTMB::beta_family())
  out <- estimate_slopes(mod, "wt", equivalence = c(-0.2, 0.2))
  expect_named(out, c("gear", "Proportion", "SE", "CI_low", "CI_high", "z", "p_Equivalence"))
  expect_equal(out$p_Equivalence, 0.9872101, tolerance = 1e-4)
  expect_identical(attributes(out)$equivalence, c(-0.2, 0.2))
})
