skip_on_cran()
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")
skip_on_os("mac")


test_that("estimate_contrast, filter by numeric values", {
  skip_if_not_installed("lme4")
  data(iris)
  mod <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
  out1 <- estimate_contrasts(mod, contrast = "Species=c('versicolor','setosa')", by = "Petal.Width", estimate = "average")
  expect_identical(dim(out1), c(5L, 10L))
  expect_equal(out1$Difference, c(0.13903, 0.06148, -0.01608, -0.09363, -0.17118), tolerance = 1e-4)

  expect_error(
    estimate_contrasts(mod, contrast = "Species=c('versicolor','setosa')", by = "Petal.Width=c(2,3)", estimate = "average"),
    regex = "None of the values"
  )

  data(CO2)
  mod <- suppressWarnings(lme4::lmer(uptake ~ conc * Plant + (1 | Type), data = CO2))
  out1 <- estimate_contrasts(mod, contrast = "Plant", by = "conc", estimate = "average")
  expect_identical(dim(out1), c(462L, 10L))

  out1 <- estimate_contrasts(mod, contrast = "Plant=c('Qn1','Qn2','Qn3')", by = "conc", estimate = "average")
  expect_identical(dim(out1), c(21L, 10L))

  out1 <- estimate_contrasts(mod, contrast = "Plant=c('Qn1','Qn2','Qn3')", estimate = "average")
  expect_identical(dim(out1), c(3L, 9L))
  expect_equal(out1$Difference, c(1.92857, 4.38571, 2.45714), tolerance = 1e-4)

  out <- estimate_contrasts(mod, contrast = "conc", by = "Plant", comparison = "b1=b2", estimate = "average")
  expect_equal(out$Difference, -0.007061251, tolerance = 1e-4)
})


test_that("estimate_contrast, filterin in `by` and `contrast`", {
  data(efc, package = "modelbased")
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
  levels(efc$c172code) <- c("low", "mid", "high")
  m <- lm(neg_c_7 ~ barthtot + c172code * e42dep + c161sex, data = efc)

  out <- estimate_contrasts(m, c("e42dep", "c172code"), estimate = "average")
  expect_identical(dim(out), c(66L, 9L))

  out <- estimate_contrasts(
    m,
    "e42dep=c('independent','slightly dependent','moderately dependent')",
    by = "c172code",
    estimate = "average"
  )
  expect_identical(dim(out), c(9L, 10L))
  expect_equal(
    out$Difference,
    c(
      -0.56667, 0.87147, 1.43814, 1.30144, 3.00341, 1.70197, 2.78974,
      3.11667, 0.32692
    ),
    tolerance = 1e-4
  )

  out <- estimate_contrasts(
    m,
    "e42dep=c('independent','slightly dependent','moderately dependent')",
    by = "c172code",
    comparison = "b1=b4",
    estimate = "average"
  )
  expect_equal(out$Difference, 1.507576, tolerance = 1e-4)

  out <- estimate_contrasts(m, "e42dep", by = "c172code=c('low','mid')", estimate = "average")
  expect_identical(dim(out), c(12L, 10L))

  out <- estimate_contrasts(m, "e42dep=c('independent','slightly dependent')", by = "c172code=c('low','mid')", estimate = "average")
  expect_identical(dim(out), c(2L, 10L))
})
