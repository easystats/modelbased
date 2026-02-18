skip_on_cran()
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
skip_on_os("mac")


test_that("estimate_means, filter by numeric values", {
  skip_if_not_installed("lme4")
  data(iris)
  mod <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
  out1 <- estimate_means(mod, c("Species=c('versicolor','setosa')", "Petal.Width"), estimate = "average")
  expect_identical(dim(out1), c(2L, 8L))
  expect_equal(out1$Mean, c(4.87019, 6.46946), tolerance = 1e-4)
  expect_equal(out1$Petal.Width, c(0.1, 1.7), tolerance = 1e-4)

  out1 <- estimate_means(mod, c("Species", "Petal.Width=c(1.2, 2.4)"), estimate = "average")
  expect_identical(dim(out1), c(2L, 8L))
  expect_equal(out1$Mean, c(5.75628, 6.83141), tolerance = 1e-4)
  expect_equal(out1$Petal.Width, c(1.2, 2.4), tolerance = 1e-4)

  expect_error(
    estimate_means(mod, c("Species=c('versicolor','setosa')", "Petal.Width=c(3,5)"), estimate = "average"),
    regex = "None of the values specified"
  )
})


test_that("estimate_contrast, filterin in `by` and `contrast`", {
  data(efc, package = "modelbased")
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
  levels(efc$c172code) <- c("low", "mid", "high")
  m <- lm(neg_c_7 ~ barthtot + c172code * e42dep + c161sex, data = efc)

  out <- estimate_means(m, c("e42dep", "c172code"), estimate = "average")
  expect_identical(dim(out), c(12L, 8L))

  out <- estimate_means(
    m,
    by = c(
      "e42dep=c('independent','slightly dependent','moderately dependent')",
      "c172code"
    ),
    estimate = "average"
  )
  expect_identical(dim(out), c(9L, 8L))
  expect_equal(
    out$Mean,
    c(
      10.41667, 8.90909, 8.8, 9.85, 10.21053, 11.58974, 11.28814,
      11.9125, 11.91667
    ),
    tolerance = 1e-4
  )

  out <- estimate_means(m, c("e42dep", "c172code=c('low','mid')"), estimate = "average")
  expect_identical(dim(out), c(8L, 8L))

  out <- estimate_means(m,
    c(
      "e42dep=c('independent','slightly dependent')",
      "c172code=c('low','mid')"
    ),
    estimate = "average"
  )
  expect_identical(dim(out), c(4L, 8L))
})
