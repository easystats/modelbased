context("signal")


test_that("signal", {
  library(parameters)
  set.seed(333)

  x <- sin(seq(0, 4 * pi, length.out = 100)) + rnorm(100, 0, 0.2)
  s1 <- smoothing(x, method = "loess")
  s2 <- smoothing(x, method = "smooth")

  testthat::expect_true(parameters::smoothness(s1) > parameters::smoothness(s2))
})
