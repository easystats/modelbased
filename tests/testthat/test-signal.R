if (require("testthat") && require("modelbased") && require("parameters")) {
  test_that("signal", {
    set.seed(333)

    x <- sin(seq(0, 4 * pi, length.out = 100)) + rnorm(100, 0, 0.2)
    s1 <- as.vector(smoothing(x, method = "loess"))
    s2 <- as.vector(smoothing(x, method = "smooth"))

    expect_true(as.numeric(parameters::smoothness(s1)) > as.numeric(parameters::smoothness(s2)))
  })
}
