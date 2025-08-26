skip_on_os(c("mac", "solaris", "linux"))
skip_if_not_installed("ggplot2")
skip_if_not_installed("see")
skip_if_not_installed("vdiffr")
skip_if_not_installed("marginaleffects", minimum_version = "0.28.0.22")
skip_on_cran()

test_that("plot 2nd by is numeric", {
  set.seed(1234)
  x1 <- rnorm(200)
  x2 <- rnorm(200)
  y <- 2 * x1 + x1^2 + 4 * x2 + rnorm(200)

  d <- data.frame(x1, x2, y)
  model <- lm(y ~ x1 + x2, data = d)

  pr <- estimate_means(model, c("x1", "x2"))
  vdiffr::expect_doppelganger(
    "plot-auto-numeric-by-1",
    plot(pr)
  )

  pr <- estimate_means(model, c("x1", "x2 = [sd]"))
  vdiffr::expect_doppelganger(
    "plot-auto-numeric-by-2",
    plot(pr)
  )
})
