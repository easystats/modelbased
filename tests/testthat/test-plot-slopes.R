skip_on_os(c("mac", "solaris", "linux"))
skip_if_not_installed("ggplot2")
skip_if_not_installed("see")
skip_if_not_installed("vdiffr")
skip_if_not_installed("marginaleffects")
skip_on_cran()

test_that("plot slopes, correct y axis labels", {
  data(mtcars)
  mtcars$gear <- as.factor(mtcars$gear)
  model <- lm(mpg ~ hp * wt, data = mtcars)
  slopes <- estimate_slopes(model, trend = "hp", by = "wt")

  vdiffr::expect_doppelganger(
    "plot-slopes-y-axis-labels-1",
    plot(slopes)
  )

  model <- lm(mpg ~ hp * gear, data = mtcars)
  slopes <- estimate_slopes(model, trend = "hp", by = "gear")

  vdiffr::expect_doppelganger(
    "plot-slopes-y-axis-labels-2",
    plot(slopes)
  )
})
