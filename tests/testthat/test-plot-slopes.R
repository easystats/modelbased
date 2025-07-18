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


test_that("estimate_slopes, plotting works with brms", {
  skip_if_not_installed("brms")
  skip_if_not_installed("BH")
  skip_if_not_installed("RcppEigen")
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")

  m <- insight::download_model("brms_slopes_1")
  skip_if(is.null(m))

  set.seed(123)
  out <- estimate_slopes(m, "Murder", by = "Illiteracy", length = 4)
  vdiffr::expect_doppelganger(
    "plot-slopes-brms-1",
    plot(out)
  )
})
