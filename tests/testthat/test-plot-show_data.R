skip_on_cran()
skip_on_os(c("mac", "solaris", "linux"))
skip_if_not_installed("ggplot2")
skip_if_not_installed("see")
skip_if_not_installed("vdiffr")

test_that("plots show data with collapsing by group", {
  skip_if_not_installed("lme4")
  data(efc, package = "modelbased")
  efc$e15relat <- as.factor(efc$e15relat)
  efc$c161sex <- as.factor(efc$c161sex)
  levels(efc$c161sex) <- c("male", "female")
  model <- lme4::lmer(neg_c_7 ~ c161sex + (1 | e15relat), data = efc)

  me <- estimate_means(model, "c161sex")

  set.seed(1234)
  vdiffr::expect_doppelganger("plot-show_data_me1", plot(me, show_data = TRUE))

  set.seed(1234)
  vdiffr::expect_doppelganger(
    "plot-show_data_me2",
    plot(me, show_data = TRUE, collapse_group = "e15relat")
  )

  set.seed(1234)
  vdiffr::expect_doppelganger("plot-show_data_me3", plot(me, show_residuals = TRUE))

  set.seed(1234)
  vdiffr::expect_doppelganger(
    "plot-show_data_me4",
    plot(me, show_residuals = TRUE, collapse_group = "e15relat")
  )
})
