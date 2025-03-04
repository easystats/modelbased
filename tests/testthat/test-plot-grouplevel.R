skip_on_cran()
skip_on_os(c("mac", "solaris", "linux"))
skip_if_not_installed("ggplot2")
skip_if_not_installed("see")
skip_if_not_installed("vdiffr")

skip_if(utils::packageVersion("insight") <= "1.1.0")
skip_if(utils::packageVersion("parameters") <= "0.24.1")

test_that("plots grouplevel frequentist", {
  skip_if_not_installed("lme4")

  data(mtcars)
  model <- lme4::lmer(mpg ~ hp + (1 | carb), data = mtcars)

  out <- estimate_grouplevel(model)
  vdiffr::expect_doppelganger("plot-grouplevel-freq-1", plot(out))

  out <- estimate_grouplevel(model, type = "total")
  vdiffr::expect_doppelganger("plot-grouplevel-freq-2", plot(out))
})

test_that("plots grouplevel Bayesian", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("brms")

  model <- insight::download_model("brms_sigma_3")

  out <- estimate_grouplevel(model)
  vdiffr::expect_doppelganger("plot-grouplevel-Bayes-1", plot(out))

  out <- estimate_grouplevel(model, type = "total")
  vdiffr::expect_doppelganger("plot-grouplevel-Bayes-2", plot(out))

  model <- insight::download_model("brms_zi_4")

  out <- estimate_grouplevel(model)
  vdiffr::expect_doppelganger("plot-grouplevel-Bayes-3", plot(out))

  out <- estimate_grouplevel(model, type = "total")
  vdiffr::expect_doppelganger("plot-grouplevel-Bayes-4", plot(out))
})
