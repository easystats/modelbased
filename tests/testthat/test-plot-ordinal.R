skip_on_os(c("mac", "solaris", "linux"))
skip_if_not_installed("ggplot2")
skip_if_not_installed("see")
skip_if_not_installed("vdiffr")
skip_if_not_installed("marginaleffects")
skip_if_not_installed("MASS")
skip_on_cran()

test_that("plots ordinal", {
  m <- MASS::polr(Species ~ Sepal.Width, data = iris)
  out <- estimate_means(m, by = "Sepal.Width")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-ordinal-1",
    plot(out, show_data = FALSE)
  )

  out <- estimate_relation(m, by = "Sepal.Width")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-ordinal-2",
    plot(out, show_data = FALSE)
  )
})
