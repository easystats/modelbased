skip_on_os(c("mac", "solaris", "linux"))
skip_if_not_installed("ggplot2")
skip_if_not_installed("see")
skip_if_not_installed("vdiffr")
skip_if_not_installed("marginaleffects")
skip_if_not_installed("MASS")
skip_if_not_installed("ordinal")
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


test_that("plots package ordinal", {
  data(wine, package = "ordinal")
  m1 <- ordinal::clm(rating ~ temp * contact, data = wine)

  out <- estimate_means(m1, by = "temp")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-ordinal-3",
    plot(out, show_data = FALSE)
  )

  out <- estimate_relation(m1, by = "temp")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-ordinal-4",
    plot(out, show_data = FALSE)
  )
})
