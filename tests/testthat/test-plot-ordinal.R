skip_on_os(c("mac", "solaris", "linux"))
skip_if_not_installed("ggplot2")
skip_if_not_installed("see")
skip_if_not_installed("vdiffr")
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
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

  out <- estimate_relation(m, by = "Sepal.Width", verbose = FALSE)
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

  out <- estimate_relation(m1, by = "temp", verbose = FALSE)
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-ordinal-4",
    plot(out, show_data = FALSE)
  )
})


test_that("plots packages bracl and nnet", {
  skip_if_not_installed("brglm2")
  skip_if_not_installed("nnet")

  data("stemcell", package = "brglm2")
  m_bracl <- brglm2::bracl(research ~ as.numeric(religion) + gender,
    weights = frequency,
    data = stemcell, type = "ML"
  )
  m_nnet <- nnet::multinom(research ~ as.numeric(religion) + gender,
    weights = frequency,
    data = stemcell
  )

  out <- estimate_means(m_bracl, "gender")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-ordinal-bracl",
    plot(out, show_data = FALSE)
  )

  out <- estimate_means(m_nnet, "gender")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-ordinal-nnet",
    plot(out, show_data = FALSE)
  )

  m_nnet <- nnet::multinom(research ~ as.numeric(religion) * gender,
    weights = frequency,
    data = stemcell
  )
  out <- estimate_means(m_nnet, c("religion", "gender"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-ordinal-nnet-2",
    plot(out, show_data = FALSE)
  )
})
