skip_on_os(c("mac", "solaris", "linux"))
skip_if_not_installed("ggplot2")
skip_if_not_installed("see")
skip_if_not_installed("vdiffr")
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")
skip_on_cran()

test_that("plots facets", {
  data(efc, package = "modelbased")

  # make categorical
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex"))
  fit <- lm(neg_c_7 ~ c12hour * barthtot * c161sex * c172code, data = efc)

  pr <- estimate_means(fit, c("c12hour", "c172code", "c161sex"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-interaction-facets-1",
    plot(pr, show_data = FALSE)
  )

  pr <- estimate_means(fit, c("c12hour", "c172code", "barthtot"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-interaction-facets-2",
    plot(pr, show_data = FALSE)
  )

  pr <- estimate_means(fit, c("c12hour", "c172code", "c161sex", "barthtot"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-interaction-facets-3",
    plot(pr, show_data = FALSE)
  )

  pr <- estimate_means(fit, c("c12hour", "c172code", "barthtot", "c161sex"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-interaction-facets-4",
    plot(pr, show_data = FALSE)
  )

  pr <- estimate_means(fit, c("c12hour", "barthtot", "c161sex", "c172code"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-interaction-facets-5",
    plot(pr, show_data = FALSE)
  )

  fit <- lm(neg_c_7 ~ c12hour * barthtot * c161sex * c172code * e16sex, data = efc)
  set.seed(123)
  pr <- estimate_means(fit, c("c12hour", "barthtot", "c161sex", "c172code", "e16sex"))
  vdiffr::expect_doppelganger(
    "plot-interaction-facets-6",
    plot(pr, show_data = FALSE)
  )

  pr <- estimate_means(fit, c("c12hour", "c161sex", "c172code", "e16sex", "barthtot"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-interaction-facets-7",
    plot(pr, show_data = FALSE)
  )

  pr <- estimate_means(fit, c("c12hour", "c172code", "c161sex", "barthtot"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-interaction-facets-8",
    plot(pr, show_data = FALSE)
  )

  pr <- estimate_means(fit, c("c12hour", "c172code", "barthtot", "c161sex"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-interaction-facets-9",
    plot(pr, show_data = FALSE)
  )

  pr <- estimate_means(fit, c("c12hour", "e16sex", "c161sex", "c172code"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-interaction-facets-10",
    plot(pr, show_data = FALSE)
  )
})


test_that("plots facets", {
  data(efc, package = "modelbased")
  # make categorical
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "nur_pst"))

  fit <- lm(neg_c_7 ~ c161sex * c172code * e16sex * nur_pst, data = efc)
  pr <- estimate_means(fit, c("c161sex", "c172code", "e16sex", "nur_pst"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-interaction-facets-cat-1",
    plot(pr, show_data = FALSE)
  )

  fit <- lm(neg_c_7 ~ c161sex * c172code * e16sex * nur_pst * negc7d, data = efc)
  pr <- estimate_means(fit, c("c161sex", "c172code", "e16sex", "nur_pst", "negc7d"))
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-interaction-facets-cat-2",
    plot(pr, show_data = FALSE)
  )
  # no connecting lines
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-interaction-facets-cat-3",
    plot(pr, show_data = FALSE, join_dots = FALSE)
  )
})


test_that("plots facets, emmeans", {
  data(efc, package = "modelbased")
  # make categorical
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "nur_pst"))

  fit <- lm(neg_c_7 ~ c161sex * c172code * e16sex * nur_pst, data = efc)
  pr <- estimate_means(fit, c("c161sex", "c172code", "e16sex", "nur_pst"), backend = "emmeans")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-interaction-facets-cat-4emmeans",
    plot(pr, show_data = FALSE)
  )

  fit <- lm(neg_c_7 ~ c161sex * c172code * e16sex * nur_pst * negc7d, data = efc)
  pr <- estimate_means(fit, c("c161sex", "c172code", "e16sex", "nur_pst", "negc7d"), backend = "emmeans")
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-interaction-facets-cat-5emmeans",
    plot(pr, show_data = FALSE)
  )
  # no connecting lines
  set.seed(123)
  vdiffr::expect_doppelganger(
    "plot-interaction-facets-cat-6emmeans",
    plot(pr, show_data = FALSE, join_dots = FALSE)
  )
})
