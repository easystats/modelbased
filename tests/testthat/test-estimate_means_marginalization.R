skip_on_cran()
skip_if_not_installed("marginaleffects")

test_that("estimate_means() - estimate options", {
  data(efc, package = "modelbased")
  efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
  levels(efc$c172code) <- c("low", "mid", "high")
  m <- lm(barthtot ~ c161sex + c172code + neg_c_7, data = efc)

  out <- estimate_means(m, "c161sex", estimate = "specific")
  expect_equal(out$Mean, c(61.15226, 60.60773), tolerance = 1e-4)
  out <- estimate_means(m, "c161sex", estimate = "typical")
  expect_equal(out$Mean, c(64.61181, 64.06727), tolerance = 1e-4)
  out <- estimate_means(m, "c161sex", estimate = "average")
  expect_equal(out$Mean, c(67.05128, 64.03226), tolerance = 1e-4)
  out <- estimate_means(m, "c161sex", estimate = "population")
  expect_equal(out$Mean, c(65.16885, 64.62431), tolerance = 1e-4)
})
