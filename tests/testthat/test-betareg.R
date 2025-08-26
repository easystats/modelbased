skip_on_cran()
skip_if_not_installed("curl")
skip_if_offline()
skip_if_not_installed("marginaleffects", minimum_version = "0.28.0.22")
skip_if_not_installed("betareg")

test_that("estimate_means for betareg", {
  data("GasolineYield", package = "betareg")
  m1 <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
  out <- estimate_means(m1, "batch")
  expect_snapshot(print(out, zap_small = TRUE))
})


test_that("estimate_relation for betareg", {
  data("GasolineYield", package = "betareg")
  m1 <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
  out <- estimate_relation(m1, by = "batch", verbose = FALSE)
  expect_snapshot(print(out, zap_small = TRUE))
})
