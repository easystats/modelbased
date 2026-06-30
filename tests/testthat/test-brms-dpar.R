skip_on_cran()
# skip_on_os("windows")
skip_if_not_installed("cogmod")
skip_if_not_installed("curl")
skip_if_offline()
skip_if_not_installed("httr2")
skip_if_not_installed("brms")
skip_if_not_installed("BH")
skip_if_not_installed("RcppEigen")
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")


test_that("estimate_means - brms, chocomini dpars", {
  m <- insight::download_model("brms_chocomini_2")
  skip_if(is.null(m))

  out1 <- estimate_means(m, by = "cond", backend = "marginaleffects")
  out2 <- estimate_means(m, by = "cond", backend = "emmeans")
  expect_equal(out1$Median, out2$Probability, tolerance = 1e-3)

  out1 <- estimate_means(m, by = "cond", backend = "marginaleffects", predict = "mu")
  out2 <- estimate_means(m, by = "cond", backend = "emmeans", predict = "mu")
  expect_equal(out1$Mu, out2$Mu, tolerance = 1e-3)
})
