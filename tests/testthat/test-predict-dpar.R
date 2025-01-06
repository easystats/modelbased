test_that("estimate_means and estimate_relation - dpar", {
  skip_if_not_installed("brms")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("httr2")

  m <- insight::download_model("brms_sigma_2")
  skip_if(is.null(m))

  out <- estimate_contrasts(
    m,
    c("time", "coffee"),
    backend = "marginaleffects",
    p_adjust = "none",
    method = "(b2-b1)=(b4-b3)"
  )
  expect_snapshot(print(out, zap_small = TRUE))
})
