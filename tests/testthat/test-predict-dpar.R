skip_on_cran()

test_that("estimate_means and estimate_relation - dpar", {
  skip_if_not_installed("brms")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("datawizard")
  skip_if_not_installed("httr2")

  m <- insight::download_model("brms_sigma_2")
  skip_if(is.null(m))

  out1 <- marginaleffects::predictions(
    m,
    newdata = insight::get_datagrid(m, c("Condition", "Participant")),
    by = c("Condition", "Participant"),
    dpar = "sigma"
  )
  out2 <- estimate_means(
    m,
    by = c("Condition", "Participant"),
    predict = "sigma",
    backend = "marginaleffects"
  )
  expect_equal(out1$estimate, out2$Mean, tolerance = 1e-4)

  out1 <- estimate_relation(
    m,
    by = c("Condition", "Participant"),
    predict = "sigma",
  ) |> datawizard::data_arrange("Condition")
  dg <- insight::get_datagrid(m, c("Condition", "Participant"))
  out2 <- cbind(
    dg,
    data.frame(
      predicted = brms::posterior_epred(
        m,
        newdata = dg,
        dpar = "sigma"
      ) |> colMeans()
    )
  ) |> datawizard::data_arrange("Condition")
  expect_equal(out1$Predicted, out2$predicted, tolerance = 1e-4)
})
