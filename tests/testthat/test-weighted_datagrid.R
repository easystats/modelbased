skip_on_cran()
skip_if_not_installed("marginaleffects")
skip_if_not_installed("insight", minimum_version = "1.5.3")

test_that("weighted data grids work", {
  data(penguins)

  # one factor
  model <- lm(bill_len ~ species + sex + island, data = penguins)
  dg <- insight::get_datagrid(model, weighted = TRUE)

  emm1 <- estimate_means(model, "species", estimate = "average")
  emm2 <- estimate_means(
    model,
    "species",
    estimate = "average",
    data = dg,
    weights = dg$Weight
  )
  expect_equal(emm1$Mean, emm2$Mean, tolerance = 1e-4)
})
