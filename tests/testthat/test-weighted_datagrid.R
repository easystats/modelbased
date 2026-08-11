skip_on_cran()
skip_if_not_installed("marginaleffects")
skip_if_not_installed("insight", minimum_version = "1.5.3")


test_that("weighted data grids work for average", {
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
  emm3 <- estimate_means(model, "species", estimate = "average", fast = TRUE)

  expect_equal(emm1$Mean, emm2$Mean, tolerance = 1e-4)
  expect_equal(emm1$Mean, emm3$Mean, tolerance = 1e-4)
})


test_that("weighted data grids work for population", {
  data(penguins)

  # one factor
  model <- lm(bill_len ~ species + sex + island, data = penguins)
  dg <- insight::get_datagrid(model, weighted = TRUE)

  emm1 <- estimate_means(model, "species", estimate = "population")
  emm2 <- estimate_means(
    model,
    "species",
    estimate = "population",
    data = dg,
    weights = dg$Weight
  )
  emm3 <- estimate_means(model, "species", estimate = "population", fast = TRUE)

  expect_equal(emm1$Mean, emm2$Mean, tolerance = 1e-4)
  expect_equal(emm1$Mean, emm3$Mean, tolerance = 1e-4)
})


test_that("weighted data grids errors for other estimate options", {
  data(penguins)

  # one factor
  model <- lm(bill_len ~ species + sex + island, data = penguins)
  dg <- insight::get_datagrid(model, weighted = TRUE)

  expect_error(
    estimate_means(model, "species", fast = TRUE),
    regex = "`fast` only works for marginal",
    fixed = TRUE
  )
})
