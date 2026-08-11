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


test_that("weighted data grids work for average, binning of numerics", {
  data(penguins)

  # one factor
  model <- lm(bill_len ~ species + sex + island + body_mass, data = penguins)
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

  # need lower tolerance, due to binning not being exact to the empirical
  # average of numeric values in the data
  expect_equal(emm1$Mean, emm2$Mean, tolerance = 1e-3)
  expect_equal(emm2$Mean, emm3$Mean, tolerance = 1e-5)

  dg <- insight::get_datagrid(model, n_bins = 15, weighted = TRUE)
  emm2 <- estimate_means(
    model,
    "species",
    estimate = "average",
    data = dg,
    weights = dg$Weight
  )
  emm3 <- estimate_means(model, "species", estimate = "average", fast = 15)

  # need lower tolerance, due to binning not being exact to the empirical
  # average of numeric values in the data, but tolerance is stricter
  # due to more precise binning
  expect_equal(emm1$Mean, emm2$Mean, tolerance = 1e-4)
  expect_equal(emm2$Mean, emm3$Mean, tolerance = 1e-5)
})


test_that("weighted data grids work for average and model weights, binning of numerics", {
  set.seed(123)
  d <- penguins
  d$weights <- abs(rnorm(nrow(d), 1, 0.2))
  model <- lm(body_mass ~ species + sex + bill_len, data = d, weights = weights)

  dg <- insight::get_datagrid(model, weighted = "weights")

  emm1 <- estimate_means(
    model,
    "species",
    estimate = "average",
    data = dg,
    weights = dg$Weight
  )
  emm2 <- estimate_means(model, "species", estimate = "average", fast = TRUE)
  expect_equal(emm1$Mean, emm2$Mean, tolerance = 1e-5)

  dg <- insight::get_datagrid(model, weighted = "weights", n_bins = 10)

  emm1 <- estimate_means(
    model,
    "species",
    estimate = "average",
    data = dg,
    weights = dg$Weight
  )
  emm2 <- estimate_means(model, "species", estimate = "average", fast = 10)
  expect_equal(emm1$Mean, emm2$Mean, tolerance = 1e-5)
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
