skip_on_cran()
skip_if_not_installed("mice")
skip_if_not_installed("marginaleffects")

test_that("pool_predictions", {
  set.seed(123)
  data("nhanes2", package = "mice")
  imp <- mice::mice(nhanes2, printFlag = FALSE)
  predictions <- lapply(1:5, function(i) {
    m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
    estimate_means(m, "age")
  })
  out <- pool_predictions(predictions)
  expect_equal(out$Mean, c(29.84661, 25.20021, 23.14022), tolerance = 1e-3)
  expect_equal(out$CI_low, c(2.10117, 3.44548, -5.79522), tolerance = 1e-3)
})


test_that("pool_conrasts", {
  set.seed(123)
  data("nhanes2", package = "mice")
  imp <- mice::mice(nhanes2, printFlag = FALSE)
  comparisons <- lapply(1:5, function(i) {
    m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
    estimate_contrasts(m, "age")
  })
  out <- pool_contrasts(comparisons)
  expect_equal(out$Mean, c(-4.6464, -6.70639, -2.05999), tolerance = 1e-3)
  expect_equal(out$CI_low, c(-9.67001, -14.71457, -8.5372), tolerance = 1e-3)
})
