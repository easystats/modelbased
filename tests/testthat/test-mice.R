skip_on_cran()
skip_if_not_installed("mice")
skip_if_not_installed("marginaleffects", minimum_version = "0.28.0.22")

test_that("pool_predictions", {
  set.seed(123)
  data("nhanes2", package = "mice")
  imp <- mice::mice(nhanes2, printFlag = FALSE)
  predictions <- lapply(1:5, function(i) {
    m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
    estimate_means(m, "age")
  })
  out <- pool_predictions(predictions)
  expect_equal(out$Mean, c(29.84661, 25.20021, 23.14022), tolerance = 1e-2)
  expect_equal(out$CI_low, c(2.10117, 3.44548, -5.79522), tolerance = 1e-2)

  # transformed response
  predictions <- lapply(1:5, function(i) {
    m <- lm(log1p(bmi) ~ age + hyp + chl, data = mice::complete(imp, action = i))
    estimate_means(m, "age")
  })
  out <- pool_predictions(predictions, transform = TRUE)
  expect_equal(out$Mean, c(29.67473, 24.99382, 23.19148), tolerance = 1e-2)
  expect_equal(out$CI_low, c(10.58962, 11.13011, 7.43196), tolerance = 1e-2)

  # glm, logistic
  predictions <- lapply(1:5, function(i) {
    m <- glm(hyp ~ age + chl, data = mice::complete(imp, action = i), family = binomial())
    estimate_means(m, "age")
  })
  expect_warning(pool_predictions(predictions), regex = "Could not extract")

  predictions <- lapply(1:5, function(i) {
    m <- glm(hyp ~ age + chl, data = mice::complete(imp, action = i), family = binomial())
    estimate_means(m, "age", type = "response")
  })
  expect_silent(pool_predictions(predictions))
})


test_that("pool_contrasts", {
  set.seed(123)
  data("nhanes2", package = "mice")
  imp <- mice::mice(nhanes2, printFlag = FALSE)
  comparisons <- lapply(1:5, function(i) {
    m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
    estimate_contrasts(m, "age")
  })
  out <- pool_contrasts(comparisons)
  expect_equal(out$Difference, c(-4.6464, -6.70639, -2.05999), tolerance = 1e-2)
  expect_equal(out$CI_low, c(-12.31066, -18.92406, -11.94194), tolerance = 1e-2)
  expect_equal(out$p, c(0.14926, 0.17899, 0.55449), tolerance = 1e-2)
})


test_that("pool_slopes", {
  set.seed(123)
  data("nhanes2", package = "mice")
  imp <- mice::mice(nhanes2, printFlag = FALSE)
  slopes <- lapply(1:5, function(i) {
    m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
    estimate_slopes(m, "chl")
  })
  out <- pool_slopes(slopes)
  expect_equal(out$Slope, 0.05666, tolerance = 1e-2)
  expect_equal(out$CI_low, 0.0005592606, tolerance = 1e-2)
  expect_named(
    out,
    c("Slope", "SE", "CI_low", "CI_high", "t", "df", "p")
  )

  slopes <- lapply(1:5, function(i) {
    m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
    estimate_slopes(m, "age")
  })
  out <- pool_slopes(slopes)
  expect_equal(out$Slope, c(-4.6464, -6.70639), tolerance = 1e-2)
  expect_equal(out$CI_low, c(-10.83711, -16.57506), tolerance = 1e-2)
  expect_named(
    out,
    c("Comparison", "Slope", "SE", "CI_low", "CI_high", "t", "df", "p")
  )
})
