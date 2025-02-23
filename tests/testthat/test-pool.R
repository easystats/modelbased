skip_on_cran()
skip_on_os(c("mac", "linux"))
skip_if_not_installed("marginaleffects")
skip_if_not_installed("mice")


test_that("pool_contrasts", {
  data("nhanes2", package = "mice")
  set.seed(333)
  imp <- mice::mice(nhanes2, printFlag = FALSE)
  comparisons <- lapply(1:5, function(i) {
    m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
    estimate_contrasts(m, "age")
  })
  out <- pool_contrasts(comparisons)
  expect_equal(out$Difference, c(-6.15852, -7.87353, -1.71501), tolerance = 1e-4)
  expect_equal(out$SE, c(1.65237, 2.16773, 2.06655), tolerance = 1e-4)
})


test_that("pool_predictions", {
  data("nhanes2", package = "mice")
  set.seed(333)
  imp <- mice::mice(nhanes2, printFlag = FALSE)
  comparisons <- lapply(1:5, function(i) {
    m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
    estimate_means(m, "age")
  })
  out <- pool_predictions(comparisons)
  expect_equal(out$Mean, c(30.87476, 24.71624, 23.00122), tolerance = 1e-4)
  expect_equal(out$SE, c(1.20644, 1.24891, 1.54681), tolerance = 1e-4)
})
