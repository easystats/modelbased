test_that("zero_crossings", {
  x <- sin(seq(0, 4 * pi, length.out = 100))
  out <- zero_crossings(x)
  expect_equal(out, c(1, 25.74975, 50.5, 75.25025), tolerance = 1e-4)
  out <- find_inversions(x)
  expect_equal(out, c(12.87478, 37.62484, 62.37516, 87.12522), tolerance = 1e-4)
  expect_true(is.na(zero_crossings(c(1, 2))))
  excpect_error(zero_crossings(1), regex = "is not smaller")
})
