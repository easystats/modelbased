skip_if_not_installed("marginaleffects")
skip_if_not_installed("MASS")

test_that("verbose", {
  set.seed(1)
  newdata <- data.frame(
    y = c(602, 38, 616, 256, 21, 723, 245, 176, 89, 1614, 31, 27, 313, 251, 345),
    x = as.factor(sample(letters[1:3], 15, replace = TRUE)),
    offset_1 = c(72, 50, 31, 30, 16, 25, 75, 16, 78, 40, 68, 25, 71, 52, 17)
  )

  m <- MASS::glm.nb(y ~ x, data = newdata)
  out1 <- estimate_means(m, "x")
  out2 <- estimate_means(m, "x", estimate = "average")
  expect_equal(out1$Mean, out2$Mean, tolerance = 1e-3)

  moff <- MASS::glm.nb(y ~ x + offset(log(offset_1)), data = newdata)
  expect_message(
    {
      out1 <- estimate_means(moff, "x")
    },
    regex = "Model contains an offset-term"
  )
  out2 <- estimate_means(moff, "x", estimate = "average")
  expect_equal(out1$Mean, c(295.12035, 454.3339, 654.64225), tolerance = 1e-3)
  expect_equal(out2$Mean, c(256.42016, 289.02697, 707.83022), tolerance = 1e-3)

  set.seed(1)
  newdata <- data.frame(
    y = c(602, 38, 616, 256, 21, 723, 245, 176, 89, 1614, 31, 27, 313, 251, 345),
    x = as.factor(sample(letters[1:3], 15, replace = TRUE)),
    offset_1 = rep_len(50, 15)
  )
  moff <- MASS::glm.nb(y ~ x + offset(log(offset_1)), data = newdata)
  out1 <- estimate_means(moff, "x", verbose = FALSE)
  out2 <- estimate_means(moff, "x", estimate = "average")
  expect_equal(out1$Mean, out2$Mean, tolerance = 1e-3)
})
