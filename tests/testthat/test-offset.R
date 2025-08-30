skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
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
    regex = "which is set to"
  )
  expect_message(
    {
      out2 <- estimate_means(moff, "x", estimate = "average")
    },
    regex = "and you average"
  )
  expect_equal(out1$Mean, c(295.12035, 454.3339, 654.64225), tolerance = 1e-3)
  expect_equal(out2$Mean, c(256.42016, 289.02697, 707.83022), tolerance = 1e-3)

  expect_message(
    {
      estimate_means(moff, "x")
    },
    regex = "We also found"
  )

  expect_silent({
    out1 <- estimate_means(moff, "x", offset = 100)
  })
  expect_message(
    {
      out2 <- estimate_means(moff, "x", estimate = "average", offset = 100)
    },
    regex = "For"
  )
  expect_equal(out1$Mean, c(664.68547, 1023.27456, 1474.41949), tolerance = 1e-3)
  expect_equal(out2$Mean, c(256.42016, 289.02697, 707.83022), tolerance = 1e-3)

  set.seed(1)
  newdata <- data.frame(
    y = c(602, 38, 616, 256, 21, 723, 245, 176, 89, 1614, 31, 27, 313, 251, 345),
    x = as.factor(sample(letters[1:3], 15, replace = TRUE)),
    offset_1 = rep_len(50, 15)
  )
  moff <- MASS::glm.nb(y ~ x + offset(log(offset_1)), data = newdata)
  expect_silent({
    out1 <- estimate_means(moff, "x", verbose = FALSE)
  })
  expect_silent({
    out2 <- estimate_means(moff, "x", estimate = "average", verbose = FALSE)
  })
  expect_equal(out1$Mean, out2$Mean, tolerance = 1e-3)
})


test_that("offset, estimate_relation", {
  set.seed(1)
  newdata <- data.frame(
    y = c(602, 38, 616, 256, 21, 723, 245, 176, 89, 1614, 31, 27, 313, 251, 345),
    x = as.factor(sample(letters[1:3], 15, replace = TRUE)),
    offset_1 = c(72, 50, 31, 30, 16, 25, 75, 16, 78, 40, 68, 25, 71, 52, 17)
  )

  moff <- MASS::glm.nb(y ~ x + offset(log(offset_1)), data = newdata)
  out <- estimate_relation(moff, by = "x")
  expect_equal(attributes(out)$datagrid$offset_1, c(44.4, 44.4, 44.4), tolerance = 1e-3)
  out <- estimate_relation(moff, by = "x", offset = 100)
  expect_equal(attributes(out)$datagrid$offset_1, c(100, 100, 100), tolerance = 1e-3)
})
