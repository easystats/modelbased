skip_on_cran()
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")
skip_on_os("mac")

test_that("estimate_contrasts - joint test, 2-way", {
  data(coffee_data, package = "modelbased")

  # 2 way interaction
  m <- lm(alertness ~ time * coffee, data = coffee_data)

  out1 <- estimate_contrasts(
    m,
    contrast = "time",
    by = "coffee",
    comparison = "joint"
  )
  out2 <- emmeans::joint_tests(m, "coffee")
  out3 <- estimate_contrasts(
    m,
    contrast = "time",
    by = "coffee",
    comparison = "joint",
    backend = "emmeans"
  )

  expect_identical(out1$coffee, out2$coffee)
  expect_equal(out1$`F`, out2$`F.ratio`, tolerance = 1e-3)
  expect_equal(out1$p, out2$p.value, tolerance = 1e-3)
  expect_equal(out3$Statistic, out2$`F.ratio`, tolerance = 1e-3)
  expect_equal(out3$p, out2$p.value, tolerance = 1e-3)
  expect_named(out1, c("Contrast", "coffee", "df1", "df2", "Difference", "F", "p"))
  expect_named(out3, c("Contrast", "coffee", "df1", "df2", "F", "p"))

  out1 <- estimate_contrasts(
    m,
    contrast = "coffee",
    by = "time",
    comparison = "joint"
  )
  out2 <- emmeans::joint_tests(m, "time")
  out3 <- estimate_contrasts(
    m,
    contrast = "coffee",
    by = "time",
    comparison = "joint",
    backend = "emmeans"
  )

  expect_identical(out1$time, out2$time)
  expect_equal(out1$`F`, out2$`F.ratio`, tolerance = 1e-3)
  expect_equal(out1$p, out2$p.value, tolerance = 1e-3)
  expect_equal(out3$Statistic, out2$`F.ratio`, tolerance = 1e-3)
  expect_equal(out3$p, out2$p.value, tolerance = 1e-3)
})


test_that("estimate_contrasts - joint test, p-adjust", {
  data(coffee_data, package = "modelbased")
  m <- lm(alertness ~ time * coffee, data = coffee_data)

  out1 <- estimate_contrasts(
    m,
    contrast = "time",
    by = "coffee",
    comparison = "joint"
  )
  out2 <- estimate_contrasts(
    m,
    contrast = "time",
    by = "coffee",
    comparison = "joint",
    p_adjust = "holm"
  )
  expect_all(out1$p != out2$p)
})


test_that("estimate_contrasts - joint test, Chi2 test", {
  data(coffee_data, package = "modelbased")
  m <- lm(alertness ~ time * coffee, data = coffee_data)

  out1 <- estimate_contrasts(
    m,
    contrast = "time",
    by = "coffee",
    comparison = "joint",
    test = "Chi2"
  )
  out2 <- estimate_contrasts(
    m,
    contrast = "time",
    by = "coffee",
    comparison = "joint"
  )
  expect_named(out1, c("Contrast", "coffee", "Difference", "Chi2", "df", "p"))
  expect_named(out2, c("Contrast", "coffee", "df1", "df2", "Difference", "F", "p"))
  expect_identical(dim(out1), c(2L, 6L))
})


test_that("estimate_contrasts - joint test, 3-way", {
  data(coffee_data, package = "modelbased")
  m <- lm(alertness ~ time * coffee * sex, data = coffee_data)

  out1 <- estimate_contrasts(
    m,
    contrast = "time",
    by = c("coffee", "sex"),
    comparison = "joint"
  )
  out2 <- emmeans::joint_tests(m, c("coffee", "sex"))
  out2 <- as.data.frame(out2[c(1, 3, 2, 4), ])
  out3 <- estimate_contrasts(
    m,
    contrast = "time",
    by = c("coffee", "sex"),
    comparison = "joint",
    backend = "emmeans"
  )

  expect_identical(out1$coffee, out2$coffee)
  expect_identical(out1$sex, out2$sex)
  expect_equal(out1$`F`, out2$`F.ratio`, tolerance = 1e-3)
  expect_equal(out1$p, out2$p.value, tolerance = 1e-3)
  expect_identical(dim(out1), c(4L, 8L))

  out1 <- estimate_contrasts(
    m,
    contrast = "coffee",
    by = c("time", "sex"),
    comparison = "joint"
  )
  out2 <- emmeans::joint_tests(m, c("time", "sex"))
  out2 <- as.data.frame(out2[c(1, 4, 2, 5, 3, 6), ])

  expect_identical(out1$time, out2$time)
  expect_identical(out1$sex, out2$sex)
  expect_equal(out1$`F`, out2$`F.ratio`, tolerance = 1e-3)
  expect_equal(out1$p, out2$p.value, tolerance = 1e-3)
  expect_identical(dim(out1), c(6L, 8L))
})


test_that("estimate_contrasts - joint test, 3-way", {
  data(coffee_data, package = "modelbased")
  m <- lm(alertness ~ time * coffee * sex, data = coffee_data)
  expect_error(
    estimate_contrasts(m, contrast = "time", comparison = "joint"),
    regex = "Joint tests using"
  )
})
