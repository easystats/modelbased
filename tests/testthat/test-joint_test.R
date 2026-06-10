skip_on_cran()
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
skip_on_os("mac")

test_that("estimate_contrasts - joint test, 2-way", {
  data(coffee_data, package = "modelbased")

  # 2 way interaction
  m <- lm(alertness ~ time * coffee, data = coffee_data)

  out1 <- estimate_contrasts(m, contrast = "time", by = "coffee", comparison = "joint")
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
  expect_identical(out3$`F`, as.character(round(out2$`F.ratio`, 2)))
  expect_equal(out3$p, out2$p.value, tolerance = 1e-3)
  expect_named(out1, c("Contrast", "coffee", "df1", "df2", "Difference", "F", "p"))
  expect_named(out3, c("Contrast", "coffee", "df1", "df2", "F", "p"))

  out1 <- estimate_contrasts(m, contrast = "coffee", by = "time", comparison = "joint")
  out2 <- emmeans::joint_tests(m, "time")

  expect_identical(out1$time, out2$time)
  expect_equal(out1$`F`, out2$`F.ratio`, tolerance = 1e-3)
  expect_equal(out1$p, out2$p.value, tolerance = 1e-3)

  # omnibus test
  out <- estimate_contrasts(m, contrast = "time", comparison = "omnibus")
  expect_equal(out$`F`, 237.2881, tolerance = 1e-3)
  expect_equal(out$p, 0, tolerance = 1e-3)

  out <- estimate_contrasts(m, contrast = "time", comparison = "omnibus", null = 15)
  expect_equal(out$`F`, 3.71661, tolerance = 1e-3)
  expect_equal(out$p, 0.01356, tolerance = 1e-3)

  set.seed(5)
  data <- data.frame(
    outcome = rbinom(100, 1, 0.5),
    var1 = rbinom(100, 1, 0.1),
    var2 = rnorm(100, 10, 7)
  )
  m <- glm(outcome ~ var1 + var2, data = data, family = binomial(link = "logit"))
  out1 <- estimate_contrasts(m, contrast = "var1", comparison = "omnibus")
  out2 <- estimate_contrasts(
    m,
    contrast = "var1",
    comparison = "omnibus",
    predict = "response"
  )
  expect_equal(out1$p, out2$p, tolerance = 1e-4)
  out3 <- estimate_contrasts(
    m,
    contrast = "var1",
    comparison = "omnibus",
    predict = "invlink(link)"
  )
  expect_equal(out3$p, 0.9912332, tolerance = 1e-4)
})


test_that("estimate_contrasts - joint test, p-adjust", {
  data(coffee_data, package = "modelbased")
  m <- lm(alertness ~ time * coffee, data = coffee_data)

  out1 <- estimate_contrasts(m, contrast = "time", by = "coffee", comparison = "joint")
  out2 <- estimate_contrasts(
    m,
    contrast = "time",
    by = "coffee",
    comparison = "joint",
    p_adjust = "holm"
  )
  expect_true(any(out1$p != out2$p))
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
  out2 <- estimate_contrasts(m, contrast = "time", by = "coffee", comparison = "joint")
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
  out3 <- as.data.frame(out3[c(1, 3, 2, 4), ])

  expect_identical(out1$coffee, out2$coffee)
  expect_identical(out1$coffee, out3$coffee)
  expect_identical(out1$sex, out2$sex)
  expect_identical(out1$sex, out3$sex)
  expect_equal(out1$`F`, out2$`F.ratio`, tolerance = 1e-3)
  expect_identical(as.character(round(out1$`F`, 2)), out3$`F`)
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


test_that("estimate_contrasts - joint test, 3-way, error", {
  data(coffee_data, package = "modelbased")
  m <- lm(alertness ~ time * coffee * sex, data = coffee_data)
  expect_error(
    estimate_contrasts(m, contrast = "time", comparison = "joint"),
    regex = "Joint tests using"
  )
})


test_that("estimate_contrasts - joint test, correct df for anova", {
  skip_if_not_installed("afex")
  skip_if_not_installed("discovr")

  date_tib <- discovr::speed_date
  date_afx1 <- suppressWarnings(afex::aov_4(
    date ~ strategy * looks + (looks | id),
    data = date_tib
  ))
  out <- estimate_contrasts(
    date_afx1,
    contrast = "looks",
    by = "strategy",
    comparison = "joint"
  )
  expect_identical(out$df2, c("18", "18"))
})

skip_if_not_installed("withr")

withr::with_options(
  list(modelbased_select = "minimal"),
  test_that("estimate_contrasts - joint test, works with select printing", {
    data(coffee_data, package = "modelbased")
    m <- lm(alertness ~ time * coffee, data = coffee_data)
    expect_snapshot(estimate_contrasts(
      m,
      contrast = "time",
      by = "coffee",
      comparison = "joint"
    ))
  })
)
