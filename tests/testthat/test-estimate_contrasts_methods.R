skip_on_cran()
skip_on_os("mac")
skip_if_not_installed("glmmTMB")
# skip_if_not(interactive())


test_that("estimate_contrasts - Random Effects Levels, pairwise", {
  # sample data set
  data(efc, package = "modelbased")

  # numeric to factors, set labels as levels
  d <- datawizard::to_factor(efc, select = c("c161sex", "c172code", "c175empl"))
  # recode age into three groups
  d <- datawizard::recode_values(
    d,
    select = "c160age",
    recode = list(`1` = "min:40", `2` = 41:64, `3` = "65:max")
  )
  # rename variables
  d <- datawizard::data_rename(
    d,
    select = c("c161sex", "c160age", "quol_5", "c175empl"),
    replacement = c("gender", "age", "qol", "employed")
  )
  # age into factor, set levels, and change labels for education
  d <- datawizard::data_modify(d, age = factor(age, labels = c("-40", "41-64", "65+")))
  dat <<- d

  # Quality of Life score ranges from 0 to 25
  m_null <- glmmTMB::glmmTMB(qol ~ 1 + (1 | gender:employed:age), data = dat)
  estim <- estimate_relation(m_null, by = c("gender", "employed", "age"))

  # test errors
  expect_error(estimate_contrasts(estim, "employed", comparison = ~reference), regex = "Invalid option for argument")
  expect_error(estimate_contrasts(estim, by = "agea"), regex = "Following variables specified")
  expect_error(estimate_contrasts(estim, contrast = "gndr", by = "age"), regex = "Following variables specified")

  # extract contrast from attribute
  out <- estimate_contrasts(estim)
  expect_identical(dim(out), c(66L, 8L))

  # test output
  expect_snapshot(print(estimate_contrasts(estim, contrast = c("gender", "employed", "age")), zap_small = TRUE, table_width = Inf))
  expect_snapshot(print(estimate_contrasts(estim, contrast = c("gender", "employed"), by = "age"), zap_small = TRUE, table_width = Inf))
  expect_snapshot(print(estimate_contrasts(estim, contrast = "employed", by = c("age", "gender")), zap_small = TRUE, table_width = Inf))

  out <- estimate_contrasts(estim, by = "age")
  expect_identical(dim(out), c(18L, 9L))
  expect_named(
    out,
    c(
      "Level1", "Level2", "age", "Difference", "SE", "CI_low", "CI_high",
      "Statistic", "p"
    )
  )
  out <- estimate_contrasts(estim, contrast = c("gender", "employed"))
  expect_identical(dim(out), c(18L, 9L))
  expect_named(
    out,
    c(
      "Level1", "Level2", "age", "Difference", "SE", "CI_low", "CI_high",
      "Statistic", "p"
    )
  )

  out <- estimate_contrasts(estim, by = c("age", "employed"))
  expect_identical(dim(out), c(6L, 10L))
  expect_named(
    out,
    c(
      "Level1", "Level2", "age", "employed", "Difference", "SE", "CI_low",
      "CI_high", "Statistic", "p"
    )
  )
  out <- estimate_contrasts(estim, contrast = "gender")
  expect_identical(dim(out), c(6L, 10L))
  expect_named(
    out,
    c(
      "Level1", "Level2", "employed", "age", "Difference", "SE", "CI_low",
      "CI_high", "Statistic", "p"
    )
  )

  # message
  expect_message(
    estimate_contrasts(m_null, contrast = "employed", by = c("age", "gender")),
    regex = "Could not calculate"
  )
  expect_silent(
    estimate_contrasts(m_null, contrast = "employed", by = c("age", "gender"), verbose = FALSE)
  )
})


test_that("estimate_contrasts - Random Effects Levels, interaction", {
  # sample data set
  data(efc, package = "modelbased")

  # numeric to factors, set labels as levels
  d <- datawizard::to_factor(efc, select = c("c161sex", "c172code", "c175empl"))
  # recode age into three groups
  d <- datawizard::recode_values(
    d,
    select = "c160age",
    recode = list(`1` = "min:40", `2` = 41:64, `3` = "65:max")
  )
  # rename variables
  d <- datawizard::data_rename(
    d,
    select = c("c161sex", "c160age", "quol_5", "c175empl"),
    replacement = c("gender", "age", "qol", "employed")
  )
  # age into factor, set levels, and change labels for education
  d <- datawizard::data_modify(d, age = factor(age, labels = c("-40", "41-64", "65+")))
  dat <<- d

  # Quality of Life score ranges from 0 to 25
  m_null <- glmmTMB::glmmTMB(qol ~ 1 + (1 | gender:employed:age), data = dat)
  estim <- estimate_relation(m_null, by = c("age", "employed"))
  expect_snapshot(print(estimate_contrasts(estim, contrast = c("age", "employed"), comparison = "interaction"), zap_small = TRUE, table_width = Inf))
})


test_that("estimate_contrasts - interaction", {
  data(efc, package = "modelbased")
  efc$c172code <- as.factor(efc$c172code)
  efc$c161sex <- as.factor(efc$c161sex)
  efc$e15relat <- as.factor(efc$e15relat)
  efc$e42dep <- as.factor(efc$e42dep)
  levels(efc$c161sex) <- c("male", "female")

  # multiple focal terms, interaction
  m <- lm(barthtot ~ c12hour + neg_c_7 + c161sex * c172code, data = efc)

  # difference-in-difference
  estim <- estimate_relation(m, by = c("c172code", "c161sex"))
  out <- estimate_contrasts(estim, comparison = "interaction")

  expect_identical(dim(out), c(3L, 8L))
  expect_identical(out$c172code, c("1-2", "1-3", "2-3"))
  expect_identical(out$c161sex, c("male and female", "male and female", "male and female"))
  expect_equal(out$Difference, c(-1.28159, 3.02394, 4.30553), tolerance = 1e-4)
})


test_that("estimate_contrasts - glm", {
  set.seed(5)
  data <- data.frame(
    outcome = rbinom(100, 1, 0.5),
    var1 = as.factor(rbinom(100, 1, 0.1)),
    var2 = rnorm(100, 10, 7)
  )
  m <- glm(
    outcome ~ var1 + var2,
    data = data,
    family = binomial(link = "logit")
  )
  estim <- estimate_relation(m, by = "var1")
  out1 <- estimate_contrasts(estim, contrast = "var1")
  out2 <- estimate_contrasts(m, contrast = "var1")
  expect_equal(out1$Difference, out2$Difference * -1, tolerance = 1e-4)
  expect_lt(abs(out1$SE - out2$SE), 0.01)
})
