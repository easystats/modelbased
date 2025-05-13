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

  out <- estimate_contrasts(estim, by = c("age", "employed"))
  expect_identical(dim(out), c(6L, 10L))
  expect_named(
    out,
    c(
      "Level1", "Level2", "age", "employed", "Difference", "SE", "CI_low",
      "CI_high", "Statistic", "p"
    )
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
