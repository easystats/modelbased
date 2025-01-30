skip_on_cran()
skip_on_os("mac")
skip_if_not_installed("glmmTMB")
skip_if(getRversion() < "4.1.0") # pipe

test_that("estimate_contrasts - Random Effects Levels, pairwise", {
  # sample data set
  data(efc, package = "modelbased")

  efc <- efc |>
    # numeric to factors, set labels as levels
    datawizard::to_factor(select = c("c161sex", "c172code", "c175empl")) |>
    # recode age into three groups
    datawizard::recode_values(
      select = "c160age",
      recode = list(`1` = "min:40", `2` = 41:64, `3` = "65:max")
    ) |>
    # rename variables
    datawizard::data_rename(
      select = c("c161sex", "c160age", "quol_5", "c175empl"),
      replacement = c("gender", "age", "qol", "employed")
    ) |>
    # age into factor, set levels, and change labels for education
    datawizard::data_modify(age = factor(age, labels = c("-40", "41-64", "65+")))

  # Quality of Life score ranges from 0 to 25
  m_null <- glmmTMB::glmmTMB(qol ~ 1 + (1 | gender:employed:age), data = efc)
  estim <- estimate_relation(m_null, by = c("gender", "employed", "age"))

  expect_error(estimate_contrasts(estim), regex = "must be specified")
  expect_snapshot(print(estimate_contrasts(estim, contrast = c("gender", "employed", "age")), zap_small = TRUE, table_width = Inf))
  expect_snapshot(print(estimate_contrasts(estim, contrast = c("gender", "employed"), by = "age"), zap_small = TRUE, table_width = Inf))
  expect_snapshot(print(estimate_contrasts(estim, contrast = "employed", by = c("age", "gender")), zap_small = TRUE, table_width = Inf))
})


test_that("estimate_contrasts - Random Effects Levels, interaction", {
  # sample data set
  data(efc, package = "modelbased")

  efc <- efc |>
    # numeric to factors, set labels as levels
    datawizard::to_factor(select = c("c161sex", "c172code", "c175empl")) |>
    # recode age into three groups
    datawizard::recode_values(
      select = "c160age",
      recode = list(`1` = "min:40", `2` = 41:64, `3` = "65:max")
    ) |>
    # rename variables
    datawizard::data_rename(
      select = c("c161sex", "c160age", "quol_5", "c175empl"),
      replacement = c("gender", "age", "qol", "employed")
    ) |>
    # age into factor, set levels, and change labels for education
    datawizard::data_modify(age = factor(age, labels = c("-40", "41-64", "65+")))

  m_null <- glmmTMB::glmmTMB(qol ~ 1 + (1 | employed:age), data = efc)
  estim <- estimate_relation(m_null, by = c("age", "employed"))
  expect_snapshot(print(estimate_contrasts(estim, contrast = c("age", "employed"), comparison = "interaction"), zap_small = TRUE, table_width = Inf))
})
