skip_on_cran()
skip_if_not_installed("glmmTMB")
skip_if_not_installed("datawizard")

test_that("maihda", {
  # sample data set
  data(efc, package = "modelbased")

  efc <- datawizard::to_factor(efc, select = c("c161sex", "c172code", "c175empl"))
  efc <- datawizard::recode_values(
    efc,
    select = "c160age",
    recode = list(`1` = "min:40", `2` = 41:64, `3` = "65:max")
  )
  efc <- datawizard::data_rename(
    efc,
    select = c("c161sex", "c160age", "quol_5", "c175empl"),
    replacement = c("gender", "age", "qol", "employed")
  )
  efc <- datawizard::data_modify(efc, age = factor(age, labels = c("-40", "41-64", "65+")))

  set.seed(1)
  efc$weights <- abs(rnorm(nrow(efc), mean = 1, sd = 0.1))

  m_null <- glmmTMB::glmmTMB(
    qol ~ 1 + (1 | gender:employed:age),
    data = efc,
    weights = weights
  )

  out <- estimate_relation(m_null, by = c("gender", "employed", "age"))
  expect_identical(dim(out), c(12L, 9L))
  out <- estimate_contrasts(out, contrast = c("gender", "employed", "age"))
  expect_identical(dim(out), c(66L, 8L))
})
