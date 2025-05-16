skip_on_cran()
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")

test_that("estimate_means() - mixed models", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")

  data(iris)
  dat <- iris
  dat$Petal.Length_factor <- as.factor(ifelse(dat$Petal.Length < 4.2, "A", "B"))
  dat <<- dat

  model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = dat)

  estim1 <- estimate_means(model, backend = "emmeans", verbose = FALSE)
  expect_identical(dim(estim1), c(3L, 5L))
  estim2 <- estimate_means(model, backend = "marginaleffects", verbose = FALSE)
  expect_identical(dim(estim2), c(3L, 7L))
  expect_lt(max(estim1$Mean - estim2$Mean), 1e-10)

  model <- lme4::glmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = dat, family = "Gamma")
  estim1 <- estimate_means(model, backend = "emmeans", verbose = FALSE)
  expect_identical(dim(estim1), c(3L, 5L))
  estim2 <- estimate_means(model, backend = "marginaleffects", verbose = FALSE)
  expect_identical(dim(estim2), c(3L, 7L))
  expect_lt(max(estim1$Mean - estim2$Mean), 1e-10)

  data(Salamanders, package = "glmmTMB")
  m <- glmmTMB::glmmTMB(
    count ~ mined * spp + (1 | site),
    zi = ~mined,
    family = poisson(),
    data = Salamanders
  )
  expect_snapshot(estimate_means(m, c("mined", "spp"), backend = "marginaleffects", predict = "inverse_link"))
  expect_snapshot(estimate_means(m, c("mined", "spp"), backend = "marginaleffects"))
  out <- estimate_means(m, c("mined", "spp"), backend = "marginaleffects", predict = "inverse_link")
  expect_true(all(out$CI_low >= 0))
  expect_true(all(out$CI_high >= 0))

  out1 <- estimate_means(m, c("mined", "spp"), type = "conditional", backend = "marginaleffects")
  out2 <- estimate_means(m, c("mined", "spp"), backend = "emmeans")
  expect_equal(out1$Mean[order(out1$spp)], out2$Rate, tolerance = 1e-1)

  m <- glm(count ~ mined + spp, family = poisson(), data = Salamanders)
  expect_snapshot(estimate_means(m, c("mined", "spp"), backend = "marginaleffects"))
  out1 <- estimate_means(m, c("mined", "spp"), backend = "marginaleffects")
  out2 <- estimate_means(m, c("mined", "spp"), backend = "emmeans")
  expect_equal(out1$Mean[order(out1$spp)], out2$Rate, tolerance = 1e-3)

  data(sleepstudy, package = "lme4")
  model <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

  estim1 <- estimate_means(model, by = "Days", backend = "emmeans")
  estim2 <- estimate_means(model, by = "Days", backend = "marginaleffects")
  expect_identical(dim(estim1), c(10L, 5L))
  expect_identical(dim(estim2), c(10L, 7L))
  expect_equal(estim1$Mean, estim2$Mean, tolerance = 1e-4)
  expect_equal(estim1$CI_low, estim2$CI_low, tolerance = 1e-1)

  data(cbpp, package = "lme4")
  gm1 <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = "binomial"
  )
  estim1 <- estimate_means(gm1, backend = "emmeans", verbose = FALSE)
  estim2 <- estimate_means(gm1, backend = "marginaleffects", verbose = FALSE, predict = "inverse_link")
  expect_identical(dim(estim1), c(4L, 5L))
  expect_identical(dim(estim2), c(4L, 6L))
  expect_equal(estim2$Probability, c(0.20293, 0.08627, 0.07612, 0.04984), tolerance = 1e-3)
  expect_equal(estim2$CI_low, c(0.13928, 0.04915, 0.04159, 0.02229), tolerance = 1e-3)
  expect_true(all(estim2$CI_low >= 0 & estim2$CI_low <= 1))
  expect_true(all(estim2$CI_high >= 0 & estim2$CI_high <= 1))

  estim3 <- estimate_means(gm1, backend = "marginaleffects", verbose = FALSE)
  expect_identical(dim(estim3), c(4L, 6L))
  expect_equal(estim3$Probability, c(0.21521, 0.0954, 0.08453, 0.05599), tolerance = 1e-3)
  expect_equal(estim3$CI_low, c(0.14233, 0.04475, 0.03608, 0.01266), tolerance = 1e-3)
})


test_that("estimate_contrasts - Random Effects Levels, pairwise", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("datawizard")

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
  expect_message(
    estimate_means(m_null, by = c("gender", "employed", "age")),
    regex = "Standard errors are probably not reliable"
  )
  expect_silent(
    estimate_means(m_null, by = c("gender", "employed", "age"), verbose = FALSE)
  )
  out <- estimate_relation(m_null, by = c("gender", "employed", "age"))
  expect_true(any(out$SE != out$SE[1]))
})
