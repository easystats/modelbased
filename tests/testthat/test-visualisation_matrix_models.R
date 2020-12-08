if (require("testthat") && require("modelbased")) {
  test_that("visualisation_matrix - models", {

    model <- lme4::lmer(Petal.Length ~ Petal.Width + (1|Species), data = iris)
    testthat::expect_equal(names(modelbased::visualisation_matrix(model, include_random=FALSE)), c("Petal.Width"))

    model <- glmmTMB::glmmTMB(Petal.Length ~ Petal.Width + (1|Species), data = iris)
    testthat::expect_equal(names(modelbased::visualisation_matrix(model, include_random=FALSE)), c("Petal.Width"))
  })
}
