if (require("testthat") && require("modelbased")) {
  test_that("visualisation_matrix - models", {

    # LMER4
    model <- lme4::lmer(Petal.Length ~ Petal.Width + (1|Species), data = iris)
    testthat::expect_equal(ncol(modelbased::visualisation_matrix(model, include_random=TRUE)), 2)
    testthat::expect_equal(ncol(modelbased::visualisation_matrix(model, include_random=FALSE)), 1)

    # GLMMTMB
    model <- glmmTMB::glmmTMB(Petal.Length ~ Petal.Width + (1|Species), data = iris)
    testthat::expect_equal(ncol(modelbased::visualisation_matrix(model, include_random=TRUE)), 2)
    testthat::expect_equal(ncol(modelbased::visualisation_matrix(model, include_random=FALSE)), 1)

    # MGCV
    model <- mgcv::gam(Petal.Length ~ Petal.Width + s(Sepal.Length), data = iris)
    testthat::expect_equal(ncol(modelbased::visualisation_matrix(model, include_random=TRUE)), 2)
    testthat::expect_equal(ncol(modelbased::visualisation_matrix(model, include_smooth=FALSE)), 1)
    testthat::expect_equal(ncol(modelbased::visualisation_matrix(model, include_smooth="fixed")), 2)

    model <- mgcv::gamm(Petal.Length ~ Petal.Width + s(Sepal.Length), random = list(Species = ~1), data = iris)
    # testthat::expect_equal(ncol(modelbased::visualisation_matrix(model, include_random=TRUE)), 3)
    # testthat::expect_equal(ncol(modelbased::visualisation_matrix(model, include_random=FALSE, include_smooth=FALSE)), 1)

    # GAMM4
    model <- gamm4::gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length), random = ~(1|Species), data = iris)
    # testthat::expect_equal(ncol(modelbased::visualisation_matrix(model, include_random=TRUE)), 3)
    testthat::expect_equal(ncol(modelbased::visualisation_matrix(model, include_random=FALSE, include_smooth="fixed")), 2)
    testthat::expect_equal(ncol(modelbased::visualisation_matrix(model, include_random=FALSE, include_smooth=FALSE)), 1)



    # STAN_GAMM4
    model <- rstanarm::stan_gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length), random=~(1|Species), data = iris, iter=100, chains=2, refresh=0)
    testthat::expect_equal(ncol(modelbased::visualisation_matrix(model, include_random=TRUE)), 3)
    testthat::expect_equal(ncol(modelbased::visualisation_matrix(model, include_random=FALSE, include_smooth="fixed")), 2)
    testthat::expect_equal(ncol(modelbased::visualisation_matrix(model, include_random=FALSE, include_smooth=FALSE)), 1)
  })
}
