if (require("testthat") && require("modelbased") && require("gamm4") && require("glmmTMB") && require("mgcv") && require("rstanarm")) {
  test_that("visualisation_matrix - models", {

    # LMER4
    model <- lme4::lmer(Petal.Length ~ Petal.Width + (1 | Species), data = iris)
    expect_equal(ncol(modelbased::visualisation_matrix(model, include_random = TRUE)), 2)
    expect_equal(ncol(modelbased::visualisation_matrix(model, include_random = FALSE)), 1)

    # GLMMTMB
    model <- suppressWarnings(glmmTMB::glmmTMB(Petal.Length ~ Petal.Width + (1 | Species), data = iris))
    expect_equal(ncol(modelbased::visualisation_matrix(model, include_random = TRUE)), 2)
    expect_equal(ncol(modelbased::visualisation_matrix(model, include_random = FALSE)), 1)

    # MGCV
    model <- mgcv::gam(Petal.Length ~ Petal.Width + s(Sepal.Length), data = iris)
    expect_equal(ncol(modelbased::visualisation_matrix(model, include_random = TRUE)), 2)
    expect_equal(ncol(modelbased::visualisation_matrix(model, include_smooth = FALSE)), 1)
    expect_equal(ncol(modelbased::visualisation_matrix(model, include_smooth = "fixed")), 2)

    model <- mgcv::gamm(Petal.Length ~ Petal.Width + s(Sepal.Length), random = list(Species = ~1), data = iris)
    # expect_equal(ncol(modelbased::visualisation_matrix(model, include_random=TRUE)), 3)
    expect_equal(ncol(modelbased::visualisation_matrix(model, include_random = FALSE, include_smooth = FALSE)), 1)

    # GAMM4
    model <- gamm4::gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length), random = ~ (1 | Species), data = iris)
    # expect_equal(ncol(modelbased::visualisation_matrix(model, include_random=TRUE)), 3)
    expect_equal(ncol(modelbased::visualisation_matrix(model, include_random = FALSE, include_smooth = "fixed")), 2)
    expect_equal(ncol(modelbased::visualisation_matrix(model, include_random = FALSE, include_smooth = FALSE)), 1)



    # STAN_GAMM4
    model <- suppressWarnings(rstanarm::stan_gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length), random = ~ (1 | Species), data = iris, iter = 100, chains = 2, refresh = 0))
    expect_equal(ncol(modelbased::visualisation_matrix(model, include_random = TRUE)), 3)
    expect_equal(ncol(modelbased::visualisation_matrix(model, include_random = FALSE, include_smooth = "fixed")), 2)
    expect_equal(ncol(modelbased::visualisation_matrix(model, include_random = FALSE, include_smooth = FALSE)), 1)
  })
}
