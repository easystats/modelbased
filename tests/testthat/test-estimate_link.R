osx <- tryCatch({
  si <- Sys.info()
  if (!is.null(si["sysname"])) {
    si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
  } else {
    FALSE
  }
})


if (require("testthat") && require("modelbased") && require("gamm4") && require("rstanarm") && require("lme4") && require("glmmTMB") && require("mgcv")) {
  test_that("estimate_link", {

    # LMER4
    model <- lme4::lmer(Petal.Length ~ Petal.Width + (1 | Species), data = iris)
    expect_equal(nrow(modelbased::estimate_link(model, length = 5)), 5)
    expect_equal(nrow(modelbased::estimate_link(model, include_random = TRUE, preserve_range = FALSE, length = 5)), 15)

    # GLMMTMB
    model <- suppressWarnings(glmmTMB::glmmTMB(Petal.Length ~ Petal.Width + (1 | Species), data = iris))
    expect_equal(nrow(modelbased::estimate_link(model, length = 5)), 5)
    expect_equal(nrow(modelbased::estimate_link(model, include_random = TRUE, preserve_range = FALSE, length = 5)), 15)

    # MGCV
    model <- mgcv::gam(Petal.Length ~ Petal.Width + s(Sepal.Length), data = iris)
    expect_equal(nrow(modelbased::estimate_link(model, length = 3)), 9)
    expect_equal(dim(modelbased::estimate_link(model, include_smooth = FALSE, length = 3)), c(3, 4))

    model <- mgcv::gamm(Petal.Length ~ Petal.Width + s(Sepal.Length), random = list(Species = ~1), data = iris)

    # GAMM4
    model <- gamm4::gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length),
      random = ~ (1 | Species), data = iris
    )
    expect_equal(nrow(modelbased::estimate_link(model, length = 3)), 9)
    expect_equal(dim(modelbased::estimate_link(model, include_smooth = FALSE, length = 3)), c(3, 4))


    if (!osx) {
      # STAN_GAMM4
      model <- suppressWarnings(rstanarm::stan_gamm4(Petal.Length ~ Petal.Width + s(Sepal.Length),
        random = ~ (1 | Species), data = iris,
        iter = 100, chains = 2, refresh = 0
      ))
      expect_equal(nrow(modelbased::estimate_link(model, length = 3)), 9)
      expect_equal(dim(modelbased::estimate_link(model, include_smooth = FALSE, length = 3)), c(3, 4))
    }
  })
}
