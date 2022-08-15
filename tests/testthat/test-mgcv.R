osx <- tryCatch({
  si <- Sys.info()
  if (!is.null(si["sysname"])) {
    si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
  } else {
    FALSE
  }
})

if (require("testthat") && require("modelbased") && require("mgcv") && require("gamm4") && require("emmeans")) {
  model <- mgcv::gam(Sepal.Length ~ Species + s(Sepal.Width, by = Species), data = iris)


  test_that("estimate_means - mgcv gam", {
    estim <- estimate_means(model)
    expect_equal(dim(estim), c(3, 5))
  })

  test_that("estimate_contrasts - mgcv gam", {
    estim <- estimate_contrasts(model)
    expect_equal(dim(estim), c(3, 9))
  })

  test_that("estimate_response - mgcv gam", {
    estim <- estimate_response(model)
    expect_equal(dim(estim), c(150, 7))
  })

  test_that("estimate_link - mgcv gam", {
    estim <- estimate_link(model, preserve_range = FALSE)
    expect_equal(dim(estim), c(30, 6))
  })

  # model <- mgcv::gam(Sepal.Length ~ Petal.Length + s(Sepal.Width) + s(Species, bs = "fs"), data = iris)
  # estim <- modelbased::estimate_link(model)

  model <- mgcv::gamm(Sepal.Length ~ Petal.Length + s(Sepal.Width), random = list(Species = ~1), data = iris)

  test_that("estimate_response - mgcv gamm", {
    estim <- estimate_response(model)
    expect_equal(dim(estim), c(150, 8))
  })

  if (!osx) {
    test_that("estimate_link - mgcv gamm", {
      estim <- estimate_link(model, length = 4)
      expect_equal(dim(estim), c(16, 6))
    })
  }


  # Gamm4 -------------------------------------------------------------------

  # model <- gamm4::gamm4(Sepal.Length ~ Petal.Length + s(Sepal.Width), random=~(1|Species), data = iris)
  #
  # test_that("estimate_response - gamm4", {
  #   estim <- estimate_response(model)
  #   expect_equal(dim(estim), c(150, 5))
  # })
  #
  # test_that("estimate_link - gamm4", {
  #   estim <- estimate_link(model, length=4)
  #   expect_equal(dim(estim), c(16, 5))
  # })
}
