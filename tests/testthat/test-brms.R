osx <- tryCatch({
  si <- Sys.info()
  if (!is.null(si["sysname"])) {
    si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
  } else {
    FALSE
  }
})

if (require("testthat") && require("modelbased") && require("brms") && require("emmeans")) {
  model <- brms::brm(Sepal.Length ~ Species * Sepal.Width, data = iris, refresh = 0, iter = 1000)

  test_that("estimate_means - brms", {
    estim <- estimate_means(model)
    expect_equal(dim(estim), c(3, 5))
  })

  test_that("estimate_relation - brms", {
    estim <- estimate_relation(model, preserve_range = FALSE)
    expect_equal(dim(estim), c(30, 6))

    # estim <- estimate_relation(model, preserve_range=FALSE, iterations = 10)
    # expect_equal(dim(estim), c(30, 6))
  })

  test_that("estimate_means - brms", {
    estim <- estimate_slopes(model, at = "Species")
    expect_equal(dim(estim), c(3, 5))
  })
}
