skip_on_os(c("mac", "linux"))
skip_if_not_installed("marginaleffects", minimum_version = "0.28.0.22")
skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("scoping issues", {
    data(iris)
    model <- lm(Sepal.Width ~ Species, data = iris)

    out1 <- estimate_contrasts(model, backend = "marginaleffects")

    contrast <- NULL
    out2 <- estimate_contrasts(model, backend = "marginaleffects")

    expect_equal(out1$Difference, out1$Difference, tolerance = 1e-4)
  })
)
