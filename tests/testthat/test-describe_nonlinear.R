skip_if_not_installed("performance")

test_that("describe_nonlinear", {
  set.seed(123)
  d <- data.frame(x = rnorm(200))
  d$y <- d$x^2 + rnorm(200, 0, 0.5)
  model <- lm(y ~ poly(x, 2), data = d)
  link_data <- estimate_relation(model, length = 100)
  out <- describe_nonlinear(link_data, x = "x")

  expect_equal(out$Start, c(-2.309, -0.011), tolerance = 1e-4)
  expect_equal(out$End, c(-0.011, 3.241), tolerance = 1e-4)
  expect_error(describe_nonlinear(link_data), regex = "The name of the predictor")
  expect_error(describe_nonlinear(link_data, x = "x", y = "test"), regex = "The name of the response")
})

# test_that("estimate_smooth", {
#   skip_on_cran()
#   skip_if_not_installed("rstanarm")
#   set.seed(333)
#
#   model <-
#     suppressWarnings(
#       rstanarm::stan_gamm4(
#         Sepal.Width ~ s(Petal.Length),
#         data = iris,
#         refresh = 0,
#         iter = 200,
#         chains = 2,
#         seed = 333
#       )
#     )
#   estim <- estimate_smooth(model)
#   expect_equal(ncol(estim), 6)
#
#   model <-
#     suppressWarnings(
#       rstanarm::stan_glm(
#         Sepal.Width ~ poly(Petal.Length, 2),
#         data = iris,
#         refresh = 0,
#         iter = 200,
#         chains = 2,
#         seed = 333
#       )
#     )
#   estim <- estimate_smooth(model)
#   expect_equal(c(nrow(estim), ncol(estim)), c(2, 6))
#
#   model <-
#     suppressWarnings(
#       rstanarm::stan_glm(
#         Sepal.Width ~ Species * poly(Petal.Length, 2),
#         data = iris,
#         refresh = 0,
#         iter = 200,
#         chains = 2,
#         seed = 333
#       )
#     )
#   estim <- estimate_smooth(model)
#   expect_equal(ncol(estim), 6)
#   estim <- estimate_smooth(model, levels = "Species")
#   expect_equal(ncol(estim), 7)
# })
#
