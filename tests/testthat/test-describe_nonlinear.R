# if (requiet("rstanarm")) {
#   test_that("estimate_smooth", {
#     skip_on_cran()
#     set.seed(333)
#
#     model <-
#       suppressWarnings(
#         rstanarm::stan_gamm4(
#           Sepal.Width ~ s(Petal.Length),
#           data = iris,
#           refresh = 0,
#           iter = 200,
#           chains = 2,
#           seed = 333
#         )
#       )
#     estim <- estimate_smooth(model)
#     expect_equal(ncol(estim), 6)
#
#     model <-
#       suppressWarnings(
#         rstanarm::stan_glm(
#           Sepal.Width ~ poly(Petal.Length, 2),
#           data = iris,
#           refresh = 0,
#           iter = 200,
#           chains = 2,
#           seed = 333
#         )
#       )
#     estim <- estimate_smooth(model)
#     expect_equal(c(nrow(estim), ncol(estim)), c(2, 6))
#
#     model <-
#       suppressWarnings(
#         rstanarm::stan_glm(
#           Sepal.Width ~ Species * poly(Petal.Length, 2),
#           data = iris,
#           refresh = 0,
#           iter = 200,
#           chains = 2,
#           seed = 333
#         )
#       )
#     estim <- estimate_smooth(model)
#     expect_equal(ncol(estim), 6)
#     estim <- estimate_smooth(model, levels = "Species")
#     expect_equal(ncol(estim), 7)
#   })
# }
