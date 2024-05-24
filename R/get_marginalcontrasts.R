# Wrapper around marginaleffects to get marginal means
#
# library(modelbased)
# if (require("marginaleffects")) {
#
# # Frequentist models
# # -------------------
# model <- lm(Petal.Length ~ Sepal.Width * Species, data = iris)
#
# estimate_means(model)
# estimate_means(model, fixed = "Sepal.Width")
# estimate_means(model, by = c("Species", "Sepal.Width"), length = 2)
# estimate_means(model, by = "Species=c('versicolor', 'setosa')")
# estimate_means(model, by = "Sepal.Width=c(2, 4)")
# estimate_means(model, by = c("Species", "Sepal.Width=0"))
# estimate_means(model, by = "Sepal.Width", length = 5)
# estimate_means(model, by = "Sepal.Width=c(2, 4)")
#
# # Methods that can be applied to it:
# means <- estimate_means(model, fixed = "Sepal.Width")
# if (require("see")) {
#   plot(means) # which runs visualisation_recipe()
# }
# standardize(means)
#
# if (require("lme4")) {
#   data <- iris
#   data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#
#   model <- lmer(Petal.Length ~ Sepal.Width + Species + (1 | Petal.Length_factor), data = data)
#  estimate_means(model)
#   estimate_means(model, by = "Sepal.Width", length = 3)
# }
# }
# }
# #' @keywords internal
# .get_marginalmeans <- function(model,
#                                by = "auto",
#                                fixed = NULL,
#                                transform = "response",
#                                ci = 0.95,
#                                ...) {
#
#   # check if available
#   insight::check_if_installed("marginaleffects")
#
#   # Guess arguments
#   args <- modelbased:::.guess_emmeans_arguments(model, at, fixed, ...)
#
#   # Run emmeans
#   means <- marginaleffects::marginalmeans(model, args$at, conf_level = ci)
#
#   # Format names
#   names(means)[names(means) %in% c("conf.low")] <- "CI_low"
#   names(means)[names(means) %in% c("conf.high")] <- "CI_high"
#   names(means)[names(means) %in% c("std.error")] <- "SE"
#   names(means)[names(means) %in% c("marginalmean")] <- "Mean"
#   names(means)[names(means) %in% c("p.value")] <- "p"
#   names(means)[names(means) %in% c("statistic")] <- "p"
#
#   ifelse(insight::find_statistic(model) == "t-statistic", "t", "statistic")
#
#   out
#
# }
