#' @rdname visualisation_recipe.estimate_predicted
#'
#' @examplesIf require("ggplot2") && require("emmeans") && require("see")
#' # ==============================================
#' # estimate_means
#' # ==============================================
#' # Simple Model ---------------
#' x <- estimate_means(lm(Sepal.Width ~ Species, data = iris), by = "Species")
#' layers <- visualisation_recipe(x)
#' layers
#' plot(layers)
#'
#' # Customize aesthetics
#' layers <- visualisation_recipe(x,
#'   points = list(width = 0.03, color = "red"),
#'   pointrange = list(size=2, linewidth=2),
#'   line = list(linetype = "dashed", color="blue")
#' )
#' plot(layers)
#'
#' # Two levels ---------------
#' data <- mtcars
#' data$cyl <- as.factor(data$cyl)
#'
#' model <- lm(mpg ~ cyl * wt, data = data)
#'
#' x <- estimate_means(model, by = c("cyl", "wt"))
#' plot(visualisation_recipe(x))
#'
#'
#' # GLMs ---------------------
#' data <- data.frame(vs = mtcars$vs, cyl = as.factor(mtcars$cyl))
#' x <- estimate_means(glm(vs ~ cyl, data = data, family = "binomial"), by = c("cyl"))
#' plot(visualisation_recipe(x))
#' @export
visualisation_recipe.estimate_means <- function(x, show_data = TRUE, ...) {
  .visualization_recipe(x, show_data=show_data, ...)
}

#' @rdname visualisation_recipe.estimate_predicted
#'
#' @examplesIf require("ggplot2") && require("emmeans") && require("see")
#' # ==============================================
#' # estimate_slopes
#' # ==============================================
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' x <- estimate_slopes(model, trend = "Petal.Length", by = "Species")
#'
#' layers <- visualisation_recipe(x)
#' layers
#' plot(layers)
#'
#' # Customize aesthetics and add horizontal line and theme
#' layers <- visualisation_recipe(x, pointrange = list(size=2, linewidth=2))
#' plot(layers) +
#'   geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
#'   theme_minimal() +
#'   labs(y="Effect of Petal.Length", title="Marginal Effects")
#'
#' model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)
#' x <- estimate_slopes(model, by = "Sepal.Width", length = 20)
#' plot(visualisation_recipe(x))
#'
#' model <- lm(Petal.Length ~ Species * poly(Sepal.Width, 3), data = iris)
#' x <- estimate_slopes(model, by = c("Sepal.Width", "Species"))
#' plot(visualisation_recipe(x))
#' }
#' @export
visualisation_recipe.estimate_slopes <- function(x, ...) {
  .visualization_recipe(x, show_data=FALSE, ...)
}


