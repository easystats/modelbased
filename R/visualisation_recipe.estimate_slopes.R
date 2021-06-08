#' @rdname visualisation_recipe.estimate_predicted
#'
#' @examples
#' # ==============================================
#' # estimate_slopes
#' # ==============================================
#' if (require("ggplot2")) {
#'   model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'   x <- estimate_slopes(model, levels = "Species", trend = "Petal.Length")
#'
#'   layers <- visualisation_recipe(x)
#'   layers
#'   plot(layers)
#'
#' }
#' @export
visualisation_recipe.estimate_slopes <- function(x,
                                                 hline = NULL,
                                                 line = NULL,
                                                 pointrange = NULL,
                                                 labs = NULL, ...) {
  info <- attributes(x)
  layers <- list()

  # Main aesthetics -----------------
  data <- as.data.frame(x)
  y <- info$trend

  levels <- info$levels
  x1 <- levels[1]

  # Layers -----------------------
  l <- 1

  # Horizontal Line
  layers[[paste0("l", l)]] <- .visualisation_random_hline(data, x1, hline = hline)
  l <- l + 1

  # Line
  layers[[paste0("l", l)]] <- .visualisation_means_line(data, x1, y = "Coefficient", color = NULL, alpha = NULL, line = line)
  l <- l + 1

  # Pointrange
  layers[[paste0("l", l)]] <- .visualisation_means_pointrange(data, x1, y = "Coefficient", color = NULL, alpha = NULL, pointrange = pointrange)
  l <- l + 1

  # Labs
  layers[[paste0("l", l)]] <- .visualisation_slopes_labs(info, labs = labs)

  # Out
  class(layers) <- c("visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  layers

}



# Layer - Labels --------------------------------------------------------------

.visualisation_slopes_labs <- function(info, labs = NULL) {
  out <- list(geom = "labs",
              y = paste0("Effect of ", info$trend),
              title = paste0(
                "Estimated Coefficients (",
                format(insight::find_formula(info$model)),
                ")"))

  if (!is.null(labs)) out <- utils::modifyList(out, labs) # Update with additional args
  out
}
