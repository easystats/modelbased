#' @rdname visualisation_recipe
#' @examples
#' library(modelbased)
#'
#' # Linear Models
#' model <- lm(mpg ~ wt, data = mtcars)
#' x <- estimate_relation(model)
#' layers <- visualisation_recipe(x)
#' layers
#'
#' if (require("ggplot2")) {
#'   ggplot() +
#'     geom_line(data = layers$l1$data,
#'               aes_string(x = layers$l1$x, y = layers$l1$y)) +
#'     geom_ribbon(data = layers$l2$data, alpha = layers$l2$alpha,
#'                 aes_string(x = layers$l2$x, y = layers$l2$y,
#'                            ymin = layers$l2$ymin, ymax = layers$l2$ymax)) +
#'     labs(x = layers$l3$x, y = layers$l3$y, title = layers$l3$title)
#' }
#'
#' x <- estimate_response(model)
#' # visualisation_recipe(x)
#' @export
visualisation_recipe.estimate_predicted <- function(x, ...) {
  info <- attributes(x)
  layers <- list()

  # Layers
  layers$l1 <- .visualisation_predicted_l1(x, info)
  layers$l2 <- .visualisation_predicted_l2(x, info, layers)
  layers$l3 <- .visualisation_predicted_l3(x, info, layers)

  # Out
  class(layers) <- c("visualisation_recipe", class(layers))
  layers
}



# Layer 1 -----------------------------------------------------------------


.visualisation_predicted_l1 <- function(x, info) {

  out <- list(data = as.data.frame(x),
              geom = "geom_line",
              y = "Predicted")

  # Retrieve predictors
  if("target" %in% names(info)) {
    targets <- info$target
  } else {
    targets <- insight::find_predictors(info$model, effects = "fixed", flatten = TRUE)
  }

  # Sanity check for now
  if(length(targets) > 1) {
    stop("Multiple targets not supported yet.")
  }

  # Find which one is the linear one
  var_x <- targets[sapply(out$data[targets], is.numeric)][1]
  if(length(var_x) == 0) {
    stop("Factors not supported yet.")
  }

  out$x <- var_x
  out
}


# Layer 2 -----------------------------------------------------------------

.visualisation_predicted_l2 <- function(x, info, layers = NULL) {
  list(geom = "geom_ribbon",
       data = layers$l1$data,
       y = layers$l1$y,
       x = layers$l1$x,
       ymin = "CI_low",
       ymax = "CI_high",
       alpha = 1/3)
}

# Layer 3 -----------------------------------------------------------------

.visualisation_predicted_l3 <- function(x, info, layers = NULL) {
  list(geom = "labs",
       x = layers$l1$x,
       y = info$response,
       title = "Predicted Response")
}