#' @rdname visualisation_recipe
#' @examples
#' library(modelbased)
#'
#' # Linear Models
#' # ----------------
#' # Simple
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
#' # 2-ways interaction between numeric
#' model <- lm(mpg ~ wt * am, data = mtcars)
#' x <- estimate_relation(model)
#' layers <- visualisation_recipe(x)
#' layers
#'
#' x <- estimate_response(model)
#' # visualisation_recipe(x)
#' @export
visualisation_recipe.estimate_predicted <- function(x,
                                                    show_points = TRUE,
                                                    points = NULL,
                                                    line = NULL,
                                                    ribbon = NULL,
                                                    labs = NULL,
                                                    ...) {
  info <- attributes(x)
  layers <- list()

  # General plot type -----------------
  data <- as.data.frame(x)
  y <- info$response

  # Retrieve predictors
  if("target" %in% names(info)) {
    targets <- info$target
  } else {
    targets <- insight::find_predictors(info$model, effects = "fixed", flatten = TRUE)
  }
  # Find which one is the linear one
  x1 <- targets[sapply(data[targets], is.numeric)][1]
  targets <- targets[targets != x1]
  if(length(x1) == 0) {
    stop("Factors not supported yet. Try using estimate_means().")
  }

  # Deal with more than one target
  alpha <- NULL
  color <- NULL
  linetype <- NULL
  if(length(targets) > 0) {
    # 2-way interaction
    x2 <- targets[1]
    targets <- targets[targets != x2]
    if(is.numeric(data[[x2]])) {
      alpha <- x2
    } else {
      color <- x2
    }

    # 3-way interaction
    if(length(targets) > 0) {
      x3 <- targets[1]
      if(!is.numeric(data[[x2]]) && !is.numeric(data[[x3]])) {
        linetype <- x3
      } else {
        if(is.null(alpha)) {
          alpha <- x3
        } else {
          color <- x3
        }
      }
    }
  }

  # Layers -----------------------
  l <- 1

  # Points
  if(show_points) {
    layers[[paste0("l", l)]] <- .visualisation_predicted_points(info, x1, y, color)
    if(!is.null(points)) {
      layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], points)
    }
    l <- l + 1
  }

  # Ribbon
  if(is.null(alpha) && is.null(linetype)) {
    layers[[paste0("l", l)]] <- .visualisation_predicted_ribbon(data, info, x1, fill = color)
    if(!is.null(ribbon)) {
      layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], ribbon)
    }
    l <- l + 1
  }

  # Line
  layers[[paste0("l", l)]] <- .visualisation_predicted_line(data, info, x1, alpha, color, linetype)
  if(!is.null(line)) {
    layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], line)
  }
  l <- l + 1


  # Labs
  layers[[paste0("l", l)]] <- .visualisation_predicted_labs(info, x1, y)
  if(!is.null(labs)) {
    layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], labs)
  }

  # Out
  class(layers) <- c("visualisation_recipe", class(layers))
  layers
}


# Layer - Points ------------------------------------------------------------

.visualisation_predicted_points <- function(info, x1, y, color) {
  # Add response to data if not there
  if(!y %in% names(info$data)) info$data[y] <- insight::get_response(info$model)

  list(data = as.data.frame(info$data),
       geom = "point",
       aes = list(x = x1, y = y, color = color),
       stroke = 0,
       shape = 16)
}

# Layer - Lines -------------------------------------------------------------


.visualisation_predicted_line <- function(data, info, x1, alpha, color, linetype) {

  group <- alpha
  if(!is.null(alpha) && !is.null(color)) {
    group <- paste0("interaction(", alpha, ", ", color, ")")
  }

  list(data = data,
       geom = "line",
       aes = list(y = "Predicted",
                  x = x1,
                  alpha = alpha,
                  color = color,
                  linetype = linetype,
                  group = group))
}


# Layer - Ribbon -------------------------------------------------------------

.visualisation_predicted_ribbon <- function(data, info, x1, fill) {
  list(geom = "ribbon",
       data = data,
       aes = list(y = "Predicted",
                  x = x1,
                  ymin = "CI_low",
                  ymax = "CI_high",
                  fill = fill),
       alpha = 1/3)
}

# Layer 3 -----------------------------------------------------------------

.visualisation_predicted_labs <- function(info, x1, y) {
  list(geom = "labs",
       x = x1,
       y = y,
       title = paste0("Predicted response (",
                      format(insight::find_formula(info$model)),
                      ")")
  )
}