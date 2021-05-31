#' @rdname visualisation_recipe
#'
#' @param show_points If \code{TRUE}, will attempt at adding the points of the original data corresponding to the x and y axes.
#' @param points,line,ribbon,labs Additional aesthetics and parameters for the geoms (see customization example).
#' @param ... Other arguments to be passed to or from other functions.
#'
#'
#' @examples
#' library(modelbased)
#' library(see)
#'
#' # Linear Models
#' # =============================
#' # Default
#' x <- estimate_relation(lm(mpg ~ wt, data = mtcars))
#' layers <- visualisation_recipe(x)
#' layers
#' plot(layers)
#'
#' # Customize aesthetics
#' x <- estimate_relation(lm(mpg ~ wt, data = mtcars))
#' layers <- visualisation_recipe(x,
#'                                points = list(color = "red", alpha = 0.6, size = 3),
#'                                line = list(color = "blue", size = 4),
#'                                ribbon = list(fill = "green", alpha = 0.7),
#'                                labs = list(subtitle = "Oh yeah!"))
#' layers
#' plot(layers)
#'
#'
#' # 2-ways interaction ------------
#'
#' # Numeric * numeric
#' x <- estimate_relation(lm(mpg ~ wt * qsec, data = mtcars))
#' layers <- visualisation_recipe(x)
#' plot(layers)
#'
#' # Factor * numeric
#' x <- estimate_relation(lm(Sepal.Width ~ Species * Sepal.Length, data = iris))
#' layers <- visualisation_recipe(x)
#' plot(layers)
#'
#'
#' # 3-ways interaction ------------
#' data <- mtcars
#' data$vs <- as.factor(data$vs)
#' data$cyl <- as.factor(data$cyl)
#' data$new_factor <- as.factor(rep(c("A", "B"), length.out = nrow(mtcars)))
#'
#' # Numeric * numeric * numeric
#' x <- estimate_relation(lm(mpg ~ wt * qsec * hp, data = data))
#' layers <- visualisation_recipe(x)
#' plot(layers)
#'
#' # Numeric * numeric * factor
#' x <- estimate_relation(lm(mpg ~ wt * am * vs, data = data))
#' layers <- visualisation_recipe(x)
#' plot(layers)
#'
#' # Numeric * factor * factor
#' x <- estimate_relation(lm(mpg ~ wt * cyl * new_factor, data = data))
#' layers <- visualisation_recipe(x)
#' plot(layers)
#'
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
    if(length(targets) == 1) {
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
    } else if(length(targets) > 1) {
      warning("It seems like more than 3 interaction terms are present. Not sure how to plot it, keeping only the 3 first variables (might not be a good visualisation of your model).")
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