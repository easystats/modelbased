#' @rdname visualisation_recipe
#'
#' @examples
#' # ==============================================
#' # estimate_expectation, estimate_response, ...
#' # ==============================================
#' if(require("ggplot2")) {
#'
#' # Simple Model ---------------
#' x <- estimate_relation(lm(mpg ~ wt, data = mtcars))
#' layers <- visualisation_recipe(x)
#' layers
#' plot(layers)
#'
#' # Customize aesthetics
#' layers <- visualisation_recipe(x,
#'                                point = list(color = "red", alpha = 0.6, size = 3),
#'                                line = list(color = "blue", size = 4),
#'                                ribbon = list(fill = "green", alpha = 0.7),
#'                                labs = list(subtitle = "Oh yeah!"))
#' layers
#' plot(layers)
#'
#' # Customize raw data
#' plot(visualisation_recipe(x, show_data = "none"))
#' plot(visualisation_recipe(x, show_data = c("density_2d", "points")))
#' plot(visualisation_recipe(x, show_data = "density_2d_filled"))
#' plot(visualisation_recipe(x, show_data = "density_2d_polygon"))
#' plot(visualisation_recipe(x, show_data = "density_2d_raster")) +
#'   scale_x_continuous(expand = c(0, 0)) +
#'   scale_y_continuous(expand = c(0, 0))
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
#' }
#' @export
visualisation_recipe.estimate_predicted <- function(x,
                                                    show_data = "points",
                                                    point = NULL,
                                                    density_2d = NULL,
                                                    line = NULL,
                                                    ribbon = NULL,
                                                    labs = NULL,
                                                    ...) {
  info <- attributes(x)
  layers <- list()

  # Main aesthetics -----------------
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
  if(!is.null(show_data) && show_data != "none") {
    rawdata <- .visualisation_recipe_getrawdata(x)
    for(i in show_data) {
      if(i %in% c("point", "points")) {
        layers[[paste0("l", l)]] <- .visualisation_predicted_points(rawdata, x1, y, color, type = "point", point = point)
      } else if(i %in% c("density_2d", "density_2d_filled", "density_2d_polygon", "density_2d_raster")) {
        layers[[paste0("l", l)]] <- .visualisation_predicted_density2d(rawdata, x1, y, type = i, density_2d = density_2d)
      } else {
        stop("'show_data' can only be some of 'points', 'density_2d', 'density_2d_filled', density_2d_polygon', 'density_2d_raster'. Check spelling.")
      }
      l <- l + 1
    }
  }

  # Ribbon
  if(is.null(alpha) && is.null(linetype)) {
    layers[[paste0("l", l)]] <- .visualisation_predicted_ribbon(data, info, x1, fill = color, ribbon = ribbon)
    l <- l + 1
  }

  # Line
  layers[[paste0("l", l)]] <- .visualisation_predicted_line(data, info, x1, alpha, color, linetype, line = line)
  l <- l + 1

  # Labs
  layers[[paste0("l", l)]] <- .visualisation_predicted_labs(info, x1, y, labs = labs)

  # Out
  class(layers) <- c("visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  layers
}


# Layer - Points ------------------------------------------------------------

.visualisation_predicted_points <- function(rawdata, x1, y, color, type = "point", point = NULL) {
  out <- list(
    data = as.data.frame(rawdata),
    geom = type,
    aes = list(x = x1, y = y, color = color),
    stroke = 0,
    shape = 16)
  if(type == "jitter") out$width <- 0.1
  if(!is.null(point)) out <- utils::modifyList(out, point) # Update with additional args
  out
}

# Layer - Density 2D ------------------------------------------------------------

.visualisation_predicted_density2d <- function(rawdata, x1, y, type = "density_2d", density_2d = NULL) {
  out <- list(
    data = as.data.frame(rawdata),
    geom = type,
    aes = list(x = x1, y = y))
  if(!is.null(density_2d)) out <- utils::modifyList(out, density_2d) # Update with additional args
  out
}

# Layer - Lines -------------------------------------------------------------


.visualisation_predicted_line <- function(data, info, x1, alpha, color, linetype, line = NULL) {

  group <- alpha
  if(!is.null(alpha) && !is.null(color)) {
    group <- paste0("interaction(", alpha, ", ", color, ")")
  }

  out <- list(data = data,
               geom = "line",
               aes = list(y = "Predicted",
                          x = x1,
                          alpha = alpha,
                          color = color,
                          linetype = linetype,
                          group = group))
  if(!is.null(line)) out <- utils::modifyList(out, line) # Update with additional args
  out
}


# Layer - Ribbon -------------------------------------------------------------

.visualisation_predicted_ribbon <- function(data, info, x1, fill, ribbon = NULL) {
  out <- list(geom = "ribbon",
             data = data,
             aes = list(y = "Predicted",
                        x = x1,
                        ymin = "CI_low",
                        ymax = "CI_high",
                        fill = fill),
             alpha = 1/3)
  if(!is.null(ribbon)) out <- utils::modifyList(out, ribbon) # Update with additional args
  out
}

# Layer - Labels --------------------------------------------------------------

.visualisation_predicted_labs <- function(info, x1, y, labs = NULL) {
  out <- list(geom = "labs",
             x = x1,
             y = y,
             title = paste0("Predicted response (",
                            format(insight::find_formula(info$model)),
                            ")")
        )
  if(!is.null(labs)) out <- utils::modifyList(out, labs) # Update with additional args
  out
}
