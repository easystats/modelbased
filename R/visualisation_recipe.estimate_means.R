#' @rdname visualisation_recipe
#'
#' @examples
#' if(require("see")) {
#' library(modelbased)
#'
#' # Default
#' x <- estimate_means(lm(Sepal.Width ~ Species, data = iris))
#' layers <- visualisation_recipe(x)
#' layers
#' plot(layers)
#' }
#' @export
visualisation_recipe.estimate_means <- function(x,
                                                show_points = TRUE,
                                                point = NULL,
                                                line = NULL,
                                                pointrange = NULL,
                                                labs = NULL,
                                                ...) {
  info <- attributes(x)
  layers <- list()


  # General plot type -----------------
  data <- as.data.frame(x)
  y <- info$response

  levels <- info$levels
  if(length(levels) > 1) {
    stop("Can't deal with more than one variable set as `levels` yet.")
  }
  x1 <- levels[1]
  color <- NULL

  # Layers -----------------------
  l <- 1

  # Points
  if(show_points) {
    layers[[paste0("l", l)]] <- .visualisation_means_jitter(info, x1, y, color)
    if(!is.null(point)) {
      layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], point)
    }
    l <- l + 1
  }

  # Line
  layers[[paste0("l", l)]] <- .visualisation_means_line(data, x1)
  if(!is.null(line)) {
    layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], line)
  }
  l <- l + 1

  # Pointrange
  layers[[paste0("l", l)]] <- .visualisation_means_pointrange(data, x1)
  if(!is.null(pointrange)) {
    layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], pointrange)
  }
  l <- l + 1

  # Labs
  layers[[paste0("l", l)]] <- .visualisation_means_labs(info, x1, y)
  if(!is.null(labs)) {
    layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], labs)
  }


  # Out
  class(layers) <- c("visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  layers
}

# Layer - Jitter ------------------------------------------------------------

.visualisation_means_jitter <- function(info, x1, y, color) {
  data <- insight::get_data(info$model)
  # Add response to data if not there
  if(!y %in% names(data)) data[y] <- insight::get_response(info$model)

  list(data = as.data.frame(data),
       geom = "jitter",
       aes = list(x = x1, y = y, color = color),
       stroke = 0,
       shape = 16,
       width = 0.1)
}


# Layer - Line -------------------------------------------------------------

.visualisation_means_line <- function(data, x1) {
  list(geom = "line",
       data = data,
       aes = list(y = "Mean",
                  x = x1,
                  group = 1)
  )
}

# Layer - Pointrange -------------------------------------------------------

.visualisation_means_pointrange <- function(data, x1) {
  list(geom = "pointrange",
       data = data,
       aes = list(y = "Mean",
                  x = x1,
                  ymin = "CI_low",
                  ymax = "CI_high")
  )
}



# Layer - Labels --------------------------------------------------------------

.visualisation_means_labs <- function(info, x1, y) {
  list(geom = "labs",
       x = x1,
       y = y,
       title = paste0("Estimated Means (",
                      format(insight::find_formula(info$model)),
                      ")")
  )
}