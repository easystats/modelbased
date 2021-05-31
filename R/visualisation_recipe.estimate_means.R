#' @rdname visualisation_recipe
#'
#' @examples
#' # ==============================================
#' # estimate_means
#' # ==============================================
#' if(require("see")) {
#'
#' # Simple Model ---------------
#' x <- estimate_means(lm(Sepal.Width ~ Species, data = iris))
#' layers <- visualisation_recipe(x)
#' layers
#' plot(layers)
#'
#' # Customize aesthetics
#' layers <- visualisation_recipe(x,
#'                                jitter = list(width = 0.03, color = "red"),
#'                                line = list(linetype = "dashed"))
#' plot(layers)
#'
#' # Two levels ---------------
#' data <- mtcars
#' data$cyl <- as.factor(data$cyl)
#' data$new_factor <- as.factor(rep(c("A", "B"), length.out = nrow(mtcars)))
#'
#' model <- lm(mpg ~ new_factor * cyl * wt, data = data)
#' x <- estimate_means(model, levels = c("new_factor", "cyl"))
#' plot(visualisation_recipe(x))
#'
#' # Modulations --------------
#' x <- estimate_means(model, levels = c("new_factor"), modulate = "wt")
#' plot(visualisation_recipe(x))
#'
#' x <- estimate_means(model, levels = c("new_factor", "cyl"), modulate = "wt")
#' plot(visualisation_recipe(x))
#'
#' }
#' @export
visualisation_recipe.estimate_means <- function(x,
                                                show_data = "points",
                                                point = NULL,
                                                jitter = point,
                                                line = NULL,
                                                pointrange = NULL,
                                                labs = NULL,
                                                ...) {
  info <- attributes(x)
  layers <- list()


  # Main aesthetics -----------------
  data <- as.data.frame(x)
  y <- info$response
  color <- NULL
  alpha <- NULL

  levels <- info$levels
  x1 <- levels[1]
  if(length(levels) > 1) {
    color <- levels[2]
    if(length(levels) > 2) {
      # TODO: add facetting (needs updating see::geom_from_list to work with facets)
      warning("Cannot deal with more than 2 levels variables for now. Other ones will be omitted.")
    }
  }
  if(!is.null(info$modulate)) {
    alpha <- info$modulate[1]
    if(length(info$modulate) > 1) {
      warning("Cannot deal with more than 2 modulate variables for now. Other ones will be omitted.")
    }
  }


  # Layers -----------------------
  l <- 1

  # Points
  if(!is.null(show_data) && any(show_data != "none")) {
    if(any(show_data %in% c("point", "points"))) {
      layers[[paste0("l", l)]] <- .visualisation_means_jitter(info, x1, y, color)
    } else {
      # TODO: Violin and boxplot
      stop("Only `show_data = 'points'` are supported for now.")
    }
    if(!is.null(jitter)) {
      layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], jitter)
    }
    l <- l + 1
  }

  # Line
  layers[[paste0("l", l)]] <- .visualisation_means_line(data, x1, color, alpha)
  if(!is.null(line)) {
    layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], line)
  }
  l <- l + 1

  # Pointrange
  layers[[paste0("l", l)]] <- .visualisation_means_pointrange(data, x1, color, alpha)
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

.visualisation_means_line <- function(data, x1, color, alpha) {
  if(is.null(color) && is.null(alpha)) {
    group <- 1
  } else if(!is.null(color) && is.null(alpha)) {
    group <- color
  } else if(is.null(color) && !is.null(alpha)) {
    group <- alpha
  } else{
    group <- paste0("interaction(", alpha, ", ", color, ")")
  }

  out <- list(geom = "line",
              data = data,
              aes = list(y = "Mean",
                         x = x1,
                         color = color,
                         group = group,
                         alpha = alpha)
         )

  if(!is.null(color) || !is.null(alpha)) {
    out$position <- "dodge"
    out$width <- 0.1
  }

  out
}

# Layer - Pointrange -------------------------------------------------------

.visualisation_means_pointrange <- function(data, x1, color, alpha) {

  out <- list(geom = "pointrange",
              data = data,
              aes = list(y = "Mean",
                         x = x1,
                         ymin = "CI_low",
                         ymax = "CI_high",
                         color = color,
                         alpha = alpha)
              )

  if(!is.null(color) || !is.null(alpha)) {
    out$position <- "dodge"
    out$width <- 0.1
  }

  out
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