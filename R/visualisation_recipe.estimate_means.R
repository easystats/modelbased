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
#' # Customize raw data
#' plot(visualisation_recipe(x, show_data = c("violin", "boxplot", "points")))
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
                                                boxplot = NULL,
                                                violin = NULL,
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

  # Show data (points, boxplot, violin, etc.)
  if(!is.null(show_data) && any(show_data != "none")) {
    rawdata <- .visualisation_recipe_getrawdata(x)
    for(i in show_data) {
      if(i %in% c("point", "points", "jitter")) {
        layers[[paste0("l", l)]] <- .visualisation_predicted_points(rawdata, x1, y, color, type = "jitter", point = jitter)
      } else if (i == "boxplot") {
        layers[[paste0("l", l)]] <- .visualisation_means_boxplot(rawdata, x1, y, color, type = "boxplot", boxplot = boxplot)
      } else if (i == "violin") {
        layers[[paste0("l", l)]] <- .visualisation_means_boxplot(rawdata, x1, y, color, type = "violin", boxplot = violin)
      } else {
        stop("'show_data' can only be some of 'points', 'boxplot', 'violin'. Check spelling.")
      }
      l <- l + 1
    }
  }

  # Line
  layers[[paste0("l", l)]] <- .visualisation_means_line(data, x1, color, alpha, line = line)
  l <- l + 1

  # Pointrange
  layers[[paste0("l", l)]] <- .visualisation_means_pointrange(data, x1, color, alpha, pointrange = pointrange)
  l <- l + 1

  # Labs
  layers[[paste0("l", l)]] <- .visualisation_means_labs(info, x1, y, labs = labs)

  # Out
  class(layers) <- c("visualisation_recipe", class(layers))
  attr(layers, "data") <- data
  layers
}


# Layer - Violin / boxplot ------------------------------------------------------------

.visualisation_means_boxplot <- function(raw_data, x1, y, color, type = "boxplot", boxplot = NULL) {
  out <- list(data = as.data.frame(raw_data),
              geom = type,
              aes = list(x = x1, y = y, fill = color))

  if(type == "boxplot") {
    out$outlier.shape <- NA
  }
  if(!is.null(boxplot)) out <- utils::modifyList(out, boxplot) # Update with additional args
  out
}

# Layer - Line -------------------------------------------------------------

.visualisation_means_line <- function(data, x1, color, alpha, line = NULL) {
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
  if(!is.null(line)) out <- utils::modifyList(out, line) # Update with additional args
  out
}

# Layer - Pointrange -------------------------------------------------------

.visualisation_means_pointrange <- function(data, x1, color, alpha, pointrange = NULL) {

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
  if(!is.null(pointrange)) out <- utils::modifyList(out, pointrange) # Update with additional args
  out
}



# Layer - Labels --------------------------------------------------------------

.visualisation_means_labs <- function(info, x1, y, labs = NULL) {
  out <- list(geom = "labs",
       x = x1,
       y = y,
       title = paste0("Estimated Means (",
                      format(insight::find_formula(info$model)),
                      ")")
  )
  if(!is.null(labs)) out <- utils::modifyList(out, labs) # Update with additional args
  out
}