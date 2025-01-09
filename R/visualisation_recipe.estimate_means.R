#' @rdname visualisation_recipe.estimate_predicted
#'
#' @examplesIf require("ggplot2") && require("emmeans") && require("see")
#' # Simple Model ---------------
#' x <- estimate_means(lm(Sepal.Width ~ Species, data = iris), by="Species")
#' layers <- visualisation_recipe(x)
#' layers
#' plot(layers)
#'
#' # Customize aesthetics
#' layers <- visualisation_recipe(x,
#'   jitter = list(width = 0.03, color = "red"),
#'   line = list(linetype = "dashed")
#' )
#' plot(layers)
#'
#' # Customize raw data
#' plot(visualisation_recipe(x, show_data = c("violin", "boxplot", "jitter")))
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
visualisation_recipe.estimate_means <- function(x,
                                                show_data = "jitter",
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
  vis_data <- as.data.frame(x)
  y <- info$response
  color <- NULL
  alpha <- NULL

  by_levels <- info$at[info$at %in% names(vis_data[!sapply(vis_data, is.numeric)])]
  modulate <- info$at[info$at %in% names(vis_data[sapply(vis_data, is.numeric)])]
  x1 <- by_levels[1]
  if (length(by_levels) > 1L) {
    color <- by_levels[2]
    if (length(by_levels) > 2L) {
      # TODO: add facetting (needs updating see::geom_from_list to work with facets)
      insight::format_warning("Cannot deal with more than 2 levels variables for now. Other ones will be omitted.")
    }
  }
  if (!is.null(modulate) && length(modulate) > 0L) {
    alpha <- modulate[1]
    if (length(modulate) > 1L) {
      insight::format_warning("Cannot deal with more than 2 modulate variables for now. Other ones will be omitted.")
    }
  }


  # Layers -----------------------
  l <- 1

  # Show data (points, boxplot, violin, etc.)
  if (!is.null(show_data) && all(show_data != "none") && all(show_data != FALSE)) { # nolint
    # Default changes for binomial models
    shape <- 16
    stroke <- 0
    if (insight::model_info(info$model)$is_binomial) {
      shape <- "|"
      stroke <- 1
    }

    rawdata <- .visualisation_recipe_getrawdata(x)
    for (i in show_data) {
      if (i %in% c("point", "points", "jitter")) {
        layers[[paste0("l", l)]] <- .visualisation_predicted_points(
          rawdata,
          x1,
          y,
          color,
          shape = shape,
          stroke = stroke,
          type = i,
          width = 0.1,
          height = 0,
          point = jitter
        )
      } else if (i == "boxplot") {
        layers[[paste0("l", l)]] <-
          .visualisation_means_boxplot(rawdata, x1, y, color, type = "boxplot", boxplot = boxplot)
      } else if (i == "violin") {
        layers[[paste0("l", l)]] <-
          .visualisation_means_boxplot(rawdata, x1, y, color, type = "violin", boxplot = violin)
      } else {
        insight::format_error("`show_data` can only be some of 'points', 'boxplot', 'violin'. Check spelling.")
      }
      l <- l + 1
    }
  }

  # Line
  layers[[paste0("l", l)]] <- .visualisation_means_line(
    vis_data,
    x1,
    y = info$coef_name[1],
    color = color,
    alpha = alpha,
    line = line
  )
  l <- l + 1

  # Pointrange
  layers[[paste0("l", l)]] <- .visualisation_means_pointrange(
    vis_data,
    x1,
    y = info$coef_name[1],
    color = color,
    alpha = alpha,
    pointrange = pointrange
  )
  l <- l + 1

  # Labs
  layers[[paste0("l", l)]] <- .visualisation_means_labs(info, x1, y, labs = labs)

  # Out
  class(layers) <- unique(c("visualisation_recipe", "see_visualisation_recipe", class(layers)))
  attr(layers, "data") <- vis_data
  layers
}


# Layer - Violin / boxplot ------------------------------------------------------------

.visualisation_means_boxplot <- function(raw_data, x1, y, color, type = "boxplot", boxplot = NULL) {
  out <- list(
    data = as.data.frame(raw_data),
    geom = type,
    aes = list(x = x1, y = y, fill = color)
  )

  if (type == "boxplot") {
    out$outlier.shape <- NA
  }
  if (!is.null(boxplot)) out <- utils::modifyList(out, boxplot) # Update with additional args
  out
}

# Layer - Line -------------------------------------------------------------

.visualisation_means_line <- function(data, x1, y, color, alpha, line = NULL) {
  if (is.null(color) && is.null(alpha)) {
    group <- 1
  } else if (!is.null(color) && is.null(alpha)) {
    group <- color
  } else if (is.null(color) && !is.null(alpha)) {
    group <- alpha
  } else {
    group <- paste0("interaction(", alpha, ", ", color, ")")
  }

  out <- list(
    geom = "line",
    data = data,
    aes = list(
      y = y,
      x = x1,
      color = color,
      group = group,
      alpha = alpha
    )
  )

  if (!is.null(color) || !is.null(alpha)) {
    out$position <- "dodge"
    out$width <- 0.1
  }
  if (!is.null(line)) out <- utils::modifyList(out, line) # Update with additional args
  out
}

# Layer - Pointrange -------------------------------------------------------

.visualisation_means_pointrange <- function(data, x1, y, color, alpha, pointrange = NULL) {
  out <- list(
    geom = "pointrange",
    data = data,
    aes = list(
      y = y,
      x = x1,
      ymin = "CI_low",
      ymax = "CI_high",
      color = color,
      alpha = alpha
    )
  )

  if (!is.null(color) || !is.null(alpha)) {
    out$position <- "dodge"
    out$width <- 0.1
  }
  if (!is.null(pointrange)) out <- utils::modifyList(out, pointrange) # Update with additional args
  out
}


# Layer - Labels --------------------------------------------------------------

.visualisation_means_labs <- function(info, x1, y, labs = NULL) {
  if (all(info$coef_name == "Probability")) {
    vis_title <- "Estimated Mean Probabilities"
  } else {
    vis_title <- "Estimated Means"
  }

  out <- list(
    geom = "labs",
    x = x1,
    y = y,
    title = paste0(vis_title, " (", format(insight::find_formula(info$model)), ")")
  )
  if (!is.null(labs)) out <- utils::modifyList(out, labs) # Update with additional args
  out
}
