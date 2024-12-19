#' Visualisation Recipe for 'modelbased' Objects
#'
#'
#' @param x A modelbased object.
#' @param show_data Display the "raw" data as a background to the model-based
#'   estimation. Can be set to `"none"` to remove it. When input is the result
#'   of `estimate_means`, `show_data` can be "points" (the jittered observation
#'   points), "boxplot", "violin" a combination of them (see examples). When
#'   input is the result of `estimate_expectation` or `estimate_relation`,
#'   `show_data` can be "points" (the points of the original data corresponding
#'   to the x and y axes), "density_2d", "density_2d_filled",
#'   "density_2d_polygon" or "density_2d_raster".
#' @param point,jitter,boxplot,violin,pointrange,density_2d,line,hline,ribbon,labs,facet_wrap Additional
#' aesthetics and parameters for the geoms (see customization example).
#' @param ... Other arguments passed to other functions.
#'
#' @examplesIf require("ggplot2") && require("emmeans") && require("see")
#' # ==============================================
#' # estimate_relation, estimate_expectation, ...
#' # ==============================================
#' # Simple Model ---------------
#' x <- estimate_relation(lm(mpg ~ wt, data = mtcars))
#' layers <- visualisation_recipe(x)
#' layers
#' plot(layers)
#'
#' \dontrun{
#' # Customize aesthetics ----------
#'
#' layers <- visualisation_recipe(x,
#'   point = list(color = "red", alpha = 0.6, size = 3),
#'   line = list(color = "blue", size = 3),
#'   ribbon = list(fill = "green", alpha = 0.7),
#'   labs = list(subtitle = "Oh yeah!")
#' )
#' layers
#' plot(layers)
#'
#' # Customize raw data -------------
#'
#' plot(visualisation_recipe(x, show_data = "none"))
#' plot(visualisation_recipe(x, show_data = c("density_2d", "points")))
#' plot(visualisation_recipe(x, show_data = "density_2d_filled"))
#' plot(visualisation_recipe(x, show_data = "density_2d_polygon"))
#' plot(visualisation_recipe(x, show_data = "density_2d_raster")) +
#'   scale_x_continuous(expand = c(0, 0)) +
#'   scale_y_continuous(expand = c(0, 0))
#'
#' # Single predictors examples -----------
#'
#' plot(estimate_relation(lm(Sepal.Length ~ Sepal.Width, data = iris)))
#' plot(estimate_relation(lm(Sepal.Length ~ Species, data = iris)))
#'
#' # 2-ways interaction ------------
#'
#' # Numeric * numeric
#' x <- estimate_relation(lm(mpg ~ wt * qsec, data = mtcars))
#' layers <- visualisation_recipe(x)
#' plot(layers)
#'
#' # Numeric * factor
#' x <- estimate_relation(lm(Sepal.Width ~ Sepal.Length * Species, data = iris))
#' layers <- visualisation_recipe(x)
#' plot(layers)
#'
#' # Factor * numeric
#' x <- estimate_relation(lm(Sepal.Width ~ Species * Sepal.Length, data = iris))
#' layers <- visualisation_recipe(x)
#' plot(layers)
#'
#' # 3-ways interaction ------------
#'
#' data <- mtcars
#' data$vs <- as.factor(data$vs)
#' data$cyl <- as.factor(data$cyl)
#' data$new_factor <- as.factor(rep(c("A", "B"), length.out = nrow(mtcars)))
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
#' # Factor * numeric * numeric
#' x <- estimate_relation(lm(mpg ~ cyl * qsec * hp, data = data))
#' layers <- visualisation_recipe(x)
#' plot(layers) +
#'   scale_size_continuous(range = c(0.2, 1))
#'
#' # GLMs ---------------------
#' x <- estimate_relation(glm(vs ~ mpg, data = mtcars, family = "binomial"))
#' plot(visualisation_recipe(x))
#' plot(visualisation_recipe(x, show_data = "jitter", point = list(height = 0.03)))
#'
#' # Multiple CIs ---------------------
#' plot(estimate_relation(lm(mpg ~ disp, data = mtcars),
#'   ci = c(.50, .80, .95)
#' ))
#' plot(estimate_relation(lm(Sepal.Length ~ Species, data = iris),
#'   ci = c(0.5, 0.7, 0.95)
#' ))
#'
#' # Bayesian models ---------------------
#' if (require("ggplot2") && require("rstanarm")) {
#'   model <- rstanarm::stan_glm(mpg ~ wt, data = mtcars, refresh = 0)
#'
#'   # Plot individual draws instead of regular ribbon
#'   x <- estimate_relation(model, keep_iterations = 100)
#'   layers <- visualisation_recipe(x, ribbon = list(color = "red"))
#'   plot(layers)
#'
#'   model <- rstanarm::stan_glm(Sepal.Width ~ Species * Sepal.Length, data = iris, refresh = 0)
#'   plot(estimate_relation(model, keep_iterations = 100))
#' }
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
  data <- as.data.frame(x)
  y <- info$response
  alpha <- NULL
  color <- NULL
  linetype <- NULL
  size <- NULL
  x2 <- NULL
  x3 <- NULL
  jitter <- FALSE
  skip_line <- FALSE
  group <- 1

  # x-axis -----------------

  # Retrieve predictors
  if ("at_specs" %in% names(info)) {
    targets <- info$at_specs$varname
  } else {
    targets <- insight::find_predictors(info$model, effects = "fixed", flatten = TRUE)
  }

  # first at-value along x-axis
  x1 <- targets[1]
  targets <- targets[targets != x1] # Remove from target list

  # if x-axis a factor, we jitter the points
  if (!is.numeric(data[[x1]]) && show_data %in% c("point", "points")) {
    show_data <- "jitter"
  }


  # Decide what variables has what aesthetics -------------------
  # 2-way interaction
  if (length(targets) > 0) {
    x2 <- targets[1]
    targets <- targets[targets != x2]
    color <- x2
    data[[".group"]] <- paste0(data[[x2]])
    group <- ".group"
    # If x-axis is a factor, then we cannot add the line + we need to jitter the pointrnage
    if (!is.numeric(data[[x1]])) {
      jitter <- TRUE
      skip_line <- TRUE
    }
  }

  # 3-way interaction
  if (length(targets) > 0) {
    x3 <- targets[1]
    targets <- targets[targets != x3]
    if (is.numeric(data[[x1]])) {
      if (is.numeric(data[[x3]])) {
        alpha <- x3
      } else {
        linetype <- x3
      }
    } else {
      size <- x3
    }
    data[[".group"]] <- paste0(data[[x2]], "_", data[[x3]])
    group <- ".group"
  }

  # 4+ interaction
  if (length(targets) > 0L) {
    # TODO: We could add the fourth term as facets
    insight::format_warning("It seems like more than 4 focal terms are present. Not sure how to plot it, so keeping only the 3 first variables (however, this might not be a good visualisation of your model).")
  }


  # Layers ========================
  l <- 1

  # Raw data ---------------------
  if (!is.null(show_data) && all(show_data != "none") && !all(isFALSE(show_data))) {
    rawdata <- .visualisation_recipe_getrawdata(x)

    # Nicer points

    if (is.numeric(data[[x1]])) {
      stroke <- 0
      shape <- 16
    } else {
      stroke <- 1
      shape <- "+"
    }


    # Default changes for binomial models
    if (insight::model_info(info$model)$is_binomial && show_data %in% c("point", "points")) {
      shape <- "|"
      stroke <- 1

      # Change scale to 1-2 in case outcome is factor (see #120)
      if (!all(unique(rawdata[[y]]) %in% c(0, 1))) {
        data[c("Predicted", "CI_low", "CI_high")] <- data[c("Predicted", "CI_low", "CI_high")] + 1
      } else {
        # Else force to numeric and not factor
        rawdata[[y]] <- as.numeric(as.character(rawdata[[y]]))
      }
    }

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
          point = point
        )
      } else if (i %in% c("density_2d", "density_2d_filled", "density_2d_polygon", "density_2d_raster")) {
        layers[[paste0("l", l)]] <- .visualisation_predicted_density2d(
          rawdata,
          x1,
          y,
          type = i,
          density_2d = density_2d
        )
      } else {
        stop("'show_data' can only be some of 'points', 'density_2d', 'density_2d_filled', density_2d_polygon', 'density_2d_raster'. Check spelling.", call. = FALSE)
      }
      l <- l + 1
    }
  }

  # Uncertainty -----------------------------------

  # Get CIs (reverse so that larger CIs are first)
  ci_lows <- rev(grep("CI_low", names(data), fixed = TRUE, value = TRUE))
  ci_highs <- rev(grep("CI_high", names(data), fixed = TRUE, value = TRUE))

  # Ribbon
  if (is.numeric(data[[x1]]) && (is.null(x3) || !is.numeric(data[[x2]])) && is.null(x3)) {
    if ("iter_1" %in% names(data)) {
      layers[[paste0("l", l)]] <- .visualisation_predicted_iterations(
        data,
        x1,
        fill = color,
        ribbon = ribbon
      )
      l <- l + 1
    } else {
      for (i in seq_len(length(ci_lows))) {
        layers[[paste0("l", l)]] <- .visualisation_predicted_ribbon(
          data,
          x1,
          y = "Predicted",
          fill = color,
          ci_low = ci_lows[i],
          ci_high = ci_highs[i],
          ribbon = ribbon,
          group = group
        )
        l <- l + 1
      }
    }
  }

  # Point range
  if (!is.numeric(data[[x1]])) {
    for (i in seq_len(length(ci_lows))) {
      layers[[paste0("l", l)]] <- .visualisation_predicted_pointrange(
        data,
        x1,
        y = "Predicted",
        color = color,
        size = size,
        ci_low = ci_lows[i],
        ci_high = ci_highs[i],
        alpha = i / length(ci_lows),
        jitter = jitter
      )
      l <- l + 1
    }
  }

  # Predicted -----------------------------------

  # Line
  if (isFALSE(skip_line)) {
    layers[[paste0("l", l)]] <- .visualisation_predicted_line(
      data,
      x1,
      alpha,
      color,
      linetype,
      group = group,
      line = line
    )
    l <- l + 1
  }

  # Ornaments -----------------------------------

  # Labs
  layers[[paste0("l", l)]] <- .visualisation_predicted_labs(info, x1, y, labs = labs)

  # Out
  class(layers) <- unique(c("visualisation_recipe", "see_visualisation_recipe", class(layers)))
  attr(layers, "data") <- data
  layers
}


# Layer - Points ------------------------------------------------------------

.visualisation_predicted_points <- function(rawdata,
                                            x1,
                                            y,
                                            color,
                                            type = "point",
                                            shape = 16,
                                            stroke = 0,
                                            width = NULL,
                                            height = NULL,
                                            point = NULL,
                                            jitter = NULL) {
  if (type == "points") type <- "point" # Sanity fix

  out <- list(
    data = as.data.frame(rawdata),
    geom = type,
    aes = list(x = x1, y = y, color = color),
    stroke = stroke,
    shape = shape
  )
  if (!is.null(width)) out$width <- width
  if (!is.null(height)) out$height <- height
  if (!is.null(jitter)) out$position <- ggplot2::position_jitter(width = jitter)
  if (!is.null(point)) out <- utils::modifyList(out, point) # Update with additional args
  out
}

# Layer - Density 2D ------------------------------------------------------------

.visualisation_predicted_density2d <- function(rawdata,
                                               x1,
                                               y,
                                               type = "density_2d",
                                               density_2d = NULL) {
  out <- list(
    data = as.data.frame(rawdata),
    geom = type,
    aes = list(x = x1, y = y)
  )

  if (!is.null(density_2d)) out <- utils::modifyList(out, density_2d) # Update with additional args
  out
}

# Layer - Lines -------------------------------------------------------------

.visualisation_predicted_line <- function(data,
                                          x1,
                                          alpha,
                                          color,
                                          linetype,
                                          group = NULL,
                                          line = NULL) {
  if (is.null(group)) group <- alpha
  if (!is.null(alpha) && !is.null(color)) {
    group <- paste0("interaction(", alpha, ", ", color, ")")
  }

  out <- list(
    data = data,
    geom = "line",
    aes = list(
      y = "Predicted",
      x = x1,
      alpha = alpha,
      color = color,
      linetype = linetype,
      group = group
    )
  )
  if (!is.null(line)) out <- utils::modifyList(out, line) # Update with additional args
  out
}


# Layer - Ribbon -------------------------------------------------------------

.visualisation_predicted_ribbon <- function(data,
                                            x1,
                                            y,
                                            fill,
                                            group = NULL,
                                            alpha = 1 / 3,
                                            ci_low = "CI_low",
                                            ci_high = "CI_high",
                                            ribbon = NULL) {
  out <- list(
    geom = "ribbon",
    data = data,
    aes = list(
      y = y,
      x = x1,
      ymin = ci_low,
      ymax = ci_high,
      fill = fill,
      group = group
    ),
    alpha = alpha
  )
  if (!is.null(ribbon)) out <- utils::modifyList(out, ribbon) # Update with additional args
  out
}

# Layer - Point range --------------------------------------------------

.visualisation_predicted_pointrange <- function(data,
                                                x1,
                                                y,
                                                color,
                                                group = NULL,
                                                alpha = NULL,
                                                size = NULL,
                                                ci_low = "CI_low",
                                                ci_high = "CI_high",
                                                jitter = FALSE) {
  out <- list(
    geom = "pointrange",
    data = data,
    aes = list(
      x = x1,
      y = y,
      ymin = ci_low,
      ymax = ci_high,
      color = color,
      size = size,
      group = group
    ),
    alpha = alpha
  )

  if (isTRUE(jitter)) {
    insight::check_if_installed("ggplot2")
    out$position <- ggplot2::position_dodge2(width = 0.25)
  }
  out
}

# Layer - Ribbon -------------------------------------------------------------

.visualisation_predicted_iterations <- function(data, x1, fill, ribbon = NULL) {
  data <- bayestestR::reshape_iterations(data)

  # Decrease alpha depending on number of iterations
  alpha <- 1 / exp(log(max(data$iter_group), base = 6))

  if (!is.null(fill)) {
    data$iter_group <- paste0(data$iter_group, "_", data[[fill]])
  }

  out <- list(
    geom = "line",
    data = data,
    aes = list(
      y = "iter_value",
      x = x1,
      group = "iter_group",
      color = fill
    ),
    size = 0.5,
    alpha = alpha
  )
  if (!is.null(ribbon)) out <- utils::modifyList(out, ribbon) # Update with additional args
  out
}

# Layer - Labels --------------------------------------------------------------

.visualisation_predicted_labs <- function(info, x1, y, labs = NULL) {
  out <- list(
    geom = "labs",
    x = x1,
    y = y,
    title = paste0(
      "Predicted response (",
      format(insight::find_formula(info$model)),
      ")"
    )
  )
  if (!is.null(labs)) out <- utils::modifyList(out, labs) # Update with additional args
  out
}


# Utilities ---------------------------------------------------------------

.visualisation_recipe_getrawdata <- function(x, ...) {
  rawdata <- insight::get_data(attributes(x)$model, verbose = FALSE)

  # Add response to data if not there
  y <- insight::find_response(attributes(x)$model)
  if (!y %in% names(rawdata)) rawdata[y] <- insight::get_response(attributes(x)$model, verbose = FALSE)
  rawdata
}
