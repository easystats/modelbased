# Find aes ----------------------------------------------------------------


#' @keywords internal
.find_aes <- function(x) {
  data <- as.data.frame(x)
  data$.group <- 1

  att <- attributes(x)
  aes <- list(
    y = "Predicted",
    group = ".group"
  )

  # extract information for labels
  model_data <- .safe(insight::get_data(attributes(x)$model, verbose = FALSE))
  model_response <- attributes(x)$response

  # Find predictors
  by <- att$focal_terms

  # Main geom
  if ("estimate_contrasts" %in% att$class) {
    insight::format_error("Automated plotting is not yet implemented for this class.")
  } else if ("estimate_means" %in% att$class) {
    aes$y <- att$coef_name
  } else if ("estimate_slopes" %in% att$class) {
    aes$y <- "Slope"
    if("Comparison" %in% names(data)) {
      # Insert "Comparison" column as the 2nd by so that it gets plotted as color
      if(length(by) > 1) by[3:(length(by)+1)] <- by[2:length(by)]
      by[2] <- "Comparison"
    }
  } else if ("estimate_grouplevel" %in% att$class) {
    aes$x <- "Level"
    aes$y <- "Coefficient"
    aes$type <- "grouplevel"
    if (length(unique(data$Parameter)) > 1) {
      aes$color <- "Parameter"
      data$.group <- paste(data$.group, data$Parameter)
    }
    if (length(unique(data$Group)) > 1) aes$facet <- "Group"
    aes <- .find_aes_ci(aes, data)
    return(list(aes = aes, data = data))
  }


  # Assign predictors to aes
  if (is.null(by)) {
    by <- att$by
  }
  if (length(by) == 0) {
    insight::format_error("No `by` variable was detected, so nothing to put in the x-axis.")
  } else if (length(by) > 0) {
    aes$x <- by[1]
    # If x is a not-numeric, make pointrange
    if (is.numeric(data[[by[1]]])) {
      aes$type <- "ribbon"
    } else {
      aes$type <- "pointrange"
    }
  }
  if (length(by) > 1) {
    aes$color <- by[2]
    data$.group <- paste(data$.group, data[[by[2]]])
  }
  if (length(by) > 2) {
    if (is.numeric(data[[by[3]]])) {
      aes$alpha <- by[3]
    } else {
      aes$facet <- stats::as.formula(paste("~", paste(utils::tail(by, -2), collapse = " * ")))
    }
    data$.group <- paste(data$.group, data[[by[3]]])
  }
  if (length(by) > 3) {
    aes$facet <- NULL
    # we have to switch variables 3 and 4, due to regression formula
    remaining <- paste(utils::tail(by, -2))
    aes$grid <- stats::as.formula(paste(
      remaining[2],
      "~",
      paste(setdiff(remaining, remaining[2]), collapse = "*")
    ))
  }

  # CI
  aes <- .find_aes_ci(aes, data)

  # axis and legend labels
  if (!is.null(model_data) && !is.null(model_response)) {
    # response - mapped to the y-axis
    ylab <- .safe(attr(model_data[[model_response]], "label", exact = TRUE))
    # fix default y-label, if necessary
    y_prefix <- aes$y
    if (y_prefix == "Predicted") {
      y_prefix <- "Predicted value"
    }
    # set y-label based on labelled data, or variable name
    if (is.null(ylab)) {
      ylab <- paste(y_prefix, "of", model_response)
    } else {
      ylab <- paste(y_prefix, "of", ylab)
    }
    # main predictor - mapped to x-axis
    xlab <- .safe(attr(model_data[[by[1]]], "label", exact = TRUE))
    # first grouping variable (2nd in "by") - mapped to legend
    if (length(by) > 1) {
      colour <- .safe(attr(model_data[[by[[2]]]], "label", exact = TRUE))
    } else {
      colour <- NULL
    }
    aes$labs <- insight::compact_list(list(y = ylab, x = xlab, colour = colour))
  }

  list(aes = aes, data = data)
}


#' @keywords internal
.find_aes_ci <- function(aes, data) {
  ci_lows <- rev(grep("CI_low", names(data), fixed = TRUE, value = TRUE))
  ci_highs <- rev(grep("CI_high", names(data), fixed = TRUE, value = TRUE))
  if (length(ci_lows) > 0) {
    aes$ymin <- ci_lows
    aes$ymax <- ci_highs
  }
  aes
}


# Workhorse function ------------------------------------------------------


#' @keywords internal
.visualization_recipe <- function(x,
                                  show_data = TRUE,
                                  point = NULL,
                                  line = NULL,
                                  pointrange = NULL,
                                  ribbon = NULL,
                                  facet = NULL,
                                  grid = NULL,
                                  join_dots = TRUE,
                                  ...) {
  aes <- .find_aes(x)
  data <- aes$data
  aes <- aes$aes
  layers <- list()
  l <- 1

  # check whether point-geoms should be connected by lines
  do_not_join <- "grouplevel"
  if (!join_dots) {
    do_not_join <- c(do_not_join, "pointrange")
  }

  # TODO: Don't plot raw data if `predict` is not on the response scale
  if (show_data) {
    layers[[paste0("l", l)]] <- .visualization_recipe_rawdata(x, aes)
    # Update with additional args
    if (!is.null(point)) layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], point)
    l <- l + 1
  }

  # Uncertainty -----------------------------------
  if (aes$type == "ribbon" && is.null(aes$alpha)) {
    for (i in seq_len(length(aes$ymin))) {
      layers[[paste0("l", l)]] <- list(
        geom = "ribbon",
        data = data,
        aes = list(
          y = aes$y,
          x = aes$x,
          ymin = aes$ymin[i],
          ymax = aes$ymax[i],
          fill = aes$color,
          group = aes$group
        ),
        alpha = 1 / 3
      )
      if (!is.null(ribbon)) layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], ribbon)
      l <- l + 1
    }
  }

  # Main ----------------------------------
  if (!aes$type %in% do_not_join) {
    layers[[paste0("l", l)]] <- list(
      geom = "line",
      data = data,
      aes = list(
        y = aes$y,
        x = aes$x,
        color = aes$color,
        group = aes$group,
        alpha = aes$alpha
      )
    )
    if (!is.null(aes$color) && aes$type == "pointrange") {
      layers[[paste0("l", l)]]$position <- "dodge"
      layers[[paste0("l", l)]]$width <- 0.2
    }
    if (!is.null(line)) layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], line)
    l <- l + 1
  }


  if (aes$type %in% c("pointrange", "grouplevel")) {
    layers[[paste0("l", l)]] <- list(
      geom = "pointrange",
      data = data,
      aes = list(
        y = aes$y,
        x = aes$x,
        ymin = aes$ymin,
        ymax = aes$ymax,
        color = aes$color,
        group = aes$group,
        alpha = aes$alpha
      )
    )
    if (!is.null(aes$color)) {
      layers[[paste0("l", l)]]$position <- "dodge"
      layers[[paste0("l", l)]]$width <- 0.2
    }
    if (!is.null(pointrange)) layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], pointrange)
    l <- l + 1
  }
  if (aes$type == "grouplevel") {
    layers[[paste0("l", l)]] <- list(geom = "coord_flip")
    l <- l + 1
  }

  # grids and facets ----------------------------------
  if (!is.null(aes$facet)) {
    layers[[paste0("l", l)]] <- list(
      geom = "facet_wrap",
      data = data,
      facets = aes$facet
    )
    if (!is.null(facet)) layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], facet)
    l <- l + 1
  }
  if (!is.null(aes$grid)) {
    layers[[paste0("l", l)]] <- list(
      geom = "facet_grid",
      data = data,
      rows = aes$grid,
      scales = "free_x"
    )
    if (!is.null(grid)) layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], facet)
    l <- l + 1
  }

  # add axis and legend labels ----------------------------------
  if (!is.null(aes$labs)) {
    layers[[paste0("l", l)]] <- insight::compact_list(list(
      geom = "labs",
      x = aes$labs$x,
      y = aes$labs$y,
      colour = aes$labs$colour,
      fill = aes$labs$colour
    ))
    l <- l + 1
  }

  # Out
  class(layers) <- unique(c("visualisation_recipe", "see_visualisation_recipe", class(layers)))
  attr(layers, "data") <- data
  layers
}


# Raw data ----------------------------------------------------------------


#' @keywords internal
.visualization_recipe_rawdata <- function(x, aes) {
  # TODO: In the main function, don't forget to NOT add raw data when `predict` is not "response"

  model <- attributes(x)$model
  rawdata <- insight::get_data(model, verbose = FALSE)

  # Add response to data if not there
  y <- insight::find_response(attributes(x)$model)
  if (!y %in% names(rawdata)) rawdata[y] <- insight::get_response(attributes(x)$model, verbose = FALSE)

  if (aes$type == "pointrange" && !is.numeric(rawdata[[aes$x]])) {
    geom <- "jitter"
  } else {
    geom <- "point"
  }

  # Default changes for binomial models
  shape <- 16
  stroke <- 0
  if (insight::model_info(model)$is_binomial) {
    shape <- "|"
    stroke <- 1
  }

  out <- list(
    geom = geom,
    data = rawdata,
    aes = list(
      y = y,
      x = aes$x,
      color = aes$color,
      alpha = aes$alpha
    ),
    height = 0,
    shape = shape,
    stroke = stroke
  )

  # set default alpha, it not mapped by aes
  if (is.null(aes$alpha)) {
    out$alpha <- 1 / 3
  } else {
    out$alpha <- NULL
  }

  out
}
