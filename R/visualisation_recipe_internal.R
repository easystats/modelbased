# Find aes ----------------------------------------------------------------

#' @keywords internal
.find_aes <- function(x, model_info = NULL, numeric_as_discrete = 8) {
  # init basic aes
  data <- as.data.frame(x)
  data$.group <- 1

  att <- attributes(x)
  aes <- list(y = "Predicted", group = ".group")

  # extract information for labels
  model_data <- .safe(insight::get_data(attributes(x)$model, verbose = FALSE))
  model_response <- attributes(x)$response

  # Find predictors
  by <- att$focal_terms

  # flag for ordinal and alike models
  has_response_levels <- isTRUE(
    model_info$is_ordinal |
      model_info$is_multinomial |
      model_info$is_categorical |
      model_info$is_cumulative
  )

  # multivariate response models? if so, we need one more stratification in "by"
  if (has_response_levels && "Response" %in% colnames(data)) {
    by <- c(by, "Response")
    data$Response <- factor(data$Response, levels = unique(data$Response))
  }

  # mixture models? if so, we need one more stratification in "by"
  if (isTRUE(model_info$is_mixture) && "Class" %in% colnames(data)) {
    by <- c(by, "Class")
    data$Class <- factor(data$Class, levels = unique(data$Class))
  }

  # if we have only few numeric values, we don't want a continuous color scale.
  # check whether we can treat numeric as discrete
  if (!isFALSE(numeric_as_discrete) && is.numeric(numeric_as_discrete)) {
    data[by] <- lapply(data[by], function(v) {
      if (is.numeric(v) && insight::n_unique(v) < numeric_as_discrete) {
        formatted <- insight::format_value(v, protect_integers = TRUE)
        v <- factor(formatted, levels = unique(formatted))
      }
      v
    })
  }

  # Main geom
  # ------------------------------------------------------------------------
  if ("estimate_contrasts" %in% att$class) {
    insight::format_error("Automated plotting is not yet implemented for this class.")
  } else if ("estimate_means" %in% att$class) {
    aes$y <- att$coef_name
  } else if ("estimate_slopes" %in% att$class) {
    aes$y <- "Slope"
    if ("Comparison" %in% names(data)) {
      # Insert "Comparison" column as the 2nd by so that it gets plotted as color
      if (length(by) > 1) {
        by[3:(length(by) + 1)] <- by[2:length(by)]
      }
      by[2] <- "Comparison"
    } else if ("p" %in% colnames(data) && length(by) == 1 && is.numeric(data[[by]])) {
      # this is for slopes of two numeric interaction terms (johnson-neymann plots)
      # the slope of the first numeric term is along the y-axis, values for the
      # 2nd numeric term on the x-axis. whenever confidence intervals exclude 0
      # we find the (range of) numeric values for the 2nd term where the interaction
      # is "significant", i.e. p < 0.05. We want to map this to a special aes
      # so we can color the ribbons accordingly
      by <- c(by, "p")
      significant <- data$p < 0.05
      data$p <- "not significant"
      data$p[significant] <- "significant"
    }
  } else if ("estimate_grouplevel" %in% att$class) {
    aes$x <- "Level"
    # find coefficient name, this may differ for Bayesian models
    if (!is.null(att$coef_name) && length(att$coef_name)) {
      aes$y <- att$coef_name
    } else {
      aes$y <- "Coefficient"
    }
    aes$type <- "grouplevel"
    # setup facets
    facet_by <- NULL
    if (insight::n_unique(data$Parameter) > 1) {
      facet_by <- c(facet_by, "Parameter")
      # aes$color <- "Parameter"
    }
    if (insight::n_unique(data$Group) > 1) {
      facet_by <- c(facet_by, "Group")
    }
    if (insight::n_unique(data$Component) > 1) {
      facet_by <- c(facet_by, "Component")
    }
    if (!is.null(facet_by)) {
      data$facet <- data$Group
      if ("Parameter" %in% facet_by) {
        data$facet <- paste0(data$facet, ": ", data$Parameter)
      }
      if ("Component" %in% facet_by) {
        data$facet <- paste0(data$facet, " (", gsub("_", " ", data$Component, fixed = TRUE), ")")
      }
      aes$facet <- "facet"
    }
    aes <- .find_aes_ci(aes, data)
    return(list(aes = aes, data = data))
  }

  # Assign predictors to aes
  if (is.null(by)) {
    by <- att$by
  }
  if (length(by) == 0) {
    insight::format_error("No `by` variable was detected, so nothing to put in the x-axis.")
  }

  # first variable for x-axis - decide whether we have a dot- or a line-plot
  # ------------------------------------------------------------------------
  aes$x <- by[1]
  # If x is a not-numeric, make pointrange
  if (is.numeric(data[[by[1]]])) {
    aes$type <- "ribbon"
  } else {
    aes$type <- "pointrange"
  }

  # second variable, mapped to the color-aes
  # ------------------------------------------------------------------------
  if (length(by) > 1) {
    aes$color <- by[2]
    # if by is of length 2, and the p-value column, *and* we have slopes, then
    # # we have a Johnson-Neyman plot. In this case, we need to re-adjust ".group",
    # which must have an own index for each "part" of the ribbons. ".group" now
    # indicates every switch / flip from significant to non-significant and
    # vice versa
    if ("estimate_slopes" %in% att$class && by[2] == "p") {
      group_index <- 1
      for (i in 2:(nrow(data))) {
        if (data$p[i] != data$p[i - 1]) {
          group_index <- group_index + 1
        }
        data$.group[i] <- group_index
      }
      # for johnson-neymann plots, the "group" aes cannot be assigned to the
      # ribbon geom - instead, it must be part of the "ggplot()" function. this
      # can be achieved by adding the aes as "global_aes" attribute to the
      # returned visualisation_recipe
      aes$group <- NULL
    } else {
      data$.group <- paste(data$.group, data[[by[2]]])
    }
  }

  # third variable, mapped to alpha when numeric, or creates a facet for factors
  # ------------------------------------------------------------------------
  if (length(by) > 2) {
    if (is.numeric(data[[by[3]]])) {
      aes$alpha <- by[3]
    } else {
      aes$facet <- stats::as.formula(paste("~", paste(utils::tail(by, -2), collapse = " * ")))
    }
    data$.group <- paste(data$.group, data[[by[3]]])
  }

  # more than three variable? create facet grids then
  # ------------------------------------------------------------------------
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
  # ------------------------------------------------------------------------
  aes <- .find_aes_ci(aes, data)

  # axis and legend labels
  # ------------------------------------------------------------------------
  if (!is.null(model_data) && !is.null(model_response)) {
    if ("estimate_slopes" %in% att$class) {
      ylab <- att$trend
    } else {
      # response - mapped to the y-axis if not slopes
      ylab <- .safe(attr(model_data[[model_response]], "label", exact = TRUE))
    }
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
  } else {
    aes$ymin <- NA_real_
    aes$ymax <- NA_real_
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
                                  numeric_as_discrete = 8,
                                  ...) {
  # init
  response_scale <- attributes(x)$predict
  model_info <- attributes(x)$model_info

  aes <- .find_aes(x, model_info, numeric_as_discrete)
  data <- aes$data
  aes <- aes$aes
  global_aes <- list()
  layers <- list()
  l <- 1

  # preparation of settings / arguments ----------------------------------

  # check whether point-geoms should be connected by lines
  do_not_join <- "grouplevel"
  if (!join_dots) {
    do_not_join <- c(do_not_join, "pointrange", "point")
  }

  # Don't plot raw data if `predict` is not on the response scale
  if (!is.null(response_scale) && !response_scale %in% c("prediction", "response", "expectation", "invlink(link)")) {
    show_data <- FALSE
  }

  # Don't plot raw data for transformed responses with no back-transformation
  transform <- attributes(x)$transform

  if (isTRUE(model_info$is_linear) && !isTRUE(transform)) {
    # add information about response transformation
    trans_fun <- .safe(insight::find_transformation(attributes(x)$model))
    if (!is.null(trans_fun) && all(trans_fun != "identity")) {
      show_data <- FALSE
    }
  }


  # add raw data as first layer ----------------------------------
  if (show_data) {
    layers[[paste0("l", l)]] <- .visualization_recipe_rawdata(x, aes)
    # Update with additional args
    if (!is.null(point)) layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], point)
    l <- l + 1
  }


  # intercept line for slopes ----------------------------------
  if (inherits(x, "estimate_slopes")) {
    layers[[paste0("l", l)]] <- insight::compact_list(list(
      geom = "hline",
      yintercept = 0,
      alpha = 1 / 2,
      linetype = "dashed"
    ))
    l <- l + 1
    global_aes$group <- ".group"
  }


  # Uncertainty -----------------------------------
  if (!identical(ribbon, "none") && aes$type == "ribbon" && is.null(aes$alpha)) {
    for (i in seq_along(aes$ymin)) {
      # base list elements
      aes_list <- list(
        y = aes$y,
        x = aes$x,
        ymin = aes$ymin[i],
        ymax = aes$ymax[i],
        fill = aes$color
      )
      # optionally add group aes, if not in global_aes and not null
      if (!"group" %in% names(global_aes) || !is.null(aes$group)) {
        aes_list$group <- aes$group
      }
      layers[[paste0("l", l)]] <- list(
        geom = "ribbon",
        data = data,
        aes = aes_list,
        alpha = 1 / 3
      )
      if (!is.null(ribbon)) layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], ribbon)
      l <- l + 1
    }
  }

  # Main ----------------------------------

  # connecting lines between point geoms
  if (!aes$type %in% do_not_join) {
    # base list elements
    aes_list <- list(
      y = aes$y,
      x = aes$x,
      color = aes$color,
      alpha = aes$alpha
    )
    # optionally add group aes, if not in global_aes and not null
    if (!"group" %in% names(global_aes) || !is.null(aes$group)) {
      aes_list$group <- aes$group
    }
    layers[[paste0("l", l)]] <- list(
      geom = "line",
      data = data,
      aes = aes_list
    )
    if (!is.null(aes$color) && aes$type %in% c("pointrange", "point")) {
      layers[[paste0("l", l)]]$position <- "dodge"
      layers[[paste0("l", l)]]$width <- 0.2
    }
    if (!is.null(line)) layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], line)
    l <- l + 1
  }

  # points with error bars
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


  # probability scale? ----------------------------------
  if (
    !is.null(response_scale) &&
      response_scale %in% c("response", "expectation", "invlink(link)", "prob", "probs") &&
      isTRUE(
        model_info$is_logit |
          model_info$is_binomial |
          model_info$is_orderedbeta |
          model_info$is_beta |
          model_info$is_ordinal |
          model_info$is_multinomial
      )
  ) {
    # nolint
    layers[[paste0("l", l)]] <- list(
      geom = "scale_y_continuous",
      labels = insight::format_value(
        x = pretty(sort(c(data[[aes$ymin]], data[[aes$ymax]]))),
        as_percent = TRUE,
        digits = 0
      ),
      breaks = pretty(sort(c(data[[aes$ymin]], data[[aes$ymax]])))
    )
    l <- l + 1
  }


  # Out
  class(layers) <- unique(c("visualisation_recipe", "see_visualisation_recipe", class(layers)))
  attr(layers, "data") <- data
  attr(layers, "global_aes") <- insight::compact_list(global_aes)
  layers
}


# Raw data ----------------------------------------------------------------


#' @keywords internal
.visualization_recipe_rawdata <- function(x, aes) {
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
  if (insight::model_info(model, response = 1)$is_binomial) {
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

  # check if we have matching columns in the raw data - some functions,
  # likes slopes, have mapped these aes to other columns that are not part
  # of the raw data - we set them to NULL
  if (!is.null(aes$color) && !aes$color %in% colnames(rawdata)) {
    out$aes$color <- NULL
  }
  if (!is.null(aes$alpha) && !aes$alpha %in% colnames(rawdata)) {
    out$aes$alpha <- NULL
  }

  # set default alpha, if not mapped by aes
  if (is.null(aes$alpha)) {
    out$alpha <- 1 / 3
  } else {
    out$alpha <- NULL
  }

  out
}
