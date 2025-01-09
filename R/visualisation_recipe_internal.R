# Find aes ----------------------------------------------------------------


#' @keywords internal
.find_aes <- function(x) {
  data <- as.data.frame(x)
  att <- attributes(x)
  aes <- list(
    y = "Predicted",
    type = "ribbon",
    group = 1
  )

  # Main geom
  if("estimate_contrasts" %in% att$class) {
    stop("Automated plotting is not yet implemented for this class.")
  } else if("estimate_means" %in% att$class) {
    aes$y <- att$coef_name
    aes$type <- "pointrange"
  } else if("estimate_slopes" %in% att$class) {
    aes$y <- "Coefficient"
  }

  # Find predictors
  by <- att$by
  if(length(by) == 0) {
    stop("No `by` variable was detected, so nothing to put in the x-axis.")
  } else if(length(by) > 0) {
    aes$x <- by[1]
    # If x is a not-numeric, make pointrange
    if(!is.numeric(data[[by[1]]])) aes$type <- "pointrange"
  }
  if(length(by) > 1) {
    aes$color <- by[2]
    aes$group <- by[2]
    # If color is a numeric variable, convert it to a factor
    # if(is.numeric(data[[by[2]]])) data[[by[2]]] <- as.factor(data[[by[2]]])
  }
  if(length(by) > 2) {
    aes$alpha <- by[3]
    data$.group <- paste(data[[by[2]]], "_", data[[by[3]]])
    aes$group <- ".group"
  }
  if(length(by) > 3) {
    aes$facet <- as.formula(paste("~", paste(tail(by, -3), collapse = " * ")))
  }

  # CI
  ci_lows <- rev(grep("CI_low", names(data), fixed = TRUE, value = TRUE))
  ci_highs <- rev(grep("CI_high", names(data), fixed = TRUE, value = TRUE))
  if(length(ci_lows) > 0) {
    aes$ymin <- ci_lows
    aes$ymax <- ci_highs
  }

  list(aes=aes, data=data)
}




#' @keywords internal
.visualization_recipe <- function(x,
                                  show_data=TRUE,
                                  point=NULL,
                                  line=NULL,
                                  pointrange=NULL,
                                  ribbon=NULL,
                                  facet=NULL,
                                  ...) {

  aes <- .find_aes(x)
  data <- aes$data
  aes <- aes$aes
  layers <- list()
  l <- 1


  # TODO: Don't plot raw data if `predict` is not on the response scale
  if (show_data) {
    layers[[paste0("l", l)]] <- .visualization_recipe_rawdata(x, aes)
    # Update with additional args
    if (!is.null(point)) layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], point)
    l <- l + 1
  }


  # Uncertainty -----------------------------------
  if(aes$type == "ribbon" & is.null(aes$alpha)) {
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
          color = NULL,
          group = aes$group
        ),
        alpha = 1/3
      )
      if (!is.null(ribbon)) layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], ribbon)
      l <- l + 1
    }
  }

  # Main ----------------------------------
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
  if (!is.null(line)) layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], line)
  l <- l + 1


  if(aes$type == "pointrange") {
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
    if (!is.null(pointrange)) layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], pointrange)
    l <- l + 1
  }
  if(!is.null(aes$facet)) {
    layers[[paste0("l", l)]] <- list(
      geom = "facet_wrap",
      data = data,
      facets = aes$facet
    )
    if (!is.null(facet)) layers[[paste0("l", l)]] <- utils::modifyList(layers[[paste0("l", l)]], facet)
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

  if(aes$type == "pointrange" & !is.numeric(rawdata[[aes$x]])) {
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

  list(
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
}

