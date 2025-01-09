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
    if(is.numeric(data[[by[2]]])) data[[by[2]]] <- as.factor(data[[by[2]]])
  }
  if(length(by) > 2) {
    aes$alpha <- by[3]
    data$.group <- paste(data[[by[2]]], "_", data[[by[3]]])
    aes$group <- ".group"
  }
  if(length(by) > 3) {
    warning("Only the first three `by` predictors will be visualized.")
  }

  # CI
  if("CI_low" %in% data) {
    aes$ymin <- x$CI_low
    aes$ymax <- x$CI_high
  }

  list(aes=aes, data=data)
}




#' @keywords internal
.visualization_recipe <- function(x) {

  aes <- .find_aes(x)
  data <- aes$data
  aes <- aes$aes
  layers <- list()
  l <- 1


  # TODO: add raw data plotting

  # Line -----------------------------------
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
  l <- l + 1

  # Main -----------------------------------
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
    l <- l + 1
  }
  if(aes$type == "ribbon" & is.null(aes$alpha)) {
    layers[[paste0("l", l)]] <- list(
      geom = "ribbon",
      data = data,
      aes = list(
        y = aes$y,
        x = aes$x,
        ymin = aes$ymin,
        ymax = aes$ymax,
        fill = aes$color,
        color = NULL,
        group = aes$group
      ),
      alpha = 1/3
    )
    l <- l + 1
  }

  # Out
  class(layers) <- unique(c("visualisation_recipe", "see_visualisation_recipe", class(layers)))
  attr(layers, "data") <- data
  layers
}

