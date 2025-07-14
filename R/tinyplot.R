#' @rdname visualisation_recipe.estimate_predicted
#'
#' @examplesIf all(insight::check_if_installed(c("tinyplot", "marginaleffects"), quietly = TRUE))
#' # ==============================================
#' # tinyplot
#' # ==============================================
#' \dontrun{
#' # todo...
#' }
#' @export
tinyplot.estimate_means <- function(x, ...) {
  insight::check_if_installed("tinyplot")

  # init
  response_scale <- attributes(x)$predict
  model_info <- attributes(x)$model_info

  # we re-use the ggplot function here to retrieve the aesthetics and data. we
  # now need to extract the aesthetics and data and use it to create a tinyplot
  # object
  aes <- .find_aes(x, model_info, numeric_as_discrete)
  data <- aes$data
  aes <- aes$aes

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
