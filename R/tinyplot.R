#' @rdname visualisation_recipe.estimate_predicted
#'
#' @examplesIf all(insight::check_if_installed(c("tinyplot", "marginaleffects"), quietly = TRUE))
#' # ==============================================
#' # tinyplot
#' # ==============================================
#' \dontrun{
#' # todo...
#' }
#' @exportS3Method tinyplot::tinyplot
tinyplot.estimate_means <- function(x,
                                    show_data = FALSE,
                                    join_dots = NULL,
                                    numeric_as_discrete = NULL,
                                    ...) {
  insight::check_if_installed("tinyplot")

  # init
  response_scale <- attributes(x)$predict
  model_info <- attributes(x)$model_info

  # set defaults
  if (is.null(join_dots)) {
    join_dots <- getOption("modelbased_join_dots", TRUE)
  }
  if (is.null(numeric_as_discrete)) {
    numeric_as_discrete <- getOption("modelbased_numeric_as_discrete", 8)
  }

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


  ## TODO: add raw data as first layer ----------------------------------


  # formula for tinyplot ----------------------------------
  if (is.null(aes$color)) {
    plot_description <- stats::as.formula(paste(aes$y, "~", aes$x))
  } else {
    plot_description <- stats::as.formula(paste(aes$y, "~", aes$x, "|", aes$color))
  }
  if (is.null(aes$facet)) {
    facet_description <- NULL
  } else {
    facet_description <- stats::as.formula(paste("~", aes$facet, collapse = " + "))
  }

  fun_args <- list(plot_description, data = data, facet = facet_description, type = aes$type)
  if (!is.null(aes$xmin)) {
    fun_args$xmin <- stats::as.formula(paste("~", aes$xmin))
  }
  if (!is.null(aes$xmax)) {
    fun_args$xmax <- stats::as.formula(paste("~", aes$xmax))
  }
  if (!is.null(aes$ymin)) {
    fun_args$ymin <- stats::as.formula(paste("~", aes$ymin))
  }
  if (!is.null(aes$ymax)) {
    fun_args$ymax <- stats::as.formula(paste("~", aes$ymax))
  }

  do.call(tinyplot::tinyplot, insight::compact_list(c(fun_args, list(...))))
}
