#' @rdname visualisation_recipe.estimate_predicted
#'
#' @examplesIf all(insight::check_if_installed(c("tinyplot", "marginaleffects"), quietly = TRUE))
#' # ==============================================
#' # tinyplot
#' # ==============================================
#' \dontrun{
#' data(efc, package = "modelbased")
#' efc <- datawizard::to_factor(efc, c("e16sex", "c172code", "e42dep"))
#' m <- lm(neg_c_7 ~ e16sex + c172code + barthtot, data = efc)
#'
#' em <- estimate_means(m, "c172code")
#' tinyplot::plt(em)
#'
#' em <- estimate_means(m, "barthtot")
#' tinyplot::plt(em)
#' m <- lm(neg_c_7 ~ e16sex * c172code + e42dep, data = efc)
#' em <- estimate_means(m, c("e16sex", "c172code")) |>
#' tinyplot::plt(em)
#' }
#' @exportS3Method tinyplot::tinyplot
tinyplot.estimate_means <- function(
  x,
  show_data = FALSE,
  join_dots = NULL,
  numeric_as_discrete = NULL,
  ...
) {
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
  dots <- list(...)

  # preparation of settings / arguments ----------------------------------

  # check whether point-geoms should be connected by lines
  do_not_join <- "grouplevel"
  if (!join_dots) {
    do_not_join <- c(do_not_join, "pointrange", "point")
  }

  # Don't plot raw data if `predict` is not on the response scale
  if (
    !is.null(response_scale) &&
      !response_scale %in% c("prediction", "response", "expectation", "invlink(link)")
  ) {
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

  # base elements as formula for tinyplot -------------------------------

  # plot formula
  if (is.null(aes$color)) {
    plot_formula <- paste(aes$y, "~", aes$x)
  } else {
    plot_formula <- paste(aes$y, "~", aes$x, "|", aes$color)
  }
  plot_description <- stats::as.formula(plot_formula)

  # facets, also as formula
  if (is.null(dots$facet)) {
    if (is.null(aes$facet)) {
      facet <- NULL
    } else {
      facet <- stats::as.formula(paste("~", aes$facet, collapse = " + "))
    }
  } else {
    facet <- dots$facet
    dots$facet <- NULL
  }

  # add remaining aesthetics to the plot description as symbols
  elements <- c("xmin", "xmax", "ymin", "ymax")
  plot_args <- lapply(elements, function(el) {
    if (is.null(aes[[el]])) {
      return(NULL)
    }
    as.symbol(aes[[el]])
  })
  names(plot_args) <- elements

  # add aesthetics to the plot description
  plot_args <- c(
    list(plot_description, facet = facet, data = data, type = aes$type),
    plot_args
  )

  # plot it!
  do.call(tinyplot::tinyplot, insight::compact_list(c(plot_args, dots)))
}

#' @exportS3Method tinyplot::tinyplot
tinyplot.estimate_predicted <- tinyplot.estimate_means

#' @exportS3Method tinyplot::tinyplot
tinyplot.estimate_slopes <- tinyplot.estimate_means

#' @exportS3Method tinyplot::tinyplot
tinyplot.estimate_grouplevel <- tinyplot.estimate_means
