#' @rdname visualisation_recipe.estimate_predicted
#' @param ... Other arguments passed to \code{\link[tinyplot]{tinyplot}}.
#'
#' @examplesIf all(insight::check_if_installed(c("tinyplot", "marginaleffects"), quietly = TRUE))
#' # ==============================================
#' # tinyplot
#' # ==============================================
#' \donttest{
#' data(efc, package = "modelbased")
#' efc <- datawizard::to_factor(efc, c("e16sex", "c172code", "e42dep"))
#' m <- lm(neg_c_7 ~ e16sex + c172code + barthtot, data = efc)
#'
#' em <- estimate_means(m, "c172code")
#' tinyplot::plt(em)
#'
#' em <- estimate_means(m, "barthtot")
#' tinyplot::plt(em)
#'
#' m <- lm(neg_c_7 ~ e16sex * c172code + e42dep, data = efc)
#' em <- estimate_means(m, c("e16sex", "c172code"))
#' tinyplot::plt(em)
#' }
#' @exportS3Method tinyplot::tinyplot
tinyplot.estimate_means <- function(
  x,
  show_data = FALSE,
  numeric_as_discrete = NULL,
  ...
) {
  insight::check_if_installed("tinyplot")

  # init --------------------------------------------------
  response_scale <- attributes(x)$predict
  model_info <- attributes(x)$model_info

  # set defaults
  if (is.null(numeric_as_discrete)) {
    numeric_as_discrete <- getOption("modelbased_numeric_as_discrete", 8)
  }

  # we re-use the ggplot function here to retrieve the aesthetics and data. we
  # now need to extract the aesthetics and data and use it to create a tinyplot
  # object
  aes <- .find_aes(x, model_info, numeric_as_discrete)
  data <- aes$data
  aes <- aes$aes

  # save additional arguments, will pass via do.call to tinyplot
  dots <- list(...)

  # preparation of settings / arguments ----------------------------------

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

  # handle non-standard plot types -------------------------------

  if (aes$type == "grouplevel") {
    aes$type <- "pointrange"
    dots$flip <- TRUE
  }

  # base elements as formula for tinyplot -------------------------------

  # plot formula
  if (is.null(aes$color)) {
    plot_formula <- paste(aes$y, "~", aes$x)
  } else {
    plot_formula <- paste(aes$y, "~", aes$x, "|", aes$color)
  }
  plot_description <- stats::as.formula(plot_formula)

  # facets, also as formula
  if (is.null(dots$facet) && !is.null(aes$facet)) {
    dots$facet <- stats::as.formula(paste("~", aes$facet, collapse = " + "))
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

  ## TODO: legend labels?
  ## TODO: show residuals?

  # x/y labels --------------------------------
  dots$xlab <- aes$labs$x
  dots$ylab <- aes$labs$y

  # add aesthetics to the plot description
  plot_args <- insight::compact_list(c(
    list(plot_description, data = data, type = aes$type),
    plot_args,
    dots
  ))

  # add data points if requested --------------------------------

  if (show_data) {
    # extract raw data from the model
    model <- attributes(x)$model
    rawdata <- as.data.frame(insight::get_data(model, verbose = FALSE))

    # set alpha
    if (is.null(dots$alpha)) {
      dots$alpha <- 0.3
    }

    # add layer
    plot_args$draw <- {
      tinyplot::tinyplot(
        # we need the original response name for the data points
        # so we update the formula for the plot description
        stats::reformulate(
          attr(stats::terms(plot_description), "term.labels"),
          response = insight::find_response(model)
        ),
        data = rawdata,
        facet = dots$facet,
        type = "jitter",
        add = TRUE,
        alpha = dots$alpha
      )
    }
  }
  browser()

  # plot it!
  do.call(tinyplot::tinyplot, plot_args)
}

#' @exportS3Method tinyplot::tinyplot
tinyplot.estimate_predicted <- tinyplot.estimate_means

#' @exportS3Method tinyplot::tinyplot
tinyplot.estimate_slopes <- tinyplot.estimate_means

#' @exportS3Method tinyplot::tinyplot
tinyplot.estimate_grouplevel <- tinyplot.estimate_means
