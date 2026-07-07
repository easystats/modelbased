#' @rdname visualisation_recipe.estimate_predicted
#' @param type The type of `tinyplot` visualization. It is recommended that
#' users leave as `NULL` (the default), in which case the plot type will be
#' determined automatically by the underlying `modelbased` object.
#' @param dodge Dodge value for grouped plots. If `NULL` (the default), then
#' the dodging behavior is determined by the number of groups and
#' `getOption("modelbased_tinyplot_dodge")`.
#' @param ... Other arguments passed to \code{\link[tinyplot]{tinyplot}}.
#'
#' @examplesIf all(insight::check_if_installed(c("tinyplot", "marginaleffects"), quietly = TRUE))
#' # ==============================================
#' # tinyplot
#' # ==============================================
#' \donttest{
#' library(tinyplot)
#' data(efc, package = "modelbased")
#' efc <- datawizard::to_factor(efc, c("e16sex", "c172code", "e42dep"))
#' m <- lm(neg_c_7 ~ e16sex + c172code + barthtot, data = efc)
#'
#' em <- estimate_means(m, "c172code")
#' plt(em)
#'
#' # pass additional tinyplot arguments for customization, e.g.
#' plt(em, theme = "classic")
#' plt(em, theme = "classic", flip = TRUE)
#' # etc.
#'
#' # Aside: use tinyplot::tinytheme() to set a persistent theme
#' tinytheme("classic")
#'
#' # continuous variable example
#' em <- estimate_means(m, "barthtot")
#' plt(em)
#'
#' # grouped example
#' m <- lm(neg_c_7 ~ e16sex * c172code + e42dep, data = efc)
#' em <- estimate_means(m, c("e16sex", "c172code"))
#' plt(em)
#'
#' # use plt_add (alias tinyplot_add) to add layers
#' plt_add(type = "l", lty = 2)
#'
#' # Reset to default theme
#' tinytheme()
#'
#' # facets
#' # ----------------------
#' # when using facets, the `tinyplot()` method checks if the legend is
#' # redundant (because it already appears in the facets), and if so, it
#' # removes the legend. Set `legend = TRUE` to add it back.
#'
#' data(efc, package = "modelbased")
#' # convert to factors, assign labels. we use datawizard::to_factor() in
#' # the second row to automatically assign value labels as factor levels.
#' # because labels are too long for `c172code`, we assign new labels using
#' # `as.factor()`
#' efc$c172code <- factor(efc$c172code, labels = c("low", "mid", "high"))
#' efc$e42dep <- datawizard::to_factor(efc$e42dep)
#' # fit model
#' m <- lm(neg_c_7 ~ c172code * e42dep, data = efc)
#' em <- estimate_means(m, c("c172code", "e42dep"))
#'
#' # for facets, it can be useful to remove dodging
#' plt(em, facet = ~e42dep, dodge = 0, theme = "float")
#'
#' # remove x-axis limits adjustments with `xlim`
#' plt(
#'   em,
#'   facet = ~e42dep,
#'   dodge = 0,
#'   theme = "float",
#'   xlim = c(1, 3),
#'   grid = TRUE
#' )
#'
#' # add back legend
#' plt(em, facet = ~e42dep, legend = TRUE)
#' }
#' @exportS3Method tinyplot::tinyplot
tinyplot.estimate_means <- function(
  x,
  type = NULL,
  dodge = NULL,
  show_data = FALSE,
  collapse_group = NULL,
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

  # type placeholder
  if (!is.null(type)) {
    aes$type <- type
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
  # disable redundant x/y axes for facets
  if (!is.null(dots$facet) && is.null(dots$frame)) {
    dots$frame <- FALSE
  }

  # move geoms on x-axis closer together
  if (is.null(dots$xlim) && !is.numeric(data[[aes$x]])) {
    n_categories <- insight::n_unique(data[[aes$x]])
    if (!is.null(n_categories)) {
      dots$xlim <- c(0.5, n_categories + 0.5)
    }
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

  # dodging -------------------------------

  # Set dodge value for grouped point or pointrange plots.
  # The value 0.07 was chosen to reduce overlap in this context; adjust via
  # option if needed.

  dodge_value <- if (!is.null(dodge)) {
    dodge
  } else {
    getOption("modelbased_tinyplot_dodge", 0.07)
  }
  if (
    !is.null(aes$color) &&
      aes$type %in% c("pointrange", "point", "l", "errorbar", "ribbon")
  ) {
    dots$dodge <- dodge_value
  }

  ## TODO: show residuals?

  # x/y labels --------------------------------
  dots$xlab <- aes$labs$x
  dots$ylab <- aes$labs$y

  # legend labels --------------------------------

  # we also need to account for custom legend options passed through dots
  if (is.null(dots$legend)) {
    # check if legend is already in facets - if so, we don't need a legend
    if (!is.null(dots$facet) && all(all.vars(dots$facet) %in% aes$color)) {
      dots$legend <- FALSE
    } else {
      dots$legend <- list(title = aes$labs$colour)
    }
  } else if (inherits(dots$legend, "list")) {
    if (!("title" %in% names(dots$legend))) {
      dots$legend <- utils::modifyList(
        dots$legend,
        list(title = aes$labs$colour),
        keep.null = TRUE
      )
    }
  } else if (!isFALSE(dots$legend)) {
    dots$legend <- tryCatch(
      utils::modifyList(
        as.list(dots$legend),
        list(title = aes$labs$colour),
        keep.null = TRUE
      ),
      error = function(e) dots$legend
    )
  }

  # add aesthetics to the plot description
  plot_args <- insight::compact_list(c(
    list(plot_description, data = data, type = aes$type),
    plot_args,
    dots
  ))

  # add data points if requested --------------------------------

  if (show_data) {
    # extract raw data from the model
    model <- insight::get_model(x)
    if (is.null(collapse_group)) {
      rawdata <- as.data.frame(insight::get_data(model, verbose = FALSE))
    } else {
      rawdata <- collapse_by_group(x, model, collapse_by = collapse_group)
    }

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

  # plot it!
  suppressWarnings(do.call(tinyplot::tinyplot, plot_args))
}

#' @exportS3Method tinyplot::tinyplot
tinyplot.estimate_predicted <- tinyplot.estimate_means

#' @exportS3Method tinyplot::tinyplot
tinyplot.estimate_slopes <- tinyplot.estimate_means

#' @exportS3Method tinyplot::tinyplot
tinyplot.estimate_grouplevel <- tinyplot.estimate_means
