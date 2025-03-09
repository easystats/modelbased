#' @rdname get_emmeans
#'
#' @examplesIf insight::check_if_installed("marginaleffects", quietly = TRUE)
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#' get_marginaltrends(model, trend = "Petal.Length", by = "Species")
#' get_marginaltrends(model, trend = "Petal.Length", by = "Petal.Length")
#' get_marginaltrends(model, trend = "Petal.Length", by = c("Species", "Petal.Length"))
#' @export
get_marginaltrends <- function(model,
                               trend = NULL,
                               by = NULL,
                               ci = 0.95,
                               p_adjust = "none",
                               transform = NULL,
                               add_iterations = FALSE,
                               verbose = TRUE,
                               ...) {
  # check if available
  insight::check_if_installed("marginaleffects")
  dots <- list(...)

  # model details
  model_info <- insight::model_info(model, verbose = FALSE)

  # Guess arguments
  trend <- .guess_marginaltrends_arguments(model, trend, verbose, ...)

  # First step: create a data grid --------------------------------------------
  # ---------------------------------------------------------------------------

  # data grid only when we have by predictors
  if (is.null(by)) {
    datagrid <- datagrid_info <- NULL
  } else {
    dg_args <- list(
      model,
      by = by,
      factors = "all",
      include_random = TRUE,
      verbose = FALSE
    )
    # add user-arguments from "...", but remove those arguments that are already set
    dots[c("by", "factors", "include_random", "verbose")] <- NULL
    dg_args <- insight::compact_list(c(dg_args, dots))

    # Get corresponding datagrid (and deal with particular ats)
    datagrid <- do.call(insight::get_datagrid, dg_args)
    datagrid_info <- attributes(datagrid)
  }

  # Second step: prepare arguments for marginaleffects ------------------------
  # ---------------------------------------------------------------------------

  # sanity check
  if (!is.null(datagrid)) {
    datagrid <- as.data.frame(datagrid)
  }

  # remove user-arguments from "..." that will be used when calling marginaleffects
  dots[c("by", "conf_level", "digits")] <- NULL

  # handle weights - argument is named "wts" in marginal effects
  if (!is.null(dots$weights)) {
    dots$wts <- dots$weights
    dots$weights <- NULL
  }

  # setup arguments again
  fun_args <- insight::compact_list(c(
    list(
      model,
      variables = trend,
      by = datagrid_info$at_specs$varname,
      newdata = datagrid,
      conf_level = ci
    ),
    dots
  ))

  # Third step: compute marginal slopes ---------------------------------------
  # ---------------------------------------------------------------------------

  # Compute stuff
  estimated <- suppressWarnings(do.call(marginaleffects::avg_slopes, fun_args))

  # Fourth step: back-transform response --------------------------------------
  # ---------------------------------------------------------------------------

  # transform reponse?
  if (isTRUE(transform)) {
    trans_fun <- insight::get_transformation(model, verbose = FALSE)$inverse
  } else {
    trans_fun <- transform
  }
  # if we have back-transformation, do that, but remove standard errors
  # these are no longer correct
  if (!is.null(trans_fun)) {
    estimated$estimate <- trans_fun(estimated$estimate)
    estimated$conf.low <- trans_fun(estimated$conf.low)
    estimated$conf.high <- trans_fun(estimated$conf.high)
    estimated$std.error <- NULL
  }


  # Last step: Save information in attributes  --------------------------------
  # ---------------------------------------------------------------------------

  estimated <- .add_attributes(
    estimated,
    model_info = model_info,
    info = c(
      datagrid_info,
      list(
        trend = trend,
        datagrid = datagrid,
        coef_name = "Slope",
        p_adjust = p_adjust,
        ci = ci,
        transform = !is.null(transform),
        add_iterations = add_iterations
      )
    )
  )
  class(estimated) <- unique(c("marginaleffects_slopes", class(estimated)))

  # adjust p-values
  if (!model_info$is_bayesian) {
    estimated <- .p_adjust(model, estimated, p_adjust, verbose, ...)
  }

  estimated
}


# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================

#' @keywords internal
.guess_marginaltrends_arguments <- function(model,
                                            trend = NULL,
                                            verbose = TRUE,
                                            ...) {
  # Gather info
  model_data <- insight::get_data(model, verbose = FALSE)
  predictors <- intersect(
    colnames(model_data),
    insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)
  )

  # Guess arguments
  if (is.null(trend)) {
    trend <- predictors[sapply(model_data[predictors], is.numeric)][1]
    if (!length(trend) || is.na(trend)) {
      insight::format_error("Model contains no numeric predictor. Please specify `trend`.")
    }
    if (verbose) {
      insight::format_alert(paste0("No numeric variable was specified for slope estimation. Selecting `trend = \"", trend, "\"`.")) # nolint
    }
  }
  if (length(trend) > 1) {
    if (verbose) {
      insight::format_alert(paste0(
        "More than one numeric variable was selected for slope estimation. Keeping only `", trend[1], "`. ", # nolint
        "If you want to estimate the slope of `", trend[1], "` at different values of `", trend[2], "`, use `by=\"", trend[2], "\"` instead." # nolint
      ))
    }
    trend <- trend[1]
  }

  trend
}


#' @keywords internal
.marginaleffects_slopes <- function() {
  c("dY/dX", "eY/eX", "eY/dX", "dY/eX")
}
