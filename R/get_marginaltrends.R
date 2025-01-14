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
                               verbose = TRUE,
                               ...) {
  # check if available
  insight::check_if_installed("marginaleffects")
  dots <- list(...)

  # Guess arguments
  trend <- .guess_marginaltrends_arguments(model, trend, by, verbose, ...)


  # First step: create a data grid --------------------------------------------
  # ---------------------------------------------------------------------------

  # data grid only when we have by predictors
  if (is.null(by)) {
    datagrid <- at_specs <- NULL
  } else {
    # setup arguments
    dg_args <- list(
      model,
      by = by,
      verbose = FALSE
    )
    # add user-arguments from "...", but remove those arguments that are already set
    dots[c("by", "verbose")] <- NULL
    dg_args <- insight::compact_list(c(dg_args, dots))

    # Get corresponding datagrid (and deal with particular ats)
    datagrid <- do.call(insight::get_datagrid, dg_args)
    at_specs <- attributes(datagrid)$at_specs
  }


  # Second step: prepare arguments for marginaleffects ------------------------
  # ---------------------------------------------------------------------------

  # setup arguments again
  fun_args <- insight::compact_list(c(
    list(
      model,
      variables = trend,
      by = at_specs$varname,
      newdata = datagrid
    ),
    dots
  ))


  # Third step: compute marginal slopes ---------------------------------------
  # ---------------------------------------------------------------------------

  # Compute stuff
  estimated <- suppressWarnings(do.call(marginaleffects::avg_slopes, fun_args))


  # Last step: Save information in attributes  --------------------------------
  # ---------------------------------------------------------------------------

  attr(estimated, "at") <- at_specs$varname
  attr(estimated, "by") <- at_specs$varname
  attr(estimated, "focal_terms") <- at_specs$varname
  attr(estimated, "trend") <- trend
  attr(estimated, "datagrid") <- datagrid
  attr(estimated, "preserve_range") <- attributes(datagrid)$preserve_range
  attr(estimated, "coef_name") <- "Slope"
  attr(estimated, "marginalize") <- marginalize

  class(estimated) <- unique(c("marginaleffects_slopes", class(estimated)))

  estimated
}


# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================

#' @keywords internal
.guess_marginaltrends_arguments <- function(model,
                                            trend = NULL,
                                            by = NULL,
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
    trend <- trend[1]
    if (verbose) {
      insight::format_alert(paste0("More than one numeric variable was selected for slope estimation. Keeping only ", trend[1], ".")) # nolint
    }
  }

  trend
}
