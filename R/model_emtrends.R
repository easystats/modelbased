#' @rdname model_emmeans
#'
#' @param trend A character vector indicating the name of the numeric variable
#'   for which to compute the slopes.
#' @param at The predictor variable(s) \emph{at} which to evaluate the desired effect / mean / contrasts. Other predictors of the model that are not included here will be collapsed and "averaged" over (the effect will be estimated across them).
#'
#' @examples
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#' model_emtrends(model)
#' model_emtrends(model, at = "Species")
#' model_emtrends(model, at = "Petal.Length")
#' model_emtrends(model, at = c("Species", "Petal.Length"))
#'
#' model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)
#' model_emtrends(model)
#' model_emtrends(model, at = "Sepal.Width")
#' @export
model_emtrends <- function(model,
                           trend = NULL,
                           at = NULL,
                           levels = NULL,
                           modulate = NULL,
                           ...) {

  # Deprecation
  if(!is.null(levels) | !is.null(modulate)) {
    warning("The `levels` and `modulate` arguments are deprecated. Please use `at` instead.")
    at <- c(at, levels, modulate)
  }

  # check if available
  insight::check_if_installed("emmeans")

  # Guess arguments
  args <- .guess_emtrends_arguments(model, trend, at, ...)

  # Run emtrends
  estimated <- emmeans::emtrends(
    model,
    specs = args$emmeans_specs,
    var = args$trend,
    at = args$emmeans_at,
    # cov.reduce = cov.reduce,
    ...
  )

  attr(estimated, "trend") <- args$trend
  attr(estimated, "at") <- args$at
  estimated
}



# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================

#' @keywords internal
.format_emmeans_arguments <- function(model, args, ...) {
  if(is.null(args$at)) {
    args$emmeans_specs <- ~1
    args$emmeans_at <- NULL
  } else {
    args$emmeans_at <- list()
    data <- insight::get_data(model)

    for (i in args$at) {
      vizdata <- visualisation_matrix(data, target = i, ...)
      var <- attributes(vizdata)$target_specs$varname[1] # Retrieve cleaned varname
      args$emmeans_at[[var]] <- vizdata[[var]]

      # Overwrite the corresponding names with clean names
      args$at[args$at == i] <- var
      args$emmeans_specs <- args$at
    }
  }
  args
}



#' @keywords internal
.guess_emtrends_arguments <- function(model,
                                      trend = NULL,
                                      at = NULL,
                                      ...) {

  # Gather info
  predictors <- insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)
  data <- insight::get_data(model)

  # Guess arguments
  if (is.null(trend)) {
    trend <- predictors[sapply(data[predictors], is.numeric)][1]
    if (!length(trend) || is.na(trend)) {
      stop("Model contains no numeric predictor. Cannot estimate trend.")
    }
    message('No numeric variable was specified for slope estimation. Selecting `trend = "', trend, '"`.')
  }
  if (length(trend) > 1) {
    message("More than one numeric variable was selected for slope estimation. Keeping only ", trend[1], ".")
    trend <- trend[1]
  }

  args <- list(trend = trend, at = at)
  .format_emmeans_arguments(model, args, ...)
}
