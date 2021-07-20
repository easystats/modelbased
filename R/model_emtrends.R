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
                           fixed = NULL,
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
  args <- .guess_emtrends_arguments(model, trend, at, fixed, ...)

  # Run emtrends
  estimated <- emmeans::emtrends(
    model,
    specs = args$emmeans_specs,
    var = args$trend,
    at = args$emmeans_at,
    ...
  )

  attr(estimated, "trend") <- args$trend
  attr(estimated, "at") <- args$at
  attr(estimated, "fixed") <- args$fixed
  estimated
}



# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================

#' @keywords internal
.format_emmeans_arguments <- function(model, args, ...) {

  # Create the data_matrix
  # ---------------------------
  data <- insight::get_data(model)

  # Deal with 'at'
  if(is.null(args$at)) {
    args$data_matrix <- NULL
  } else {
    grid <- visualisation_matrix(data, target = args$at, ...)
    vars <- attributes(grid)$target_specs$varname
    args$data_matrix <- as.data.frame(grid[vars])
    args$at <- vars # Replace by cleaned varnames
  }
  # Deal with 'fixed'
  if(!is.null(args$fixed)) {
    fixed <- visualisation_matrix(data[args$fixed], target = NULL, ...)
    if(is.null(args$data_matrix)) {
      args$data_matrix <- fixed
    } else {
      args$data_matrix <- merge(args$data_matrix, fixed)
    }
  }

  # Get 'specs' and 'at'
  # --------------------
  if(is.null(args$data_matrix)) {
    args$emmeans_specs <- ~1
    args$emmeans_at <- NULL
  } else {
    args$emmeans_specs <- names(args$data_matrix)
    args$emmeans_at <- sapply(as.list(args$data_matrix), unique, simplify = FALSE)
  }

  args
}



#' @keywords internal
.guess_emtrends_arguments <- function(model,
                                      trend = NULL,
                                      at = NULL,
                                      fixed = NULL,
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

  args <- list(trend = trend, at = at, fixed = fixed)
  .format_emmeans_arguments(model, args, ...)
}
