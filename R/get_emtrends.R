#' @rdname get_emmeans
#'
#' @param trend A character indicating the name of the variable
#'   for which to compute the slopes.
#'
#' @examples
#' if (require("emmeans")) {
#'   model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#'   get_emtrends(model)
#'   get_emtrends(model, at = "Species")
#'   get_emtrends(model, at = "Petal.Length")
#'   get_emtrends(model, at = c("Species", "Petal.Length"))
#'
#'   model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)
#'   get_emtrends(model)
#'   get_emtrends(model, at = "Sepal.Width")
#' }
#' @export
get_emtrends <- function(model,
                         trend = NULL,
                         at = NULL,
                         fixed = NULL,
                         levels = NULL,
                         modulate = NULL,
                         ...) {
  # Deprecation
  if (!is.null(levels) || !is.null(modulate)) {
    warning("The `levels` and `modulate` arguments are deprecated. Please use `at` instead.", call. = FALSE)
    at <- c(levels, modulate)
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

#' @rdname get_emmeans
#' @export
model_emtrends <- get_emtrends

# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================

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
      stop("Model contains no numeric predictor. Please specify 'trend'.", call. = FALSE)
    }
    message('No numeric variable was specified for slope estimation. Selecting `trend = "', trend, '"`.')
  }
  if (length(trend) > 1) {
    message("More than one numeric variable was selected for slope estimation. Keeping only ", trend[1], ".")
    trend <- trend[1]
  }

  args <- list(trend = trend, at = at, fixed = fixed)
  .format_emmeans_arguments(model, args, data, ...)
}
