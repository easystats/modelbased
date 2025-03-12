#' @rdname get_emmeans
#' @examplesIf insight::check_if_installed("emmeans", quietly = TRUE)
#' \dontrun{
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#' get_emtrends(model)
#' get_emtrends(model, by = "Species")
#' get_emtrends(model, by = "Petal.Length")
#' get_emtrends(model, by = c("Species", "Petal.Length"))
#' }
#'
#' model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)
#' get_emtrends(model)
#' get_emtrends(model, by = "Sepal.Width")
#' @export
get_emtrends <- function(model,
                         trend = NULL,
                         by = NULL,
                         keep_iterations = FALSE,
                         verbose = TRUE,
                         ...) {
  # check if available
  insight::check_if_installed("emmeans")

  # Guess arguments
  my_args <- .guess_emtrends_arguments(model, trend, by, verbose, ...)

  # Run emtrends
  estimated <- emmeans::emtrends(
    model,
    specs = my_args$emmeans_specs,
    var = my_args$trend,
    at = my_args$emmeans_at,
    ...
  )

  # for Bayesian model, keep iterations
  if (insight::model_info(model)$is_bayesian) {
    attr(estimated, "posterior_draws") <- insight::get_parameters(estimated)
  } else {
    keep_iterations <- FALSE
  }

  attr(estimated, "trend") <- my_args$trend
  attr(estimated, "at") <- my_args$by
  attr(estimated, "by") <- my_args$by
  attr(estimated, "coef_name") <- "Slope"
  attr(estimated, "transform") <- TRUE
  attr(estimated, "keep_iterations") <- keep_iterations

  estimated
}


# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================

#' @keywords internal
.guess_emtrends_arguments <- function(model,
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

  my_args <- list(trend = trend, by = by)
  .process_emmeans_arguments(model, args = my_args, data = model_data, ...)
}


# Formatting ===============================================================

.format_emmeans_slopes <- function(model, estimated, ci, ...) {
  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    trends <- parameters::parameters(estimated, ci = ci, ...)
    trends <- .clean_names_bayesian(trends, model, predict = "none", type = "trend")
    em_grid <- as.data.frame(estimated@grid)
    em_grid[[".wgt."]] <- NULL # Drop the weight column
    colums_to_add <- setdiff(colnames(em_grid), colnames(trends))
    if (length(colums_to_add)) {
      trends <- cbind(em_grid[colums_to_add], trends)
    }
  } else {
    trends <- parameters::parameters(estimated, ci = ci, ...)
  }
  # Remove the "1 - overall" column that can appear in cases like y ~ x
  trends <- trends[names(trends) != "1"]

  # rename
  trends <- datawizard::data_rename(trends, select = c(Slope = "Coefficient"))

  # Restore factor levels
  out <- datawizard::data_restoretype(trends, insight::get_data(model, verbose = FALSE))

  # add posterior draws?
  .add_posterior_draws_emmeans(attributes(estimated), out)
}
