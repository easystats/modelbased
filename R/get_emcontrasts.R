#' @rdname get_emmeans
#'
#' @param contrast A character vector indicating the name of the variable(s)
#' for which to compute the contrasts.
#' @param method Contrast method. See same argument in [emmeans::contrast].
#'
#' @examplesIf insight::check_if_installed("emmeans", quietly = TRUE)
#' # Basic usage
#' model <- lm(Sepal.Width ~ Species, data = iris)
#' get_emcontrasts(model)
#'
#' # Dealing with interactions
#' model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
#' # By default: selects first factor
#' get_emcontrasts(model)
#' # Can also run contrasts between points of numeric
#' get_emcontrasts(model, contrast = "Petal.Width", length = 3)
#' # Or both
#' get_emcontrasts(model, contrast = c("Species", "Petal.Width"), length = 2)
#' # Or with custom specifications
#' estimate_contrasts(model, contrast = c("Species", "Petal.Width=c(1, 2)"))
#' # Or modulate it
#' get_emcontrasts(model, by = "Petal.Width", length = 4)
#' @export
get_emcontrasts <- function(model,
                            contrast = NULL,
                            by = NULL,
                            predict = "none",
                            method = "pairwise",
                            transform,
                            ...) {
  # check if available
  insight::check_if_installed("emmeans")

  ## TODO: remove deprecation warning later
  if (!missing(transform)) {
    insight::format_warning("Argument `transform` is deprecated. Please use `predict` instead.")
    predict <- transform
  }

  # Guess arguments
  my_args <- .guess_emcontrasts_arguments(model, contrast, by, ...)

  # find default response-type
  predict <- .get_emmeans_type_argument(model, predict, ...)

  # Run emmeans
  estimated <- emmeans::emmeans(
    model,
    specs = my_args$emmeans_specs,
    at = my_args$emmeans_at,
    type = predict,
    ...
  )

  # Find by variables
  emm_by <- my_args$emmeans_specs[!my_args$emmeans_specs %in% my_args$contrast]
  if (length(emm_by) == 0) emm_by <- NULL

  out <- emmeans::contrast(estimated, by = emm_by, method = method, ...)

  attr(out, "contrast") <- my_args$contrast
  attr(out, "at") <- my_args$by
  attr(out, "by") <- my_args$by
  out
}

#' @rdname get_emmeans
#' @export
model_emcontrasts <- get_emcontrasts


# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================

#' @keywords internal
.guess_emcontrasts_arguments <- function(model,
                                         contrast = NULL,
                                         by = NULL,
                                         ...) {
  # Gather info
  model_data <- insight::get_data(model, verbose = FALSE)
  predictors <- intersect(
    colnames(model_data),
    insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)
  )

  # Guess arguments
  if (is.null(contrast)) {
    contrast <- predictors[!sapply(model_data[predictors], is.numeric)][1]
    if (!length(contrast) || is.na(contrast)) {
      contrast <- predictors[1]
    }
    insight::format_alert(paste0("No variable was specified for contrast estimation. Selecting `contrast = \"", contrast, "\"`.")) # nolint
  } else if (all(contrast == "all")) {
    contrast <- predictors
  }

  my_args <- list(contrast = contrast, by = by)
  .format_emmeans_arguments(model, args = my_args, data = model_data, ...)
}
