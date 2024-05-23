#' @rdname get_emmeans
#'
#' @param contrast A character vector indicating the name of the variable(s)
#'   for which to compute the contrasts.
#' @param method Contrast method. See same argument in [emmeans::contrast].
#'
#' @examples
#' if (require("emmeans", quietly = TRUE)) {
#'   # Basic usage
#'   model <- lm(Sepal.Width ~ Species, data = iris)
#'   get_emcontrasts(model)
#'
#'   # Dealing with interactions
#'   model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
#'   # By default: selects first factor
#'   get_emcontrasts(model)
#'   # Can also run contrasts between points of numeric
#'   get_emcontrasts(model, contrast = "Petal.Width", length = 3)
#'   # Or both
#'   get_emcontrasts(model, contrast = c("Species", "Petal.Width"), length = 2)
#'   # Or with custom specifications
#'   estimate_contrasts(model, contrast = c("Species", "Petal.Width=c(1, 2)"))
#'   # Can fixate the numeric at a specific value
#'   get_emcontrasts(model, fixed = "Petal.Width")
#'   # Or modulate it
#'   get_emcontrasts(model, by = "Petal.Width", length = 4)
#' }
#' @export
get_emcontrasts <- function(model,
                            contrast = NULL,
                            by = NULL,
                            fixed = NULL,
                            transform = "none",
                            method = "pairwise",
                            at = NULL,
                            ...) {
  # check if available
  insight::check_if_installed("emmeans")

  if (!is.null(at)) {
    insight::format_warning("The `at` argument is deprecated and will be removed in the future. Please use `by` instead.") # nolint
    by <- at
  }

  # Guess arguments
  my_args <- .guess_emcontrasts_arguments(model, contrast, by, fixed, ...)

  # Run emmeans
  estimated <- emmeans::emmeans(
    model,
    specs = my_args$emmeans_specs,
    at = my_args$emmeans_at,
    type = transform,
    ...
  )

  # Find by variables
  emm_by <- my_args$emmeans_specs[!my_args$emmeans_specs %in% my_args$contrast]
  if (length(emm_by) == 0) emm_by <- NULL

  out <- emmeans::contrast(estimated, by = emm_by, method = method, ...)

  attr(out, "contrast") <- my_args$contrast
  attr(out, "at") <- my_args$by
  attr(out, "by") <- my_args$by
  attr(out, "fixed") <- my_args$fixed
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
                                         fixed = NULL,
                                         ...) {
  # Gather info
  predictors <- insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)
  model_data <- insight::get_data(model)

  # Guess arguments
  if (is.null(contrast)) {
    contrast <- predictors[!sapply(model_data[predictors], is.numeric)][1]
    if (!length(contrast) || is.na(contrast)) {
      contrast <- predictors[1]
    }
    insight::format_alert('No variable was specified for contrast estimation. Selecting `contrast = "', contrast, '"`.') # nolint
  } else if (all(contrast == "all")) {
    contrast <- predictors
  }

  my_args <- list(contrast = contrast, by = by, fixed = fixed)
  .format_emmeans_arguments(model, args = my_args, data = model_data, ...)
}
