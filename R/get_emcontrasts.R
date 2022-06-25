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
#'   get_emcontrasts(model, at = "Petal.Width", length = 4)
#' }
#' @export
get_emcontrasts <- function(model,
                            contrast = NULL,
                            at = NULL,
                            fixed = NULL,
                            transform = "none",
                            method = "pairwise",
                            ...) {
  # check if available
  insight::check_if_installed("emmeans")

  # Guess arguments
  args <- .guess_emcontrasts_arguments(model, contrast, at, fixed, ...)

  # Run emmeans
  estimated <- emmeans::emmeans(
    model,
    specs = args$emmeans_specs,
    at = args$emmeans_at,
    type = transform,
    ...
  )

  # Find by variables
  by <- args$emmeans_specs[!args$emmeans_specs %in% args$contrast]
  if (length(by) == 0) by <- NULL

  contrasts <- emmeans::contrast(estimated, by = by, method = method, ...)

  attr(contrasts, "contrast") <- args$contrast
  attr(contrasts, "at") <- args$at
  attr(contrasts, "fixed") <- args$fixed
  contrasts
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
                                         at = NULL,
                                         fixed = NULL,
                                         ...) {
  # Gather info
  predictors <- insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)
  data <- insight::get_data(model)

  # Guess arguments
  if (is.null(contrast)) {
    contrast <- predictors[!sapply(data[predictors], is.numeric)][1]
    if (!length(contrast) || is.na(contrast)) {
      contrast <- predictors[1]
    }
    message('No variable was specified for contrast estimation. Selecting `contrast = "', contrast, '"`.')
  } else {
    if (all(contrast == "all")) {
      contrast <- predictors
    }
  }

  args <- list(contrast = contrast, at = at, fixed = fixed)
  .format_emmeans_arguments(model, args, data, ...)
}
