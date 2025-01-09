#' Consistent API for 'emmeans' and 'marginaleffects'
#'
#' @description
#' These functions are convenient wrappers around the `emmeans` and the
#' `marginaleffects` packages. They are mostly available for developers who want
#' to leverage a unified API for getting model-based estimates, and regular users
#' should use the `estimate_*` set of functions.
#'
#' The `get_emmeans()`, `get_emcontrasts()` and `get_emtrends()` functions are
#' wrappers around `emmeans::emmeans()` and `emmeans::emtrends()`.
#'
#' @inheritParams estimate_means
#' @inheritParams estimate_slopes
#' @inheritParams estimate_contrasts
#'
#' @examplesIf require("emmeans", quietly = TRUE)
#' model <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)
#'
#' # By default, 'by' is set to "Species"
#' get_emmeans(model)
#'
#' # Overall mean (close to 'mean(iris$Sepal.Length)')
#' get_emmeans(model, by = NULL)
#'
#' # One can estimate marginal means at several values of a 'modulate' variable
#' get_emmeans(model, by = "Petal.Width", length = 3)
#'
#' # Interactions
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#' get_emmeans(model)
#' get_emmeans(model, by = c("Species", "Petal.Length"), length = 2)
#' get_emmeans(model, by = c("Species", "Petal.Length = c(1, 3, 5)"), length = 2)
#' @export
get_emmeans <- function(model,
                        by = "auto",
                        predict = NULL,
                        transform = NULL,
                        ...) {
  # check if available
  insight::check_if_installed("emmeans")

  ## TODO: remove deprecation warning later
  if (!is.null(transform)) {
    insight::format_warning("Argument `transform` is deprecated. Please use `predict` instead.")
    predict <- transform
  }

  # Guess arguments
  my_args <- .guess_emmeans_arguments(model, by, ...)

  # find default response-type
  predict <- .get_emmeans_type_argument(model, predict, type = "means", ...)

  # setup arguments
  fun_args <- list(
    model,
    specs = my_args$emmeans_specs,
    at = my_args$emmeans_at
  )

  # handle distributional parameters
  if (predict %in% .brms_aux_elements() && inherits(model, "brmsfit")) {
    fun_args$dpar <- predict
  } else {
    fun_args$type <- predict
  }

  # add dots
  dots <- list(...)
  fun_args <- insight::compact_list(c(fun_args, dots))

  # Run emmeans
  estimated <- suppressWarnings(do.call(emmeans::emmeans, fun_args))

  # Special behaviour for transformations #138 (see below)
  if ("retransform" %in% names(my_args) && length(my_args$retransform) > 0) {
    for (var in names(my_args$retransform)) {
      estimated@levels[[var]] <- levels(my_args$retransform[[var]])
      estimated@grid[[var]] <- my_args$retransform[[var]]
    }
  }

  attr(estimated, "at") <- my_args$by
  attr(estimated, "by") <- my_args$by
  attr(estimated, "predict") <- predict
  estimated
}


# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================

#' @keywords internal
.guess_emmeans_arguments <- function(model,
                                     by = NULL,
                                     ...) {
  # Gather info
  model_data <- insight::get_data(model, verbose = FALSE)
  predictors <- intersect(
    colnames(model_data),
    insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)
  )

  # Guess arguments
  if (!is.null(by) && length(by) == 1 && by == "auto") {
    by <- predictors[!sapply(model_data[predictors], is.numeric)]
    if (!length(by) || all(is.na(by))) {
      stop("Model contains no categorical factor. Please specify 'by'.", call. = FALSE)
    }
    insight::format_alert(paste0("We selected `by = c(", toString(paste0('"', by, '"')), ")`."))
  }

  my_args <- list(by = by)
  .process_emmeans_arguments(model, args = my_args, data = model_data, ...)
}


## TODO: validate predict argument to make sure it only has valid options
.get_emmeans_type_argument <- function(model, predict, type = "means", ...) {
  if (is.null(predict)) {
    predict <- switch(type,
      means = "response",
      contrasts = "response",
      "none"
    )
  } else if (predict == "link") {
    predict <- "none"
  }
  predict
}
