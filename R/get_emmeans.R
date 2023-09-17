#' Easy 'emmeans' and 'emtrends'
#'
#' The `get_emmeans()` function is a wrapper to facilitate the usage of
#' `emmeans::emmeans()` and `emmeans::emtrends()`, providing a somewhat simpler
#' and intuitive API to find the specifications and variables of interest. It is
#' meanly made to for the developers to facilitate the organization and
#' debugging, and end-users should rather use the `estimate_*()` series of
#' functions.
#'
#' @param model A statistical model.
#' @param fixed A character vector indicating the names of the predictors to be
#'   "fixed" (i.e., maintained), so that the estimation is made at these values.
#' @param transform Is passed to the `type` argument in
#'   `emmeans::emmeans()`. See
#'   [this vignette](https://CRAN.R-project.org/package=emmeans/vignettes/transformations.html).
#'   Can be `"none"` (default for contrasts), `"response"`
#'   (default for means), `"mu"`, `"unlink"`, `"log"`.
#'   `"none"` will leave the values on scale of the linear predictors.
#'   `"response"` will transform them on scale of the response variable.
#'   Thus for a logistic model, `"none"` will give estimations expressed in
#'   log-odds (probabilities on logit scale) and `"response"` in terms of
#'   probabilities.
#' @param levels,modulate Deprecated, use `at` instead.
#' @param at The predictor variable(s) *at* which to evaluate the desired effect
#'   / mean / contrasts. Other predictors of the model that are not included
#'   here will be collapsed and "averaged" over (the effect will be estimated
#'   across them).
#' @param ... Other arguments passed for instance to [insight::get_datagrid()].
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)
#'
#' if (require("emmeans", quietly = TRUE)) {
#'   # By default, 'at' is set to "Species"
#'   get_emmeans(model)
#'
#'   # Overall mean (close to 'mean(iris$Sepal.Length)')
#'   get_emmeans(model, at = NULL)
#'
#'   # One can estimate marginal means at several values of a 'modulate' variable
#'   get_emmeans(model, at = "Petal.Width", length = 3)
#'
#'   # Interactions
#'   model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#'   get_emmeans(model)
#'   get_emmeans(model, at = c("Species", "Petal.Length"), length = 2)
#'   get_emmeans(model, at = c("Species", "Petal.Length = c(1, 3, 5)"), length = 2)
#' }
#' @export
get_emmeans <- function(model,
                        at = "auto",
                        fixed = NULL,
                        transform = "response",
                        levels = NULL,
                        modulate = NULL,
                        ...) {
  # Deprecation
  if (!is.null(levels) || !is.null(modulate)) {
    insight::format_warning("The `levels` and `modulate` arguments are deprecated. Please use `at` instead.")
    at <- c(levels, modulate)
  }

  # check if available
  insight::check_if_installed("emmeans")

  # Guess arguments
  args <- .guess_emmeans_arguments(model, at, fixed, ...)


  # Run emmeans
  estimated <- emmeans::emmeans(
    model,
    specs = args$emmeans_specs,
    at = args$emmeans_at,
    type = transform,
    ...
  )

  # Special behaviour for transformations #138 (see below)
  if ("retransform" %in% names(args) && length(args$retransform) > 0) {
    for (var in names(args$retransform)) {
      estimated@levels[[var]] <- levels(args$retransform[[var]])
      estimated@grid[[var]] <- args$retransform[[var]]
    }
  }

  attr(estimated, "at") <- args$at
  attr(estimated, "fixed") <- args$fixed
  estimated
}

#' @rdname get_emmeans
#' @export
model_emmeans <- get_emmeans


# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================

#' @keywords internal
.format_emmeans_arguments <- function(model, args, data, ...) {
  # Create the data_matrix
  # ---------------------------
  # data <- insight::get_data(model)
  data <- data[insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)]

  # Deal with 'at'
  if (is.null(args$at)) {
    args$data_matrix <- NULL
  } else {
    if (is.data.frame(args$at)) {
      args$data_matrix <- args$at
      args$at <- names(args$at)
    } else if (is.list(args$at)) {
      args$data_matrix <- expand.grid(args$at)
      args$at <- names(args$data_matrix)
    } else if (inherits(args$at, "formula")) {
      args$data_matrix <- stats::model.frame(args$at, data = data)
      args$at <- names(args$data_matrix)
    } else {
      if (!is.null(args$at) && all(args$at == "all")) {
        target <- insight::find_predictors(model, effects = "fixed", flatten = TRUE)
        target <- target[!target %in% args$fixed]
      } else {
        target <- args$at
      }
      grid <- insight::get_datagrid(data, at = target, ...)
      args$at <- attributes(grid)$at_specs$varname
      args$data_matrix <- as.data.frame(grid[args$at])
      if (length(args$at) == 0) args$at <- NULL # Post-clean
    }
  }

  # Deal with 'contrast'
  if (!is.null(args$contrast)) {
    contrast <- insight::get_datagrid(data, at = args$contrast, ...)
    args$contrast <- attributes(contrast)$at_specs$varname
    contrast <- as.data.frame(contrast[args$contrast])
    if (is.null(args$data_matrix)) {
      args$data_matrix <- contrast
    } else {
      contrast <- contrast[!names(contrast) %in% names(args$data_matrix)]
      if (ncol(contrast) > 0) args$data_matrix <- merge(args$data_matrix, contrast)
    }
  }

  # Deal with 'fixed'
  if (!is.null(args$fixed)) {
    fixed <- insight::get_datagrid(data[args$fixed], at = NULL, ...)
    if (is.null(args$data_matrix)) {
      args$data_matrix <- fixed
    } else {
      args$data_matrix <- merge(args$data_matrix, fixed)
    }
  }

  # Get 'specs' and 'at'
  # --------------------
  if (is.null(args$data_matrix)) {
    args$emmeans_specs <- ~1
    args$emmeans_at <- NULL
  } else {
    args$emmeans_specs <- names(args$data_matrix)
    args$emmeans_at <- sapply(as.list(args$data_matrix), unique, simplify = FALSE)
  }

  # Special behaviour for transformations #138
  # It's annoying and an ugly fix, not sure how to address
  if (!is.null(args$emmeans_at)) {
    args$retransform <- list()
    terms <- insight::find_terms(model)$conditional
    for (var_at in names(args$emmeans_at)) {
      term <- terms[grepl(var_at, terms, fixed = TRUE)]
      if (any(grepl(paste0("as.factor(", var_at, ")"), term, fixed = TRUE)) ||
        any(grepl(paste0("as.character(", var_at, ")"), term, fixed = TRUE))) {
        args$retransform[[var_at]] <- args$emmeans_at[[var_at]]
        args$emmeans_at[[var_at]] <- as.numeric(as.character(args$emmeans_at[[var_at]]))
      }
    }
  }

  args
}




#' @keywords internal
.guess_emmeans_arguments <- function(model,
                                     at = NULL,
                                     fixed = NULL,
                                     ...) {
  # Gather info
  predictors <- insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)
  data <- insight::get_data(model)

  # Guess arguments
  if (!is.null(at) && length(at) == 1 && at == "auto") {
    at <- predictors[!sapply(data[predictors], is.numeric)]
    if (!length(at) || all(is.na(at))) {
      stop("Model contains no categorical factor. Please specify 'at'.", call. = FALSE)
    }
    message("We selected `at = c(", toString(paste0('"', at, '"')), ")`.")
  }

  args <- list(at = at, fixed = fixed)
  .format_emmeans_arguments(model, args, data, ...)
}
