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
#' @param by The predictor variable(s) at which to evaluate the desired effect
#'   / mean / contrasts. Other predictors of the model that are not included
#'   here will be collapsed and "averaged" over (the effect will be estimated
#'   across them).
#' @param ... Other arguments passed for instance to [insight::get_datagrid()].
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)
#'
#' if (require("emmeans", quietly = TRUE)) {
#'   # By default, 'by' is set to "Species"
#'   get_emmeans(model)
#'
#'   # Overall mean (close to 'mean(iris$Sepal.Length)')
#'   get_emmeans(model, by = NULL)
#'
#'   # One can estimate marginal means at several values of a 'modulate' variable
#'   get_emmeans(model, by = "Petal.Width", length = 3)
#'
#'   # Interactions
#'   model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#'   get_emmeans(model)
#'   get_emmeans(model, by = c("Species", "Petal.Length"), length = 2)
#'   get_emmeans(model, by = c("Species", "Petal.Length = c(1, 3, 5)"), length = 2)
#' }
#' @export
get_emmeans <- function(model,
                        by = "auto",
                        fixed = NULL,
                        transform = "response",
                        ...) {
  # check if available
  insight::check_if_installed("emmeans")

  # Guess arguments
  my_args <- .guess_emmeans_arguments(model, by, fixed, ...)


  # Run emmeans
  estimated <- emmeans::emmeans(
    model,
    specs = my_args$emmeans_specs,
    at = my_args$emmeans_at,
    type = transform,
    ...
  )

  # Special behaviour for transformations #138 (see below)
  if ("retransform" %in% names(my_args) && length(my_args$retransform) > 0) {
    for (var in names(my_args$retransform)) {
      estimated@levels[[var]] <- levels(my_args$retransform[[var]])
      estimated@grid[[var]] <- my_args$retransform[[var]]
    }
  }

  attr(estimated, "at") <- my_args$by
  attr(estimated, "by") <- my_args$by
  attr(estimated, "fixed") <- my_args$fixed
  estimated
}

#' @rdname get_emmeans
#' @export
model_emmeans <- get_emmeans



# =========================================================================
# HELPERS  ----------------------------------------------------------------
# =========================================================================
# This function is the actual equivalent of .get_marginalmeans(); both being used
# in estimate_means

#' @keywords internal
.format_emmeans_means <- function(estimated, model, ci = 0.95, transform = "response", ...) {
  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    means <- parameters::parameters(estimated, ci = ci, ...)
    means <- .clean_names_bayesian(means, model, transform, type = "mean")
    em_grid <- as.data.frame(estimated@grid)
    em_grid[[".wgt."]] <- NULL # Drop the weight column
    colums_to_add <- setdiff(colnames(em_grid), colnames(means))
    if (length(colums_to_add)) {
      means <- cbind(em_grid[colums_to_add], means)
    }
  } else {
    means <- as.data.frame(stats::confint(estimated, level = ci))
    means$df <- NULL
    means <- .clean_names_frequentist(means)
  }
  # Remove the "1 - overall" column that can appear in cases like at = NULL
  means <- means[names(means) != "1"]

  # Restore factor levels
  means <- datawizard::data_restoretype(means, insight::get_data(model, verbose = FALSE))


  info <- attributes(estimated)

  attr(means, "at") <- info$by
  attr(means, "by") <- info$by
  attr(means, "fixed") <- info$fixed
  means
}



# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================

#' @keywords internal
.guess_emmeans_arguments <- function(model,
                                     by = NULL,
                                     fixed = NULL,
                                     ...) {
  # Gather info
  predictors <- insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)
  my_data <- insight::get_data(model, verbose = FALSE)

  # Guess arguments
  if (!is.null(by) && length(by) == 1 && by == "auto") {
    by <- predictors[!sapply(my_data[predictors], is.numeric)]
    if (!length(by) || all(is.na(by))) {
      stop("Model contains no categorical factor. Please specify 'by'.", call. = FALSE)
    }
    insight::format_alert(paste0("We selected `by = c(", toString(paste0('"', by, '"')), ")`."))
  }

  my_args <- list(by = by, fixed = fixed)
  .format_emmeans_arguments(model, args = my_args, data = my_data, ...)
}


#' @keywords internal
.format_emmeans_arguments <- function(model, args, data, ...) {
  # Create the data_matrix
  # ---------------------------
  # data <- insight::get_data(model)
  data <- data[insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)]

  # Deal with 'at'
  if (is.null(args$by)) {
    args$data_matrix <- NULL
  } else if (is.data.frame(args$by)) {
    args$data_matrix <- args$by
    args$by <- names(args$by)
  } else if (is.list(args$by)) {
    args$data_matrix <- expand.grid(args$by)
    args$by <- names(args$data_matrix)
  } else if (inherits(args$by, "formula")) {
    args$data_matrix <- stats::model.frame(args$by, data = data)
    args$by <- names(args$data_matrix)
  } else {
    if (!is.null(args$by) && all(args$by == "all")) {
      target <- insight::find_predictors(model, effects = "fixed", flatten = TRUE)
      target <- target[!target %in% args$fixed]
    } else {
      target <- args$by
    }
    datagrid <- insight::get_datagrid(data, by = target, ...)
    args$by <- attributes(datagrid)$at_specs$varname
    args$data_matrix <- as.data.frame(datagrid[args$by])
    if (length(args$by) == 0) args$by <- NULL # Post-clean
  }

  # Deal with 'contrast'
  if (!is.null(args$contrast)) {
    contrast <- insight::get_datagrid(data, by = args$contrast, ...)
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
    fixed <- insight::get_datagrid(data[args$fixed], by = NULL, ...)
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
    model_terms <- insight::find_terms(model)$conditional
    for (var_at in names(args$emmeans_at)) {
      term <- model_terms[grepl(var_at, model_terms, fixed = TRUE)]
      if (any(grepl(paste0("as.factor(", var_at, ")"), term, fixed = TRUE)) ||
        any(grepl(paste0("as.character(", var_at, ")"), term, fixed = TRUE))) {
        args$retransform[[var_at]] <- args$emmeans_at[[var_at]]
        args$emmeans_at[[var_at]] <- as.numeric(as.character(args$emmeans_at[[var_at]]))
      }
    }
  }

  args
}
