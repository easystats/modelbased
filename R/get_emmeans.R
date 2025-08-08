#' Consistent API for 'emmeans' and 'marginaleffects'
#'
#' @description
#' These functions are convenient wrappers around the **emmeans** and the
#' **marginaleffects** packages. They are mostly available for developers who want
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
#' \dontrun{
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
#' }
#' @export
get_emmeans <- function(model,
                        by = "auto",
                        predict = NULL,
                        keep_iterations = FALSE,
                        verbose = TRUE,
                        ...) {
  # check if available
  insight::check_if_installed("emmeans")

  # Guess arguments
  my_args <- .guess_emmeans_arguments(model, by, verbose, ...)

  # find default response-type
  predict <- .get_emmeans_type_argument(model, predict, type = "means", ...)

  # setup arguments
  fun_args <- list(
    model,
    specs = my_args$emmeans_specs,
    at = my_args$emmeans_at
  )

  # handle distributional parameters
  if (predict %in% .brms_aux_elements(model) && inherits(model, "brmsfit")) {
    dpars <- TRUE
    fun_args$dpar <- predict
  } else {
    dpars <- FALSE
    fun_args$type <- predict
  }

  # add dots
  dots <- list(...)
  fun_args <- insight::compact_list(c(fun_args, dots))

  # Run emmeans
  estimated <- suppressMessages(suppressWarnings(do.call(emmeans::emmeans, fun_args)))

  # backtransform to response scale for dpars
  if (dpars) {
    estimated <- emmeans::regrid(estimated)
  }

  # Special behaviour for transformations #138 (see below)
  if ("retransform" %in% names(my_args) && length(my_args$retransform) > 0) {
    for (var in names(my_args$retransform)) {
      estimated@levels[[var]] <- levels(my_args$retransform[[var]])
      estimated@grid[[var]] <- my_args$retransform[[var]]
    }
  }

  # for Bayesian model, keep iterations
  if (insight::model_info(model, response = 1)$is_bayesian) {
    attr(estimated, "posterior_draws") <- insight::get_parameters(estimated)
  } else {
    keep_iterations <- FALSE
  }

  attr(estimated, "at") <- my_args$by
  attr(estimated, "by") <- my_args$by
  attr(estimated, "predict") <- predict
  attr(estimated, "focal_terms") <- my_args$emmeans_specs
  attr(estimated, "transform") <- TRUE
  attr(estimated, "keep_iterations") <- keep_iterations

  estimated
}


# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================

#' @keywords internal
.guess_emmeans_arguments <- function(model, by = NULL, verbose = TRUE, ...) {
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
      insight::format_error("Model contains no categorical factor. Please specify `by`.")
    }
    if (verbose) {
      insight::format_alert(paste0(
        "We selected `by = c(",
        toString(paste0('"', by, '"')),
        ")`."
      ))
    }
  }

  my_args <- list(by = by)
  .process_emmeans_arguments(model, args = my_args, data = model_data, ...)
}


## TODO: validate predict argument to make sure it only has valid options
.get_emmeans_type_argument <- function(model, predict, type = "means", ...) {
  if (is.null(predict)) {
    predict <- switch(type, means = "response", contrasts = "response", "none")
  } else if (predict == "link") {
    predict <- "none"
  }
  predict
}


# Table formatting emmeans ----------------------------------------------------

.format_emmeans_means <- function(x, model, ci = 0.95, verbose = TRUE, ...) {
  predict <- attributes(x)$predict
  m_info <- insight::model_info(model, response = 1)

  # Summarize and clean
  if (m_info$is_bayesian) {
    means <- parameters::parameters(x, ci = ci, ...)
    means <- .clean_names_bayesian(means, model, predict, type = "mean")
    em_grid <- as.data.frame(x@grid)
    em_grid[[".wgt."]] <- NULL # Drop the weight column
    colums_to_add <- setdiff(colnames(em_grid), colnames(means))
    if (length(colums_to_add)) {
      means <- cbind(em_grid[colums_to_add], means)
    }
  } else {
    means <- as.data.frame(stats::confint(x, level = ci))
    means$df <- NULL
    means <- .clean_names_frequentist(means, predict, m_info)
  }

  # Remove the "1 - overall" column that can appear in cases like at = NULL
  means <- means[names(means) != "1"]

  # Restore factor levels
  means <- datawizard::data_restoretype(means, insight::get_data(model, verbose = FALSE))

  info <- attributes(x)

  attr(means, "at") <- info$by
  attr(means, "by") <- info$by

  .add_posterior_draws_emmeans(info, means)
}


# adds posterior draws to output for emmeans objects
.add_posterior_draws_emmeans <- function(info, estimated) {
  # add posterior draws?
  if (!is.null(info$posterior_draws)) {
    # how many?
    keep_iterations <- info$keep_iterations
    # check if user wants to keep any posterior draws
    if (isTRUE(keep_iterations) || is.numeric(keep_iterations)) {
      # reshape draws
      posterior_draws <- datawizard::data_transpose(info$posterior_draws)
      # keep all iterations when `TRUE`
      if (isTRUE(keep_iterations)) {
        keep_iterations <- ncol(posterior_draws)
      }
      colnames(posterior_draws) <- paste0("iter_", seq_len(ncol(posterior_draws)))
      estimated <- cbind(estimated, posterior_draws[, 1:keep_iterations, drop = FALSE])
    }
  }
  # remove from attributes
  attr(estimated, "posterior_draws") <- NULL
  estimated
}


# Bring arguments in shape for emmeans ----------------------------------------


#' @keywords internal
.process_emmeans_arguments <- function(model, args, data, ...) {
  # Create the data_matrix
  # ---------------------------
  # data <- insight::get_data(model, verbose = FALSE)
  predictors <- insight::find_predictors(
    model,
    effects = "fixed",
    flatten = TRUE,
    ...
  )
  data <- data[intersect(predictors, colnames(data))]

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
      target <- intersect(predictors, colnames(data))
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
