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
#' @param predict Is passed to the `type` argument in `emmeans::emmeans()`. See
#' [this vignette](https://CRAN.R-project.org/package=emmeans/vignettes/transformations.html).
#' Can be `"link"` (default for contrasts), `"response"` (default for means),
#' `"mu"`, `"unlink"`, `"log"`. `"link"` will leave the values on scale of the
#' linear predictors. `"response"` will transform them on scale of the response
#' variable. Thus for a logistic model, `"link"` will give estimations expressed
#' in log-odds (probabilities on logit scale) and `"response"` in terms of
#' probabilities. To predict distributional parameters (called "dpar" in other
#' packages), for instance when using complex formulae in `brms` models, the
#' `predict` argument can take the value of the parameter you want to estimate,
#' for instance `"sigma"`, `"kappa"`, etc.
#' @param by The predictor variable(s) at which to evaluate the desired effect
#' / mean / contrasts. Other predictors of the model that are not included
#' here will be collapsed and "averaged" over (the effect will be estimated
#' across them).
#' @param ... Other arguments passed for instance to [insight::get_datagrid()].
#' @param transform Deprecated, please use `predict` instead.
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

#' @rdname get_emmeans
#' @export
model_emmeans <- get_emmeans


# =========================================================================
# HELPERS  ----------------------------------------------------------------
# =========================================================================
# This function is the actual equivalent of get_marginalmeans(); both being used
# in estimate_means

#' @keywords internal
.format_emmeans_means <- function(model, estimated, ci = 0.95, ...) {
  predict <- attributes(estimated)$predict
  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    means <- parameters::parameters(estimated, ci = ci, ...)
    means <- .clean_names_bayesian(means, model, predict, type = "mean")
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
  means
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
  .format_emmeans_arguments(model, args = my_args, data = model_data, ...)
}


## TODO: validate predict argument to make sure it only has valid options
.get_emmeans_type_argument <- function(model, predict, type = "means", ...) {
  if (is.null(predict)) {
    predict <- switch(type,
      means = "response",
      "none"
    )
  } else if (predict == "link") {
    predict <- "none"
  }
  predict
}


#' @keywords internal
.format_emmeans_arguments <- function(model, args, data, ...) {
  # Create the data_matrix
  # ---------------------------
  # data <- insight::get_data(model)
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
