#' Estimate Marginal Means (Model-based average at each factor level)
#'
#' Estimate average value of response variable at each factor level. For
#' plotting, check the examples in [visualisation_recipe()]. See also
#' other related functions such as [estimate_contrasts()] and
#' [estimate_slopes()].
#'
#' @param model A statistical model.
#' @param by The predictor variable(s) at which to evaluate the desired effect
#' / mean / contrasts. Other predictors of the model that are not included
#' here will be collapsed and "averaged" over (the effect will be estimated
#' across them).
#' @param predict Is passed to the `type` argument in `emmeans::emmeans()` (when
#' `backend = "emmeans"`) or in `marginaleffects::avg_predictions()` (when
#' `backend = "marginaleffects"`). For emmeans, see also
#' [this vignette](https://CRAN.R-project.org/package=emmeans/vignettes/transformations.html).
#' Valid options for `predict`` are:
#'
#' * `backend = "emmeans"`: `predict` can be `"response"`, `"link"`, `"mu"`,
#'   `"unlink"`, or `"log"`. If `predict = NULL` (default), the most appropriate
#'   transformation is selected (which usually is `"response"`).
#' * `backend = "marginaleffects"`: `predict` can be `"response"`, `"link"` or
#'   any valid `type` option supported by model's class `predict()` method. By
#'   default, when `predict = NULL`, the most appropriate transformation is
#'   selected, which usually returns predictions or contrasts on the
#'   response-scale.
#'
#' `"link"` will leave the values on scale of the linear predictors.
#' `"response"` (or `NULL`) will transform them on scale of the response
#' variable. Thus for a logistic model, `"link"` will give estimations expressed
#' in log-odds (probabilities on logit scale) and `"response"` in terms of
#' probabilities. To predict distributional parameters (called "dpar" in other
#' packages), for instance when using complex formulae in `brms` models, the
#' `predict` argument can take the value of the parameter you want to estimate,
#' for instance `"sigma"`, `"kappa"`, etc.
#' @param backend Whether to use `"emmeans"` or `"marginaleffects"` as a backend.
#' Results are usually very similar. The major difference will be found for mixed
#' models, where `backend = "marginaleffects"` will also average across random
#' effects levels, producing "marginal predictions" (instead of "conditional
#' predictions", see Heiss 2022).
#'
#' You can set a default backend via `options()`, e.g. use
#' `options(modelbased_backend = "emmeans")` to use the **emmeans** package or
#' `options(modelbased_backend = "marginaleffects")` to set **marginaleffects**
#' as default backend.
#' @param transform Deprecated, please use `predict` instead.
#' @param ... Other arguments passed for instance to [insight::get_datagrid()].
#'
#' @inheritParams parameters::model_parameters.default
#' @inheritParams estimate_expectation
#' @inherit estimate_slopes details
#'
#' @return A data frame of estimated marginal means.
#'
#' @references
#' Heiss, A. (2022). Marginal and conditional effects for GLMMs with
#' {marginaleffects}. Andrew Heiss. \doi{10.59350/xwnfm-x1827}
#'
#' @examplesIf all(insight::check_if_installed(c("emmeans", "see", "lme4"), quietly = TRUE))
#' library(modelbased)
#'
#' # Frequentist models
#' # -------------------
#' model <- lm(Petal.Length ~ Sepal.Width * Species, data = iris)
#'
#' estimate_means(model)
#' estimate_means(model, by = c("Species", "Sepal.Width"), length = 2)
#' estimate_means(model, by = "Species=c('versicolor', 'setosa')")
#' estimate_means(model, by = "Sepal.Width=c(2, 4)")
#' estimate_means(model, by = c("Species", "Sepal.Width=0"))
#' estimate_means(model, by = "Sepal.Width", length = 5)
#' estimate_means(model, by = "Sepal.Width=c(2, 4)")
#'
#' # Methods that can be applied to it:
#' means <- estimate_means(model, by = c("Species", "Sepal.Width=0"))
#'
#' plot(means) # which runs visualisation_recipe()
#' standardize(means)
#'
#' \donttest{
#' data <- iris
#' data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#'
#' model <- lme4::lmer(
#'   Petal.Length ~ Sepal.Width + Species + (1 | Petal.Length_factor),
#'   data = data
#' )
#' estimate_means(model)
#' estimate_means(model, by = "Sepal.Width", length = 3)
#' }
#' @export
estimate_means <- function(model,
                           by = "auto",
                           predict = NULL,
                           ci = 0.95,
                           backend = getOption("modelbased_backend", "emmeans"),
                           transform = NULL,
                           ...) {
  ## TODO: remove deprecation warning later
  if (!is.null(transform)) {
    insight::format_warning("Argument `transform` is deprecated. Please use `predict` instead.")
    predict <- transform
  }

  if (backend == "emmeans") {
    # Emmeans ------------------------------------------------------------------
    estimated <- get_emmeans(model, by = by, predict = predict, ...)
    means <- .format_emmeans_means(model, estimated = estimated, ci = ci, ...)
  } else {
    # Marginalmeans ------------------------------------------------------------
    estimated <- get_marginalmeans(model, by = by, predict = predict, ci, ...)
    means <- .format_marginaleffects_means(model, estimated = estimated, ...)
  }

  # Table formatting
  attr(means, "table_title") <- c("Estimated Marginal Means", "blue")
  attr(means, "table_footer") <- .estimate_means_footer(
    means,
    type = "means",
    predict = attributes(estimated)$predict,
    model_info = insight::model_info(model)
  )

  # Add attributes
  attr(means, "model") <- model
  attr(means, "response") <- insight::find_response(model)
  attr(means, "ci") <- ci
  attr(means, "transform") <- predict
  attr(means, "backend") <- backend

  attr(means, "coef_name") <- intersect(
    c("Mean", "Probability", tools::toTitleCase(.brms_aux_elements())),
    names(means)
  )

  # Output
  class(means) <- c("estimate_means", class(means))
  means
}


# Table formatting emmeans ----------------------------------------------------


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


# Table formatting marginaleffects --------------------------------------------


#' @keywords internal
.format_marginaleffects_means <- function(model, estimated, predict = NULL, ...) {
  predict <- attributes(estimated)$predict
  # model information
  model_data <- insight::get_data(model)
  info <- insight::model_info(model, verbose = FALSE)
  non_focal <- setdiff(colnames(model_data), attr(estimated, "focal_terms"))
  is_contrast_analysis <- !is.null(list(...)$hypothesis)
  predict_type <- attributes(estimated)$predict

  # do we have contrasts? For contrasts, we want to keep p-values
  if (is_contrast_analysis) {
    remove_column <- "SE"
    estimate_name <- "Difference"
  } else {
    remove_column <- "p"
    # estimate name
    if (!is.null(predict_type) && tolower(predict_type) %in% .brms_aux_elements()) {
      # for Bayesian models with distributional parameter
      estimate_name <- tools::toTitleCase(predict_type)
    } else if (!predict %in% c("none", "link") && (info$is_binomial || info$is_bernoulli)) {
      estimate_name <- "Probability"
    } else {
      estimate_name <- "Mean"
    }
  }

  # Format
  params <- suppressWarnings(parameters::model_parameters(estimated, verbose = FALSE))
  # add ci?
  params <- .add_contrasts_ci(is_contrast_analysis, params)
  params <- datawizard::data_relocate(params, c("Predicted", "SE", "CI_low", "CI_high"), after = -1, verbose = FALSE) # nolint
  # move p to the end
  params <- datawizard::data_relocate(params, "p", after = -1, verbose = FALSE)
  params <- datawizard::data_rename(params, "Predicted", estimate_name)
  # remove redundant columns
  params <- datawizard::data_remove(params, c(remove_column, "Statistic", "s.value", "S", "CI", "df", "rowid_dedup", non_focal), verbose = FALSE) # nolint
  params <- datawizard::data_restoretype(params, model_data)
  # Rename for Categorical family
  if (info$is_categorical) {
    params <- datawizard::data_rename(params, "group", "Response")
  }

  # Store info
  attr(params, "at") <- attr(estimated, "by")
  attr(params, "by") <- attr(estimated, "by")
  params
}


# Bring arguments in shape for emmeans ----------------------------------------


#' @keywords internal
.process_emmeans_arguments <- function(model, args, data, ...) {
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


#' @keywords internal
.add_contrasts_ci <- function(is_contrast_analysis, params) {
  if (is_contrast_analysis && !"CI_low" %in% colnames(params) && "SE" %in% colnames(params)) {
    # extract ci-level
    if ("CI" %in% colnames(params)) {
      ci <- params[["CI"]][1]
    } else {
      ci <- attributes(params)$ci
    }
    if (is.null(ci)) {
      ci <- 0.95
    }
    # get degrees of freedom
    if ("df" %in% colnames(params)) {
      dof <- params[["df"]]
    } else {
      dof <- Inf
    }
    # critical test value
    crit <- stats::qt((1 + ci) / 2, df = dof)
    # add CI
    params$CI_low <- params$Predicted - crit * params$SE
    params$CI_high <- params$Predicted + crit * params$SE
  }
  params
}


# Table footer ===============================================================


.estimate_means_footer <- function(x,
                                   by = NULL,
                                   type = "means",
                                   p_adjust = NULL,
                                   predict = NULL,
                                   model_info = NULL) {
  table_footer <- paste("\nMarginal", type)

  # Levels
  if (!is.null(by) && length(by) > 0) {
    table_footer <- paste0(table_footer, " estimated at ", toString(by))
  } else {
    table_footer <- paste0(table_footer, " estimated at ", attr(x, "by"))
  }

  # P-value adjustment footer
  if (!is.null(p_adjust) && "p" %in% names(x)) {
    if (p_adjust == "none") {
      table_footer <- paste0(table_footer, "\np-values are uncorrected.")
    } else {
      table_footer <- paste0(table_footer, "\np-value adjustment method: ", parameters::format_p_adjust(p_adjust))
    }
  }

  # tell user about scale of predictions / contrasts
  if (!is.null(predict) && isFALSE(model_info$is_linear)) {
    result_type <- switch(
      type,
      means = "Predictions",
      contrasts = "Contrasts"
    )
    # exceptions
    predict <- switch(
      predict,
      none = "link",
      `invlink(link)` = "response",
      predict
    )
    table_footer <- paste0(table_footer, "\n", result_type, " are on the ", predict, "-scale.")
  }

  if (all(table_footer == "")) { # nolint
    table_footer <- NULL
  }

  c(table_footer, "blue")
}
