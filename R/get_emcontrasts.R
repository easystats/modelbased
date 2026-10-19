#' @rdname get_emmeans
#' @examplesIf insight::check_if_installed("emmeans", quietly = TRUE)
#' # Basic usage
#' model <- lm(Sepal.Width ~ Species, data = iris)
#' get_emcontrasts(model)
#'
#' \dontrun{
#' # Dealing with interactions
#' model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
#' # By default: selects first factor
#' get_emcontrasts(model)
#' # Or both
#' get_emcontrasts(model, contrast = c("Species", "Petal.Width"), length = 2)
#' # Or with custom specifications
#' get_emcontrasts(model, contrast = c("Species", "Petal.Width=c(1, 2)"))
#' # Or modulate it
#' get_emcontrasts(model, by = "Petal.Width", length = 4)
#' }
#' @export
get_emcontrasts <- function(
  model,
  contrast = NULL,
  by = NULL,
  predict = NULL,
  comparison = "revpairwise",
  keep_iterations = FALSE,
  verbose = TRUE,
  ...
) {
  # check if available
  insight::check_if_installed("emmeans")

  # check whether contrasts should be made for numerics or categorical
  model_data <- insight::get_data(model, source = "mf", verbose = FALSE)
  on_the_fly_factors <- attributes(model_data)$factors

  # Guess arguments
  my_args <- .guess_emcontrasts_arguments(model, contrast, by, verbose, ...)

  # find default response-type
  predict <- .get_emmeans_type_argument(model, predict, type = "contrasts", ...)

  # extract first focal term
  first_focal <- my_args$contrast[1]

  # setup arguments
  fun_args <- list(model)
  ordinal <- list(mode = NULL, pseudo_factor = NULL)

  # handle distributional parameters
  if (inherits(model, "brmsfit")) {
    if (identical(predict, "response")) {
      dpars <- FALSE
      fun_args$epred <- TRUE
      fun_args$type <- "response"
    } else if (predict %in% .brms_aux_elements(model)) {
      dpars <- TRUE
      fun_args$dpar <- predict
    } else {
      dpars <- FALSE
      fun_args$type <- predict
    }
  } else {
    dpars <- FALSE
    # ordinal models need the `mode` argument, and probabilities are the default
    ordinal <- .emmeans_ordinal_mode(model, predict)
    predict <- ordinal$predict
    if (is.null(ordinal$mode)) {
      fun_args$type <- predict
    } else {
      fun_args$mode <- ordinal$mode
    }
    .check_ordinal_response_filter(my_args$response_filter, ordinal$mode)
    .check_ordinal_response_filter(my_args$contrast_filter, ordinal$mode, "contrast")
  }

  # add dots
  dots <- list(...)
  fun_args <- insight::compact_list(c(fun_args, dots))

  # if first focal term is numeric, we contrast slopes
  if (
    is.numeric(model_data[[first_focal]]) &&
      !first_focal %in% on_the_fly_factors &&
      # if these are identical, only slopes are contrasted - we need emmeans then
      !identical(my_args$by, my_args$contrast)
  ) {
    # sanity check - contrast for slopes only makes sense when we have a "by" argument
    if (is.null(my_args$by)) {
      insight::format_error(
        "Please specify the `by` argument to calculate contrasts of slopes."
      )
    }
    # filter values in `by`, but not for the response categories of ordinal
    # models, which are filtered later
    slope_filter <- .parse_by_filter(by)
    slope_filter <- slope_filter[setdiff(
      intersect(names(slope_filter), my_args$by),
      c(my_args$contrast, my_args$response_filter$response)
    )]
    # emmeans holds numeric variables in `by` at their mean, and `at` does not
    # work for factors that are created in the model formula. So we only
    # filter when all variables in `by` are factors or character vectors
    numeric_by <- vapply(my_args$by, function(i) is.numeric(model_data[[i]]), logical(1))
    if (any(numeric_by)) {
      slope_filter <- list()
    }
    slope_filter <- .present_filter_values(model_data, slope_filter)
    # sanity check - we need at least two slopes to compare
    if (
      (is.character(comparison) && !.is_custom_comparison(comparison)) ||
        inherits(comparison, "formula")
    ) {
      # ordinal models: the slopes are contrasted within each response
      # category or threshold (or, for `mean.class`, once per level of `by`),
      # so the slopes to compare are counted from `by`
      .check_slope_contrast_filter(
        model,
        my_args$by,
        slope_filter,
        within_categories = !is.null(ordinal$mode)
      )
    }
    # Run emmeans. Filters in `by` go into `at`, so only those slopes are
    # contrasted
    fun_args <- c(fun_args, list(specs = my_args$by, var = my_args$contrast))
    if (length(slope_filter)) {
      fun_args$at <- slope_filter
    }
    if (is.null(ordinal$mode)) {
      estimated <- suppressMessages(do.call(emmeans::emtrends, fun_args))
      emm_by <- NULL
    } else {
      # ordinal models: for modes with response categories or thresholds, we
      # contrast the slopes within each one
      fun_args <- .emtrends_ordinal_arguments(fun_args, ordinal)
      estimated <- suppressMessages(do.call(emmeans::emtrends, fun_args))
      if (ordinal$mode %in% c("prob", "mean.class")) {
        estimated <- .emtrends_ordinal_from_cumprob(estimated, model, ordinal$mode)
      }
      emm_by <- ordinal$pseudo_factor
    }
  } else {
    # Run emmeans. For ordinal models, the pseudo-factor (response categories
    # or thresholds) is added to `specs`, so we contrast within each category
    specs <- .add_emmeans_pseudo_factor(my_args$emmeans_specs, ordinal$pseudo_factor)
    fun_args <- c(fun_args, list(specs = specs, at = my_args$emmeans_at))
    estimated <- suppressMessages(do.call(emmeans::emmeans, fun_args))
    # ordinal models: a `contrast` term on the response with values keeps only
    # those response categories, before they are contrasted
    estimated <- .filter_ordinal_contrast_grid(
      estimated,
      ordinal$pseudo_factor,
      my_args$contrast_filter
    )
    # Find by variables
    emm_by <- specs[!specs %in% my_args$contrast]
    if (length(emm_by) == 0) {
      emm_by <- NULL
    }
  }

  # If means are on the response scale (e.g., probabilities), need to regrid
  if (predict == "response" || dpars) {
    estimated <- emmeans::regrid(estimated)
  }

  # joint test?
  if (identical(comparison, "joint")) {
    comparison <- "pairwise"
    joint_test <- TRUE
  } else {
    joint_test <- FALSE
  }

  out <- emmeans::contrast(estimated, by = emm_by, method = comparison, ...)

  # intermediate step: joint tests
  if (joint_test) {
    out <- .get_jointtest(out, my_args)
  }

  # for Bayesian model, keep iterations
  if (insight::model_info(model, response = 1)$is_bayesian) {
    attr(out, "posterior_draws") <- insight::get_parameters(estimated)
  } else {
    keep_iterations <- FALSE
  }

  attr(out, "contrast") <- my_args$contrast
  attr(out, "predict") <- predict
  attr(out, "at") <- my_args$by
  attr(out, "by") <- my_args$by
  # the pseudo-factor of ordinal models is no focal term
  focal_terms <- setdiff(emm_by, ordinal$pseudo_factor)
  if (!length(focal_terms)) {
    focal_terms <- NULL
  }
  attr(out, "focal_terms") <- focal_terms
  attr(out, "p_adjust") <- list(...)$adjust
  attr(out, "comparison") <- comparison
  attr(out, "joint_test") <- joint_test
  attr(out, "transform") <- TRUE
  attr(out, "keep_iterations") <- keep_iterations
  attr(out, "ordinal_mode") <- ordinal$mode
  attr(out, "response_filter") <- my_args$response_filter

  out
}


# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================

#' @keywords internal
.guess_emcontrasts_arguments <- function(
  model,
  contrast = NULL,
  by = NULL,
  verbose = TRUE,
  ...
) {
  # Gather info
  model_data <- insight::get_data(model, source = "mf", verbose = FALSE)
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
    if (verbose) {
      insight::format_alert(paste0(
        "No variable was specified for contrast estimation. Selecting `contrast = \"",
        contrast,
        "\"`."
      ))
    }
  } else if (all(contrast == "all")) {
    contrast <- predictors
  }

  my_args <- list(contrast = contrast, by = by)
  .process_emmeans_arguments(model, args = my_args, data = model_data, ...)
}


# Table formatting emmeans ----------------------------------------------------

.format_emmeans_contrasts <- function(model, estimated, ci, p_adjust, ...) {
  predict <- attributes(estimated)$predict
  m_info <- insight::model_info(model, response = 1)

  # Summarize and clean
  if (m_info$is_bayesian) {
    out <- cbind(
      estimated@grid,
      bayestestR::describe_posterior(
        estimated,
        ci = ci,
        diagnostic = NULL,
        verbose = FALSE,
        ...
      )
    )
    out <- .clean_names_bayesian(out, model, predict, type = "contrast")
  } else {
    ordinal_mode <- attributes(estimated)$ordinal_mode
    if (isTRUE(attributes(estimated)$joint_test)) {
      out <- as.data.frame(estimated)
    } else {
      estimated_data <- as.data.frame(estimated)
      out <- as.data.frame(merge(
        estimated_data,
        stats::confint(estimated, level = ci, adjust = p_adjust)
      ))
      # for ordinal models with response categories or thresholds, we keep
      # the order of emmeans, else these are sorted alphabetically
      if (
        !is.null(ordinal_mode) &&
          !is.null(.emmeans_ordinal_pseudo_factor(model, ordinal_mode))
      ) {
        # all columns of the emmeans summary form the key, because a numeric
        # column can also identify the rows (e.g. a numeric `by` variable)
        row_keys <- function(d) {
          do.call(paste, c(lapply(d[colnames(estimated_data)], as.character), sep = "\r"))
        }
        out <- out[match(row_keys(estimated_data), row_keys(out)), , drop = FALSE]
        rownames(out) <- NULL
      }
    }
    # the response categories or thresholds are renamed first, because the
    # response can have the name of an output column (e.g. "response")
    out <- .clean_pseudo_factor_emmeans_ordinal(
      out,
      model,
      ordinal_mode,
      attributes(estimated)$response_filter
    )
    out <- .clean_names_frequentist(out, predict, m_info)
  }
  out$null <- NULL # introduced in emmeans 1.6.1 (#115)

  # relicate CI columns
  if (all(c("CI_low", "CI_high") %in% colnames(out))) {
    out <- datawizard::data_relocate(
      out,
      c("CI_low", "CI_high"),
      after = c("Difference", "Odds_ratio", "Ratio")
    )
  }

  # Format contrasts names
  # Split by either " - " or "/"
  # If levels contain " - " themselves, emmeans wraps them in parentheses
  # like "(A - Low) - (A - High)", so we need to handle that case
  contrast_strings <- as.character(out$contrast)
  if (all(grepl("^\\(", contrast_strings))) {
    # Levels with " - " are wrapped in parentheses by emmeans, e.g.
    # "(A - Low) - (A - High)". Extract the parenthesized groups.
    level_list <- lapply(contrast_strings, function(x) {
      m <- regmatches(x, gregexpr("\\([^)]+\\)", x))[[1]]
      if (length(m) >= 2) {
        insight::trim_ws(gsub("[()]", "", m[seq_len(2)]))
      } else {
        insight::trim_ws(strsplit(x, " - |\\/")[[1]])
      }
    })
  } else {
    level_list <- lapply(contrast_strings, function(x) {
      insight::trim_ws(strsplit(x, " - |\\/")[[1]])
    })
  }
  level_cols <- data.frame(do.call(rbind, level_list))

  # other comparison methods than "pairwise" do not return two columns
  if (ncol(level_cols) == 2) {
    colnames(level_cols) <- c("Level1", "Level2")
    level_cols$Level1 <- gsub(",", " - ", level_cols$Level1, fixed = TRUE)
    level_cols$Level2 <- gsub(",", " - ", level_cols$Level2, fixed = TRUE)
  } else if (ncol(level_cols) > 0) {
    colnames(level_cols)[1] <- "Level"
  }

  # Merge levels and rest
  out$contrast <- NULL
  # for joint tests, we have no levels, and ncol is 0
  if (ncol(level_cols) > 0) {
    out <- cbind(level_cols, out)
  }

  # add posterior draws?
  .add_posterior_draws_emmeans(attributes(estimated), out)
}
