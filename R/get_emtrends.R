#' @rdname get_emmeans
#' @examplesIf insight::check_if_installed("emmeans", quietly = TRUE)
#' \dontrun{
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#' get_emtrends(model)
#' get_emtrends(model, by = "Species")
#' get_emtrends(model, by = "Petal.Length")
#' get_emtrends(model, by = c("Species", "Petal.Length"))
#' }
#'
#' model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)
#' get_emtrends(model)
#' get_emtrends(model, by = "Sepal.Width")
#' @export
get_emtrends <- function(
  model,
  trend = NULL,
  by = NULL,
  predict = NULL,
  keep_iterations = FALSE,
  verbose = TRUE,
  ...
) {
  # check if available
  insight::check_if_installed("emmeans")

  # Guess arguments
  my_args <- .guess_emtrends_arguments(model, trend, by, verbose, ...)

  # ordinal models: prediction mode, used for formatting and the table footer
  ordinal_mode <- NULL

  # setup function arguments
  fun_args <- insight::compact_list(c(
    list(
      model,
      specs = my_args$emmeans_specs,
      var = my_args$trend,
      at = my_args$emmeans_at
    ),
    list(...)
  ))

  # handle distributional parameters
  if (inherits(model, "brmsfit")) {
    if (identical(predict, "response") || is.null(predict)) {
      # slope of the expected response
      fun_args$epred <- TRUE
      fun_args$type <- "response"
    } else if (predict %in% .brms_aux_elements(model)) {
      # slope of a specific distributional parameter
      fun_args$dpar <- predict
    } else {
      # link, none, unlink, etc.
      fun_args$type <- predict
    }
  } else {
    # ordinal models need the `mode` argument, and probabilities are the default
    ordinal <- .emmeans_ordinal_mode(model, predict)
    ordinal_mode <- ordinal$mode
    if (is.null(ordinal_mode)) {
      fun_args$type <- predict
    } else {
      fun_args <- .emtrends_ordinal_arguments(fun_args, ordinal)
    }
    .check_ordinal_response_filter(my_args$response_filter, ordinal_mode)
  }

  # Run emtrends
  estimated <- suppressMessages(do.call(emmeans::emtrends, fun_args))

  # slopes of probabilities or of the mean class, from the slopes of the
  # cumulative probabilities
  if (!is.null(ordinal_mode) && ordinal_mode %in% c("prob", "mean.class")) {
    estimated <- .emtrends_ordinal_from_cumprob(estimated, model, ordinal_mode)
  }

  # for Bayesian model, keep iterations
  if (insight::model_info(model, response = 1)$is_bayesian) {
    attr(estimated, "posterior_draws") <- insight::get_parameters(estimated)
  } else {
    keep_iterations <- FALSE
  }

  attr(estimated, "trend") <- my_args$trend
  attr(estimated, "at") <- my_args$by
  attr(estimated, "by") <- my_args$by
  attr(estimated, "coef_name") <- "Slope"
  attr(estimated, "transform") <- TRUE
  attr(estimated, "keep_iterations") <- keep_iterations
  attr(estimated, "ordinal_mode") <- ordinal_mode
  attr(estimated, "response_filter") <- my_args$response_filter

  estimated
}


# For ordinal models, sets the `mode` and `specs` arguments of `emtrends()`.
# `ordinal` is the list returned by `.emmeans_ordinal_mode()`. Slopes of
# probabilities and of the mean class are linear functions of the slopes of
# the cumulative probabilities. We compute them from these (see
# `.emtrends_ordinal_from_cumprob()`), because `emtrends()` with these modes
# can fail ("Irregular reference grid") or return wrong slopes (clmm)
.emtrends_ordinal_arguments <- function(fun_args, ordinal) {
  if (ordinal$mode %in% c("prob", "mean.class")) {
    fun_args$mode <- "cum.prob"
    fun_args$specs <- .add_emmeans_pseudo_factor(fun_args$specs, "cut")
  } else {
    fun_args$mode <- ordinal$mode
    fun_args$specs <- .add_emmeans_pseudo_factor(fun_args$specs, ordinal$pseudo_factor)
  }
  fun_args
}


# For ordinal models, computes the slopes of the probability of each response
# category (`mode = "prob"`), or of the mean class (`mode = "mean.class"`),
# from the slopes of the cumulative probabilities at each threshold (`cut`).
# With K categories, P(y = 1) = P(y <= 1), P(y = k) = P(y <= k) - P(y <= k - 1),
# and P(y = K) = 1 - P(y <= K - 1). The mean class is 1 + the sum of the
# exceedance probabilities, i.e. K - the sum of the cumulative probabilities.
.emtrends_ordinal_from_cumprob <- function(estimated, model, ordinal_mode) {
  n_cuts <- length(estimated@levels$cut)
  by <- setdiff(names(estimated@levels), "cut")
  if (!length(by)) {
    by <- NULL
  }
  if (ordinal_mode == "mean.class") {
    out <- emmeans::contrast(
      estimated,
      method = list(mean.class = rep(-1, n_cuts)),
      by = by,
      adjust = "none"
    )
    # drop the factor with the name of the linear function, which has only
    # one level
    out <- suppressMessages(emmeans::emmeans(out, if (is.null(by)) ~1 else by))
  } else {
    # names of the response categories, as emmeans labels them for
    # `mode = "prob"`
    pseudo_factor <- .emmeans_ordinal_pseudo_factor(model, "prob")
    categories <- suppressMessages(emmeans::ref_grid(model, mode = "prob")@levels[[
      pseudo_factor
    ]])
    coefficients <- lapply(seq_len(n_cuts + 1), function(k) {
      weights <- numeric(n_cuts)
      if (k <= n_cuts) {
        weights[k] <- 1
      }
      if (k > 1) {
        weights[k - 1] <- -1
      }
      weights
    })
    names(coefficients) <- categories
    out <- emmeans::contrast(
      estimated,
      method = coefficients,
      by = by,
      name = pseudo_factor,
      adjust = "none"
    )
  }
  out
}


# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================

#' @keywords internal
.guess_emtrends_arguments <- function(
  model,
  trend = NULL,
  by = NULL,
  verbose = TRUE,
  ...
) {
  # Gather info
  model_data <- insight::get_data(model, verbose = FALSE)
  predictors <- intersect(
    colnames(model_data),
    insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)
  )

  # Guess arguments
  if (is.null(trend)) {
    trend <- predictors[sapply(model_data[predictors], is.numeric)][1]
    if (!length(trend) || is.na(trend)) {
      insight::format_error(
        "Model contains no numeric predictor. Please specify `trend`."
      )
    }
    if (verbose) {
      insight::format_alert(paste0(
        "No numeric variable was specified for slope estimation. Selecting `trend = \"",
        trend,
        "\"`."
      ))
    }
  }
  if (length(trend) > 1) {
    trend <- trend[1]
    if (verbose) {
      insight::format_alert(paste0(
        "More than one numeric variable was selected for slope estimation. Keeping only `",
        trend[1],
        "`."
      ))
    }
  }

  if (trend %in% colnames(model_data) && !is.numeric(model_data[[trend]])) {
    insight::format_error(paste0(
      "Variable `",
      trend,
      "` is not numeric. Slopes can only be estimated for numeric variables when `backend = \"emmeans\"`. Please use `estimate_contrasts()` instead."
    ))
  }

  my_args <- list(trend = trend, by = by)
  .process_emmeans_arguments(model, args = my_args, data = model_data, ...)
}


# Formatting ===============================================================

.format_emmeans_slopes <- function(model, estimated, ci, ...) {
  # Summarize and clean
  if (insight::model_info(model, response = 1)$is_bayesian) {
    trends <- parameters::parameters(estimated, ci = ci, ...)
    trends <- .clean_names_bayesian(trends, model, predict = "none", type = "trend")
    em_grid <- as.data.frame(estimated@grid)
    em_grid[[".wgt."]] <- NULL # Drop the weight column
    colums_to_add <- setdiff(colnames(em_grid), colnames(trends))
    if (length(colums_to_add)) {
      trends <- cbind(em_grid[colums_to_add], trends)
    }
  } else {
    trends <- parameters::parameters(estimated, ci = ci, ...)
  }
  # Remove the "1 - overall" column that can appear in cases like y ~ x
  trends <- trends[names(trends) != "1"]

  # rename
  trends <- datawizard::data_rename(trends, select = c(Slope = "Coefficient"))

  # ordinal models: rename the response categories or thresholds, and place
  # them right before the slopes
  ordinal_mode <- attributes(estimated)$ordinal_mode
  if (!is.null(ordinal_mode)) {
    trends <- .clean_pseudo_factor_emmeans_ordinal(
      trends,
      model,
      ordinal_mode,
      attributes(estimated)$response_filter
    )
    pseudo_column <- intersect(c("Response", "Threshold"), colnames(trends))
    if (length(pseudo_column)) {
      trends <- datawizard::data_relocate(trends, pseudo_column, before = "Slope")
    }
  }

  # Restore factor levels
  out <- datawizard::data_restoretype(trends, insight::get_data(model, verbose = FALSE))

  # add posterior draws?
  .add_posterior_draws_emmeans(attributes(estimated), out)
}
