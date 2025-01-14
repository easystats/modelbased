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
#'   any valid `type` option supported by model's class `predict()` method (e.g.,
#'   for zero-inflation models from package **glmmTMB**, you can choose
#'   `predict = "zprob"` or `predict = "conditional"` etc., see
#'   [glmmTMB::predict.glmmTMB]). By default, when `predict = NULL`, the most
#'   appropriate transformation is selected, which usually returns predictions
#'   or contrasts on the response-scale.
#'
#' `"link"` will leave the values on scale of the linear predictors.
#' `"response"` (or `NULL`) will transform them on scale of the response
#' variable. Thus for a logistic model, `"link"` will give estimations expressed
#' in log-odds (probabilities on logit scale) and `"response"` in terms of
#' probabilities. To predict distributional parameters (called "dpar" in other
#' packages), for instance when using complex formulae in `brms` models, the
#' `predict` argument can take the value of the parameter you want to estimate,
#' for instance `"sigma"`, `"kappa"`, etc.
#' @param marginalize Character string, indicating the type of marginalization.
#' This dictates how the predictions are "averaged" over the non-focal predictors,
#' i.e. those variables that are not specified in `by` or `contrast`.
#' - `"average"` (default): Takes the mean value for non-focal numeric predictors and
#'   marginalizes over the factor levels of non-focal terms, which computes a
#'   kind of "weighted average" for the values at which these terms are hold
#'   constant. These predictions are a good representation of the sample,
#'   because all possible values and levels of the non-focal predictors are
#'   taken into account. It answers the question, "What is the predicted value
#'   for an 'average' observation in *my data*?". It refers to
#'   randomly picking a subject of your sample and the result you get on average.
#'   This approach is the one taken by default in the `emmeans` package.
#' - `"population"`: Non-focal predictors are marginalized over the observations
#'   in the sample, where the sample is replicated multiple times to produce
#'   "counterfactuals" and then takes the average of these predicted values
#'   (aggregated/grouped by the focal terms). It can be considered as
#'   extrapolation to the population. Counterfactual predictions are useful,
#'   insofar as the results can also be transferred to other contexts
#'   (Dickerman and Hernan, 2020). It answers the question, "What is the
#'   predicted for the 'average' observation in *the general population*?".
#'   It does not only refer to the actual data in your observed sample, but also
#'   "what would be if" we had more data, or if we had data from a different sample.
#'
#' In other words, the distinction between marginalization types resides in whether
#' the prediction are made for:
#' - A specific "individual" (i.e., a specific combination of predictor values):
#'   this is what is obtained when using [`estimate_relation()`] and the other
#'   prediction functions.
#' - An average individual: obtained with `estimate_means(..., marginalize = "average")`
#' - The "general population": obtained with `estimate_means(..., marginalize = "population")`
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
#' @param verbose Use `FALSE` to silence messages and warnings.
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
                           marginalize = "average",
                           backend = getOption("modelbased_backend", "emmeans"),
                           transform = NULL,
                           verbose = TRUE,
                           ...) {
  ## TODO: remove deprecation warning later
  if (!is.null(transform)) {
    insight::format_warning("Argument `transform` is deprecated. Please use `predict` instead.")
    predict <- transform
  }

  # validate input
  marginalize <- insight::validate_argument(marginalize, c("average", "population"))

  if (backend == "emmeans") {
    # Emmeans ------------------------------------------------------------------
    estimated <- get_emmeans(model, by = by, predict = predict, verbose = verbose, ...)
    means <- .format_emmeans_means(estimated, model, ci = ci, verbose = verbose, ...)
  } else {
    # Marginalmeans ------------------------------------------------------------
    estimated <- get_marginalmeans(model, by = by, predict = predict, ci = ci, marginalize = marginalize, verbose = verbose, ...) # nolint
    means <- format(estimated, model, ...)
  }

  # restore attributes later
  info <- attributes(estimated)

  # Table formatting
  attr(means, "table_title") <- c("Estimated Marginal Means", "blue")
  attr(means, "table_footer") <- .estimate_means_footer(
    means,
    type = ifelse(marginalize == "population", "counterfactuals", "means"),
    predict = attributes(estimated)$predict,
    model_info = insight::model_info(model)
  )

  # Add attributes
  attr(means, "model") <- model
  attr(means, "response") <- insight::find_response(model)
  attr(means, "ci") <- ci
  attr(means, "backend") <- backend
  attr(means, "coef_name") <- intersect(.valid_coefficient_names(), colnames(means))

  # add attributes from workhorse function
  attributes(means) <- utils::modifyList(
    attributes(means),
    info[c("at", "by", "datagrid", "predict", "focal_terms", "preserve_range")]
  )

  # Output
  class(means) <- unique(c("estimate_means", class(means)))
  means
}


# Table footer ===============================================================


.estimate_means_footer <- function(x,
                                   by = NULL,
                                   type = "means",
                                   p_adjust = NULL,
                                   predict = NULL,
                                   model_info = NULL,
                                   comparison = NULL,
                                   datagrid = NULL) {
  table_footer <- switch(type,
    counterfactuals = "Average",
    "Marginal"
  )
  table_footer <- paste0("\n", table_footer, " ", type)

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
    result_type <- switch(type,
      counterfactuals = ,
      means = "Predictions",
      contrasts = "Contrasts"
    )
    # exceptions
    predict <- switch(predict,
      none = "link",
      `invlink(link)` = "response",
      predict
    )
    table_footer <- paste0(table_footer, "\n", result_type, " are on the ", predict, "-scale.")
  }

  # for special hypothesis testing, like "(b1 - b2) = (b4 - b3)", we want to
  # add information about the parameter names
  if (!is.null(comparison) && is.character(comparison) && grepl("=", comparison, fixed = TRUE) && grepl("\\bb\\d+\\b", comparison)) { # nolint
    # find all "b" strings
    matches <- gregexpr("\\bb\\d+\\b", comparison)[[1]]
    match_lengths <- attr(matches, "match.length")

    # extract all "b" strings, so we have a vector of all "b" used in the comparison
    parameter_names <- unlist(lapply(seq_along(matches), function(i) {
      substr(comparison, matches[i], matches[i] + match_lengths[i] - 1)
    }), use.names = FALSE)

    # datagrid contains all parameters, so we just need to find out the rows
    # and combine column names with row values
    if (!is.null(datagrid)) {
      # transpose, so we can easier extract information
      transposed_dg <- t(datagrid)
      # interate over all parameters and create labels with proper names
      hypothesis_labels <- unlist(lapply(parameter_names, function(i) {
        rows <- as.numeric(sub(".", "", i))
        paste0(i, " = ", toString(paste0(colnames(datagrid), " [", transposed_dg[, rows], "]")))
      }), use.names = FALSE)
      # add all names to the footer
      table_footer <- paste0(
        table_footer,
        "\n",
        paste0("Parameters:\n", paste(unlist(hypothesis_labels), collapse = "\n"))
      )
    }
  }

  if (all(table_footer == "")) { # nolint
    return(NULL)
  }

  c(paste0(table_footer, "\n"), "blue")
}
