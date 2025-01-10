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
                           backend = getOption("modelbased_backend", "emmeans"),
                           transform = NULL,
                           verbose = TRUE,
                           ...) {
  ## TODO: remove deprecation warning later
  if (!is.null(transform)) {
    insight::format_warning("Argument `transform` is deprecated. Please use `predict` instead.")
    predict <- transform
  }

  if (backend == "emmeans") {
    # Emmeans ------------------------------------------------------------------
    estimated <- get_emmeans(model, by = by, predict = predict, verbose = verbose, ...)
    means <- .format_emmeans_means(estimated, model, ci = ci, verbose = verbose, ...)
  } else {
    # Marginalmeans ------------------------------------------------------------
    estimated <- get_marginalmeans(model, by = by, predict = predict, ci = ci, verbose = verbose, ...)
    means <- format(estimated, model, ...)
  }

  # restore attributes later
  info <- attributes(estimated)

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
  attr(means, "backend") <- backend
  attr(means, "coef_name") <- intersect(.valid_coefficient_names(), colnames(means))

  # add attributes from workhorse function
  attributes(means) <- utils::modifyList(
    attributes(means),
    info[c("at", "by", "datagrid", "predict", "focal_terms")]
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
    result_type <- switch(type,
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

  if (all(table_footer == "")) { # nolint
    table_footer <- NULL
  }

  c(table_footer, "blue")
}
