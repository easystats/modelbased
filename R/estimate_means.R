#' Estimate Marginal Means (Model-based average at each factor level)
#'
#' Estimate average value of response variable at each factor levels. For
#' plotting, check the examples in [visualisation_recipe()]. See also
#' other related functions such as [estimate_contrasts()] and
#' [estimate_slopes()].
#'
#' @inheritParams get_emmeans
#' @inheritParams parameters::model_parameters.default
#' @inheritParams estimate_expectation
#' @param backend Whether to use `"emmeans"` or `"marginaleffects"` as a backend.
#' Results are usually very similar. The major difference will be found for mixed
#' models, where `backend = "marginaleffects"` will also average across random
#' effects levels, producing "marginal predictions" (instead of "conditional
#' predictions", see Heiss 2022).
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
                           backend = "emmeans",
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
    means <- .format_emmeans_means(
      model,
      estimated = estimated,
      ci = ci,
      ...
    )
  } else {
    # Marginalmeans ------------------------------------------------------------
    estimated <- get_marginalmeans(model, by = by, predict = predict, ci, ...)
    means <- .format_marginaleffects_means(
      model,
      estimated = estimated,
      ...
    )
  }


  # Table formatting
  attr(means, "table_title") <- c("Estimated Marginal Means", "blue")
  attr(means, "table_footer") <- .estimate_means_footer(means, type = "means")

  # Add attributes
  attr(means, "model") <- model
  attr(means, "response") <- insight::find_response(model)
  attr(means, "ci") <- ci
  attr(means, "transform") <- predict

  attr(means, "coef_name") <- intersect(
    c("Mean", "Probability", .capitalize(.brms_aux_elements())),
    names(means)
  )


  # Output
  class(means) <- c("estimate_means", class(means))
  means
}


# Table Formating ----------------------------------------------------------


.estimate_means_footer <- function(x, by = NULL, type = "means", p_adjust = NULL) {
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

  if (all(table_footer == "")) table_footer <- NULL # nolint
  c(table_footer, "blue")
}
