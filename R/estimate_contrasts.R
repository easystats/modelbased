#' Estimate Marginal Contrasts
#'
#' Run a contrast analysis by estimating the differences between each level of a
#' factor. See also other related functions such as [estimate_means()]
#' and [estimate_slopes()].
#'
#' @param contrast A character vector indicating the name of the variable(s)
#' for which to compute the contrasts.
#' @param p_adjust The p-values adjustment method for frequentist multiple
#' comparisons. Can be one of `"holm"` (default), `"hochberg"`, `"hommel"`,
#' `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"tukey"` or `"none"`. See the
#' p-value adjustment section in the `emmeans::test` documentation or
#' `?stats::p.adjust`.
#' @param comparison Specify the type of contrasts or tests that should be
#' carried out. When `backend = "emmeans"`, see `method` argument in
#' [emmeans::contrast]. For `backend = "marginaleffects"`, see
#' [this website](https://marginaleffects.com/bonus/hypothesis.html) for the
#' different comparison options.
#' @inheritParams estimate_means
#'
#' @inherit estimate_slopes details
#'
#' @examplesIf all(insight::check_if_installed(c("lme4", "emmeans", "rstanarm"), quietly = TRUE))
#' \dontrun{
#' # Basic usage
#' model <- lm(Sepal.Width ~ Species, data = iris)
#' estimate_contrasts(model)
#'
#' # Dealing with interactions
#' model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
#'
#' # By default: selects first factor
#' estimate_contrasts(model)
#'
#' # Can also run contrasts between points of numeric
#' estimate_contrasts(model, contrast = "Petal.Width", length = 4)
#'
#' # Or both
#' estimate_contrasts(model, contrast = c("Species", "Petal.Width"), length = 2)
#'
#' # Or with custom specifications
#' estimate_contrasts(model, contrast = c("Species", "Petal.Width=c(1, 2)"))
#'
#' # Or modulate it
#' estimate_contrasts(model, by = "Petal.Width", length = 4)
#'
#' # Standardized differences
#' estimated <- estimate_contrasts(lm(Sepal.Width ~ Species, data = iris))
#' standardize(estimated)
#'
#' # Other models (mixed, Bayesian, ...)
#' data <- iris
#' data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#'
#' model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)
#' estimate_contrasts(model)
#'
#' data <- mtcars
#' data$cyl <- as.factor(data$cyl)
#' data$am <- as.factor(data$am)
#'
#' model <- rstanarm::stan_glm(mpg ~ cyl * am, data = data, refresh = 0)
#' estimate_contrasts(model)
#' # fix `am` at value 1
#' estimate_contrasts(model, by = c("cyl", "am='1'"))
#'
#' model <- rstanarm::stan_glm(mpg ~ cyl * wt, data = data, refresh = 0)
#' estimate_contrasts(model)
#' estimate_contrasts(model, by = "wt", length = 4)
#'
#' model <- rstanarm::stan_glm(
#'   Sepal.Width ~ Species + Petal.Width + Petal.Length,
#'   data = iris,
#'   refresh = 0
#' )
#' estimate_contrasts(model, by = "Petal.Length", test = "bf")
#' }
#'
#' @return A data frame of estimated contrasts.
#' @export
estimate_contrasts <- function(model,
                               contrast = NULL,
                               by = NULL,
                               predict = NULL,
                               ci = 0.95,
                               p_adjust = "holm",
                               comparison = "pairwise",
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
    estimated <- get_emcontrasts(model,
      contrast = contrast,
      by = by,
      predict = predict,
      comparison = comparison,
      adjust = p_adjust,
      ...
    )
    out <- .format_emmeans_contrasts(model, estimated, ci, p_adjust, ...)
  } else {
    # Marginalmeans ------------------------------------------------------------
    estimated <- get_marginalcontrasts(model,
      contrast = contrast,
      by = by,
      predict = predict,
      comparison = comparison,
      p_adjust = p_adjust,
      ci = ci,
      ...
    )
    out <- format(estimated, model, p_adjust, comparison, ...)
  }

  info <- attributes(estimated)

  # Table formatting
  attr(out, "table_title") <- c("Marginal Contrasts Analysis", "blue")
  attr(out, "table_footer") <- .estimate_means_footer(
    out,
    info$contrast,
    type = "contrasts",
    p_adjust = p_adjust,
    predict = attributes(estimated)$predict,
    model_info = insight::model_info(model)
  )

  # Add attributes
  attr(out, "model") <- model
  attr(out, "response") <- insight::find_response(model)
  attr(out, "ci") <- ci
  attr(out, "transform") <- predict
  attr(out, "at") <- info$by
  attr(out, "by") <- info$by
  attr(out, "contrast") <- info$contrast
  attr(out, "p_adjust") <- p_adjust
  attr(out, "backend") <- backend

  # Output
  class(out) <- c("estimate_contrasts", "see_estimate_contrasts", class(out))
  out
}
