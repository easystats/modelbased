#' Estimate Marginal Contrasts
#'
#' Run a contrast analysis by estimating the differences between each level of a
#' factor. See also other related functions such as [estimate_means()]
#' and [estimate_slopes()].
#'
#' @inheritParams estimate_means
#' @inheritParams get_emcontrasts
#' @param p_adjust The p-values adjustment method for frequentist multiple
#'   comparisons. Can be one of "holm" (default), "tukey", "hochberg", "hommel",
#'   "bonferroni", "BH", "BY", "fdr" or "none". See the p-value adjustment
#'   section in the `emmeans::test` documentation.
#' @param adjust Deprecated in favour of `p_adjust`.
#'
#' @inherit estimate_slopes details
#'
#' @examplesIf require("lme4", quietly = TRUE) && require("emmeans", quietly = TRUE) && require("rstanarm", quietly = TRUE)
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
#' # Can fixate the numeric at a specific value
#' estimate_contrasts(model, fixed = "Petal.Width")
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
#' library(rstanarm)
#'
#' data <- mtcars
#' data$cyl <- as.factor(data$cyl)
#' data$am <- as.factor(data$am)
#'
#' model <- stan_glm(mpg ~ cyl * am, data = data, refresh = 0)
#' estimate_contrasts(model)
#' estimate_contrasts(model, fixed = "am")
#'
#' model <- stan_glm(mpg ~ cyl * wt, data = data, refresh = 0)
#' estimate_contrasts(model)
#' estimate_contrasts(model, fixed = "wt")
#' estimate_contrasts(model, by = "wt", length = 4)
#'
#' model <- stan_glm(Sepal.Width ~ Species + Petal.Width + Petal.Length, data = iris, refresh = 0)
#' estimate_contrasts(model, by = "Petal.Length", test = "bf")
#' }
#'
#' @return A data frame of estimated contrasts.
#' @export
estimate_contrasts <- function(model,
                               contrast = NULL,
                               by = NULL,
                               fixed = NULL,
                               transform = "none",
                               ci = 0.95,
                               p_adjust = "holm",
                               method = "pairwise",
                               adjust = NULL,
                               at = NULL,
                               ...) {
  if (!is.null(at)) {
    insight::format_warning("The `at` argument is deprecated and will be removed in the future. Please use `by` instead.") # nolint
    by <- at
  }

  # Deprecation
  if (!is.null(adjust)) {
    insight::format_warning("The `adjust` argument is deprecated. Please write `p_adjust` instead.")
    p_adjust <- adjust
  }

  # Run emmeans
  estimated <- get_emcontrasts(model,
    contrast = contrast,
    by = by,
    fixed = fixed,
    transform = transform,
    method = method,
    adjust = p_adjust,
    ...
  )

  info <- attributes(estimated)

  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    out <- cbind(estimated@grid, bayestestR::describe_posterior(estimated, ci = ci, verbose = FALSE, ...))
    out <- .clean_names_bayesian(out, model, transform, type = "contrast")
  } else {
    out <- as.data.frame(merge(
      as.data.frame(estimated),
      stats::confint(estimated, level = ci, adjust = p_adjust)
    ))
    out <- .clean_names_frequentist(out)
  }
  out$null <- NULL # introduced in emmeans 1.6.1 (#115)
  out <- datawizard::data_relocate(
    out,
    c("CI_low", "CI_high"),
    after = c("Difference", "Odds_ratio", "Ratio")
  )


  # Format contrasts names
  # Split by either " - " or "/"
  level_cols <- strsplit(as.character(out$contrast), " - |\\/")
  level_cols <- data.frame(do.call(rbind, lapply(level_cols, trimws)))
  names(level_cols) <- c("Level1", "Level2")
  level_cols$Level1 <- gsub(",", " - ", level_cols$Level1, fixed = TRUE)
  level_cols$Level2 <- gsub(",", " - ", level_cols$Level2, fixed = TRUE)

  # Merge levels and rest
  out$contrast <- NULL
  out <- cbind(level_cols, out)


  # Table formatting
  attr(out, "table_title") <- c("Marginal Contrasts Analysis", "blue")
  attr(out, "table_footer") <- .estimate_means_footer(
    out,
    info$contrast,
    type = "contrasts",
    p_adjust = p_adjust
  )

  # Add attributes
  attr(out, "model") <- model
  attr(out, "response") <- insight::find_response(model)
  attr(out, "ci") <- ci
  attr(out, "transform") <- transform
  attr(out, "at") <- info$by
  attr(out, "by") <- info$by
  attr(out, "fixed") <- info$fixed
  attr(out, "contrast") <- info$contrast
  attr(out, "p_adjust") <- p_adjust


  # Output
  class(out) <- c("estimate_contrasts", "see_estimate_contrasts", class(out))
  out
}
