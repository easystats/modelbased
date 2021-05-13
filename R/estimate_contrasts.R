#' Estimate contrasts between factor levels
#'
#' Run a contrast analysis.
#'
#' @inheritParams estimate_means
#' @param adjust The p-values adjustment method for frequentist multiple comparisons. Can be
#'   one of "holm" (default), "tukey", "hochberg", "hommel", "bonferroni", "BH",
#'   "BY", "fdr" or "none". See the p-value adjustment section in the
#'   \code{emmeans::test} documentation.
#'
#' @examples
#' library(modelbased)
#'
#' model <- lm(Sepal.Width ~ Species, data = iris)
#' estimate_contrasts(model)
#'
#' model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
#' estimate_contrasts(model)
#' estimate_contrasts(model, fixed = "Petal.Width")
#' estimate_contrasts(model, modulate = "Petal.Width", length = 4)
#' estimate_contrasts(model, levels = "Petal.Width", length = 4)
#'
#' if (require("lme4")) {
#'   data <- iris
#'   data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#'
#'   model <- lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)
#'   estimate_contrasts(model)
#' }
#'
#' data <- mtcars
#' data$cyl <- as.factor(data$cyl)
#' data$am <- as.factor(data$am)
#'
#' \dontrun{
#' if (require("rstanarm")) {
#'   model <- stan_glm(mpg ~ cyl * am, data = data, refresh = 0)
#'   estimate_contrasts(model)
#'   estimate_contrasts(model, fixed = "am")
#'
#'   model <- stan_glm(mpg ~ cyl * wt, data = data, refresh = 0)
#'   estimate_contrasts(model)
#'   estimate_contrasts(model, fixed = "wt")
#'   estimate_contrasts(model, modulate = "wt", length = 4)
#'   estimate_contrasts(model, levels = "wt", length = 4)
#'
#'   model <- stan_glm(Sepal.Width ~ Species + Petal.Width + Petal.Length, data = iris, refresh = 0)
#'   estimate_contrasts(model, fixed = "Petal.Width", modulate = "Petal.Length", test = "bf")
#' }
#'
#' if (require("brms")) {
#'   model <- brm(mpg ~ cyl * am, data = data, refresh = 0)
#'   estimate_contrasts(model)
#' }
#' }
#'
#' @return A data frame of estimated contrasts.
#' @export
estimate_contrasts <- function(model,
                               levels = NULL,
                               fixed = NULL,
                               modulate = NULL,
                               transform = "none",
                               ci = 0.95,
                               adjust = "holm",
                               ...) {

  # check if available
  insight::check_if_installed("emmeans")

  # Sanitize arguments
  args <- .guess_arguments(model, levels = levels, fixed = fixed, modulate = modulate)

  # Run emmeans
  estimated <- model_emmeans(
    model,
    levels = args$levels,
    fixed = args$fixed,
    modulate = args$modulate,
    transform = transform,
    ...
  )



  # Compute pairwise contrasts
  estimated <- emmeans::contrast(estimated,
                                 by = c(.clean_argument(args$fixed), .clean_argument(args$modulate)),
                                 method = "pairwise",
                                 ...)

  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    contrasts <- bayestestR::describe_posterior(estimated, ci = ci, ...)
    contrasts <- cbind(estimated@grid, contrasts)
    contrasts <- .clean_names_bayesian(contrasts, model, transform, type = "contrast")
  } else {
    contrasts <- as.data.frame(merge(as.data.frame(estimated), stats::confint(estimated, level = ci, adjust = adjust)))
    contrasts <- .clean_names_frequentist(contrasts)
  }
  contrasts <- insight::data_relocate(contrasts, c("CI_low", "CI_high"), after = c("Difference", "Odds_ratio", "Ratio"))



  # Standardized differences
  # if (standardize & transform != "response") {
  #   contrasts <- cbind(contrasts, .standardize_contrasts(contrasts, model, robust = standardize_robust))
  # }


  # Format contrasts names
  # Split by either " - " or "/"
  level_cols <- strsplit(as.character(contrasts$contrast), " - |\\/")
  level_cols <- data.frame(do.call(rbind, lapply(level_cols, trimws)))
  names(level_cols) <- c("Level1", "Level2")
  level_cols$Level1 <- gsub(",", " - ", level_cols$Level1)
  level_cols$Level2 <- gsub(",", " - ", level_cols$Level2)

  # Merge levels and rest
  contrasts$contrast <- NULL
  contrasts <- cbind(level_cols, contrasts)


  # Table formatting
  attr(contrasts, "table_title") <- c("Marginal Contrasts Analysis", "blue")
  attr(contrasts, "table_footer") <- .estimate_add_footer(contrasts, args, type = "contrasts", adjust = adjust)

  # Add attributes
  attributes(contrasts) <- c(
    attributes(contrasts),
    list(
      levels = args$levels,
      fixed = args$fixed,
      modulate = args$modulate,
      transform = transform,
      ci = ci,
      adjust = adjust,
      response = insight::find_response(model)
    )
  )

  class(contrasts) <- c("estimate_contrasts", class(contrasts))
  contrasts
}