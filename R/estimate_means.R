#' Estimate Marginal Means (Model-based average at each factor level)
#'
#' Estimate average value of response variable at each factor levels. For
#' plotting, check the examples in [visualisation_recipe()]. See also
#' other related functions such as [estimate_contrasts()] and
#' [estimate_slopes()].
#'
#' @inheritParams get_emmeans
#' @inheritParams parameters::model_parameters.default
#' @param backend Whether to use 'emmeans' or 'marginaleffects' as a backend.
#'  The latter is experimental and some features might not work.
#' @inherit estimate_slopes details
#'
#' @examples
#' library(modelbased)
#' if (require("emmeans")) {
#'
#' # Frequentist models
#' # -------------------
#' model <- lm(Petal.Length ~ Sepal.Width * Species, data = iris)
#'
#' estimate_means(model)
#' estimate_means(model, fixed = "Sepal.Width")
#' estimate_means(model, at = c("Species", "Sepal.Width"), length = 2)
#' estimate_means(model, at = "Species=c('versicolor', 'setosa')")
#' estimate_means(model, at = "Sepal.Width=c(2, 4)")
#' estimate_means(model, at = c("Species", "Sepal.Width=0"))
#' estimate_means(model, at = "Sepal.Width", length = 5)
#' estimate_means(model, at = "Sepal.Width=c(2, 4)")
#'
#' # Methods that can be applied to it:
#' means <- estimate_means(model, fixed = "Sepal.Width")
#' if (require("see")) {
#'   plot(means) # which runs visualisation_recipe()
#' }
#' standardize(means)
#' \donttest{
#' if (require("lme4")) {
#'   data <- iris
#'   data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#'
#'   model <- lmer(Petal.Length ~ Sepal.Width + Species + (1 | Petal.Length_factor), data = data)
#'   estimate_means(model)
#'   estimate_means(model, at = "Sepal.Width", length = 3)
#' }
#' }
#' }
#' @return A dataframe of estimated marginal means.
#' @export
estimate_means <- function(model,
                           at = "auto",
                           fixed = NULL,
                           transform = "response",
                           ci = 0.95,
                           backend = "emmeans",
                           ...) {

  # Compute means
  if (backend == "emmeans") {
    estimated <- get_emmeans(model, at, fixed, transform = transform, ...)
  } else {
    estimated <- .get_marginalmeans(model, at, fixed, transform = transform, ...)
  }
  info <- attributes(estimated)

  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    means <- parameters::parameters(estimated, ci = ci, ...)
    means <- .clean_names_bayesian(means, model, transform, type = "mean")
    means <- cbind(estimated@grid, means)
    means$`.wgt.` <- NULL # Drop the weight column
  } else {
    means <- as.data.frame(stats::confint(estimated, level = ci))
    means$df <- NULL
    means <- .clean_names_frequentist(means)
  }
  # Remove the "1 - overall" column that can appear in cases like at = NULL
  means <- means[names(means) != "1"]

  # Restore factor levels
  means <- datawizard::data_restoretype(means, insight::get_data(model))


  # Table formatting
  attr(means, "table_title") <- c("Estimated Marginal Means", "blue")
  attr(means, "table_footer") <- .estimate_means_footer(means, info$at, type = "means")

  # Add attributes
  attr(means, "model") <- model
  attr(means, "response") <- insight::find_response(model)
  attr(means, "ci") <- ci
  attr(means, "transform") <- transform
  attr(means, "at") <- info$at
  attr(means, "fixed") <- info$fixed
  attr(means, "coef_name") <- intersect(c("Mean", "Probability"), names(means))


  # Output
  class(means) <- c("estimate_means", class(means))
  means
}




# Table Formating ----------------------------------------------------------


.estimate_means_footer <- function(x, at = NULL, type = "means", p_adjust = NULL) {
  table_footer <- paste("\nMarginal", type)

  # Levels
  if (!is.null(at) && length(at) > 0) {
    table_footer <- paste0(
      table_footer,
      " estimated at ",
      paste0(at, collapse = ", ")
    )
  }

  # P-value adjustment footer
  if (!is.null(p_adjust) && "p" %in% names(x)) {
    if (p_adjust == "none") {
      table_footer <- paste0(table_footer, "\np-values are uncorrected.")
    } else {
      table_footer <- paste0(table_footer, "\np-value adjustment method: ", parameters::format_p_adjust(p_adjust))
    }
  }

  if (table_footer == "") table_footer <- NULL
  c(table_footer, "blue")
}
