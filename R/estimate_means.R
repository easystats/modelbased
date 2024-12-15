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
#' @examplesIf require("emmeans", quietly = TRUE)
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
#' @examplesIf require("see") && require("emmeans", quietly = TRUE)
#' plot(means) # which runs visualisation_recipe()
#'
#' standardize(means)
#'
#' @examplesIf require("lme4") && require("emmeans", quietly = TRUE)
#' \donttest{
#' data <- iris
#' data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#'
#' model <- lmer(Petal.Length ~ Sepal.Width + Species + (1 | Petal.Length_factor), data = data)
#' estimate_means(model)
#' estimate_means(model, by = "Sepal.Width", length = 3)
#' }
#' @return A data frame of estimated marginal means.
#' @export
estimate_means <- function(model,
                           by = "auto",
                           transform = "response",
                           ci = 0.95,
                           backend = "emmeans",
                           ...) {
  if (backend == "emmeans") {
    # Emmeans ------------------------------------------------------------------
    estimated <- get_emmeans(model, by, transform = transform, ...)
    means <- .format_emmeans_means(estimated, model, ci, transform, ...)
  } else {
    # Marginalmeans ------------------------------------------------------------
    estimated <- get_marginalmeans(model, by, transform = transform, ci = ci, ...)
    means <- .format_marginaleffects_means(estimated, model, transform, ...)
  }


  # Table formatting
  attr(means, "table_title") <- c("Estimated Marginal Means", "blue")
  attr(means, "table_footer") <- .estimate_means_footer(means, type = "means")

  # Add attributes
  attr(means, "model") <- model
  attr(means, "response") <- insight::find_response(model)
  attr(means, "ci") <- ci
  attr(means, "transform") <- transform

  attr(means, "coef_name") <- intersect(c("Mean", "Probability"), names(means))


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
