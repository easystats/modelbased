#' Estimate Marginal Means (Model-based average at each factor level)
#'
#' Estimate average value of response variable at each factor levels. For
#' plotting, check the examples in \code{\link{visualisation_recipe}}. See also
#' other related functions such as \code{\link{estimate_contrasts}} and
#' \code{\link{estimate_slopes}}.
#'
#' @inheritParams model_emmeans
#' @inheritParams parameters::model_parameters.default
#'
#' @examples
#' library(modelbased)
#'
#' # Frequentist models
#' # -------------------
#' model <- lm(Petal.Length ~ Sepal.Width * Species, data = iris)
#'
#' estimate_means(model)
#' estimate_means(model, fixed = "Sepal.Width")
#' estimate_means(model, levels = c("Species", "Sepal.Width"), length = 2)
#' estimate_means(model, levels = "Species=c('versicolor', 'setosa')")
#' estimate_means(model, levels = "Sepal.Width=c(2, 4)")
#' estimate_means(model, levels = c("Species", "Sepal.Width=0"))
#' estimate_means(model, modulate = "Sepal.Width", length = 5)
#' estimate_means(model, modulate = "Sepal.Width=c(2, 4)")
#'
#' # Methods that can be applied to it:
#' means <- estimate_means(model, fixed = "Sepal.Width")
#' plot(means) # which runs visualisation_recipe()
#' effectsize::standardize(means)
#' \donttest{
#' if (require("lme4")) {
#'   data <- iris
#'   data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#'
#'   model <- lmer(Petal.Length ~ Sepal.Width + Species + (1 | Petal.Length_factor), data = data)
#'   estimate_means(model)
#'   estimate_means(model, modulate = "Sepal.Width", length = 3)
#' }
#'
#' # Bayesian models
#' # -------------------
#' data <- mtcars
#' data$cyl <- as.factor(data$cyl)
#' data$am <- as.factor(data$am)
#'
#' if (require("rstanarm")) {
#'   model <- stan_glm(mpg ~ cyl * am, data = data, refresh = 0)
#'   estimate_means(model)
#'
#'   model <- stan_glm(mpg ~ cyl * wt, data = data, refresh = 0)
#'   estimate_means(model)
#'   estimate_means(model, modulate = "wt")
#'   estimate_means(model, fixed = "wt")
#' }
#' }
#'
#' \dontrun{
#' if (require("brms")) {
#'   model <- brm(mpg ~ cyl * am, data = data, refresh = 0)
#'   estimate_means(model)
#' }
#' }
#'
#' @return A dataframe of estimated marginal means.
#' @export
estimate_means <- function(model,
                           levels = NULL,
                           fixed = NULL,
                           modulate = NULL,
                           transform = "response",
                           ci = 0.95,
                           ...) {

  # Run emmeans
  estimated <- model_emmeans(model, levels, fixed, modulate, transform = transform, ...)
  args <- attributes(estimated)$args

  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    means <- bayestestR::describe_posterior(estimated,
      test = NULL,
      rope_range = NULL,
      ci = ci,
      ...
    )
    means <- cbind(estimated@grid, means)
    means$`.wgt.` <- NULL # Drop the weight column
    means <- .clean_names_bayesian(means, model, transform, type = "mean")
  } else {
    means <- as.data.frame(stats::confint(estimated, level = ci))
    means$df <- NULL
    means <- .clean_names_frequentist(means)
  }

  # Restore factor levels
  means <- datawizard::data_restoretype(means, insight::get_data(model))


  # Table formatting
  attr(means, "table_title") <- c("Estimated Marginal Means", "blue")
  attr(means, "table_footer") <- .estimate_means_footer(means, args, type = "means")

  # Add attributes
  attr(means, "model") <- model
  attr(means, "response") <- insight::find_response(model)
  attr(means, "ci") <- ci
  attr(means, "transform") <- transform
  attr(means, "levels") <- args$levels
  attr(means, "fixed") <- args$fixed
  attr(means, "modulate") <- args$modulate
  attr(means, "coef_name") <- intersect(c("Mean", "Probability"), names(means))


  # Output
  class(means) <- c("estimate_means", class(means))
  means
}


# Clean names -------------------------------------------------------------


#' @keywords internal
.clean_names_frequentist <- function(means) {
  names(means)[names(means) == "emmean"] <- "Mean"
  names(means)[names(means) == "response"] <- "Mean"
  names(means)[names(means) == "prob"] <- "Probability"
  names(means)[names(means) == "estimate"] <- "Difference"
  names(means)[names(means) == "odds.ratio"] <- "Odds_ratio"
  names(means)[names(means) == "ratio"] <- "Ratio"
  names(means)[names(means) == "t.ratio"] <- "t"
  names(means)[names(means) == "z.ratio"] <- "z"
  names(means)[names(means) == "p.value"] <- "p"
  names(means)[names(means) == "lower.CL"] <- "CI_low"
  names(means)[names(means) == "upper.CL"] <- "CI_high"
  names(means)[names(means) == "asymp.LCL"] <- "CI_low"
  names(means)[names(means) == "asymp.UCL"] <- "CI_high"
  means
}




#' @keywords internal
.clean_names_bayesian <- function(means, model, transform, type = "mean") {
  vars <- names(means)[names(means) %in% c("Median", "Mean", "MAP")]
  if (length(vars) == 1) {
    if (type == "contrast") {
      if (insight::model_info(model)$is_logit & transform == "response") {
        names(means)[names(means) == vars] <- "Odds_ratio"
      } else if (insight::model_info(model)$is_poisson & transform == "response") {
        names(means)[names(means) == vars] <- "Ratio"
      } else {
        names(means)[names(means) == vars] <- "Difference"
      }
    } else if (type == "mean") {
      if (insight::model_info(model)$is_logit & transform == "response") {
        names(means)[names(means) == vars] <- "Probability"
      } else {
        names(means)[names(means) == vars] <- "Mean"
      }
    } else {
      names(means)[names(means) == vars] <- "Coefficient"
    }
  }
  means$CI <- NULL
  means$ROPE_CI <- NULL
  means$ROPE_low <- NULL
  means$ROPE_high <- NULL
  means$Parameter <- NULL
  means
}


# Table Formating ----------------------------------------------------------


.estimate_means_footer <- function(x, args = NULL, type = "means", adjust = NULL) {
  table_footer <- ""

  # Levels
  if (length(args$levels) > 0) {
    table_footer <- paste0(
      table_footer,
      "\nMarginal ",
      type,
      " estimated for ",
      paste0(args$levels, collapse = ", ")
    )
  }

  # P-value adjustment footer
  if (!is.null(adjust) && "p" %in% names(x)) {
    if (adjust == "none") {
      table_footer <- paste0(table_footer, "\np-values are uncorrected.")
    } else {
      table_footer <- paste0(table_footer, "\np-value adjustment method: ", parameters::format_p_adjust(adjust))
    }
  }

  if (table_footer == "") table_footer <- NULL
  c(table_footer, "blue")
}
