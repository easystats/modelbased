#' Estimate marginal means (Frequentist models)
#'
#' @inheritParams estimate_means.stanreg
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#'
#' @examples
#' library(estimate)
#'
#' model <- lm(Petal.Length ~ Sepal.Width + Species, data = iris)
#' estimate_means(model)
#' estimate_means(model, modulate = "Sepal.Width")
#'
#' \dontrun{
#' library(lme4)
#'
#' data <- iris
#' data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#'
#' model <- lmer(Petal.Length ~ Sepal.Width + Species + (1|Petal.Length_factor), data = data)
#' estimate_means(model)
#'
#' }
#'
#' @import emmeans
#' @importFrom graphics pairs
#' @importFrom stats confint
#' @export
estimate_means.lm <- function(model, levels = NULL, fixed = NULL, modulate = NULL, transform = "response", length = 10, ci = 0.95, ...) {

  estimated <- .emmeans_wrapper(model, levels = levels, fixed = fixed, modulate = modulate, transform, length = length, type = "mean", ...)

  # Summary
  means <- as.data.frame(confint(estimated$means, level = ci))
  if("df" %in% names(means)) means$df <- NULL
  names(means)[names(means) == "emmean"] <- "Mean"
  names(means)[names(means) == "lower.CL"] <- "CI_low"
  names(means)[names(means) == "upper.CL"] <- "CI_high"


  # Format means
  # levelcols <- strsplit(as.character(means$Parameter), ", ")
  # levelcols <- data.frame(do.call(rbind, levelcols))
  # names(levelcols) <- unlist(sapply(levelcols, .find_name_level))
  # if (nrow(levelcols) > 1) {
  #   levelcols <- as.data.frame(sapply(levelcols, .remove_name_level), stringsAsFactors = FALSE)
  #   levelcols <- as.data.frame(sapply(levelcols, as.numeric_ifnumeric), stringsAsFactors = FALSE)
  # } else {
  #   levelcols <- as.data.frame(t(sapply(levelcols, .remove_name_level)), stringsAsFactors = FALSE)
  #   levelcols <- as.data.frame(t(sapply(levelcols, as.numeric_ifnumeric)), stringsAsFactors = FALSE)
  # }
  # means$Parameter <- NULL
  # means <- cbind(levelcols, means)


  # Restore factor levels
  means <- .restore_factor_levels(means, insight::get_data(model))

  # Add attributes
  attributes(means) <- c(
    attributes(means),
    list(
      ci = ci,
      levels = estimated$levels,
      fixed = estimated$fixed,
      modulate = estimated$modulate,
      transform = transform
    )
  )

  class(means) <- c("estimate_means", class(means))
  means
}





#' @export
estimate_means.merMod <- estimate_means.lm