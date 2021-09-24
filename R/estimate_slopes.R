#' Estimate Marginal Effects
#'
#' Estimate the slopes (i.e., the coefficient) of a predictor over or within different
#' factor levels, or alongside a numeric variable . In other words, to assess the effect of a predictor *at* specific configurations data. Other related
#' functions based on marginal estimations includes [estimate_contrasts()] and
#' [estimate_means()].
#' \cr\cr
#' See the **Details** section below, and don't forget to also check out the [Vignettes](https://easystats.github.io/modelbased/articles/estimate_slopes.html) and [README examples](https://easystats.github.io/modelbased/index.html#features) for various examples, tutorials and usecases.
#'
#' @inheritParams model_emmeans
#' @inheritParams estimate_means
#'
#' @details The [estimate_slopes()], [estimate_means()] and [estimate_contrasts()] functions are forming a group, as they are all based on *marginal* estimations (estimations about the model based on a model). All three are also built on the \pkg{emmeans} package, so reading its documentation (for instance for [emmeans::emmeans()] and [emmeans::emtrends()]) is advised to understand the idea behind these types of procedures.
#'
#' \itemize{
#' \item Model-based **predictions** is the basis for all that follows. Indeed, the first thing to understand is how models can be used to make predictions (see [estimate_link()]). This corresponds to the predicted response (or "outcome variable") given specific predictor values of the predictors (i.e., given a specific data configuration). This is why the concept of [`reference grid()`][visualisation_matrix] is so important for direct predictions.
#' \item **Marginal "means"**, obtained via [estimate_means()], are an extension of such predictions, allowing to "average" (collapse) some of the predictors, to obtain the average response value at a specific predictors configuration. This is typically used when some of the predictors of interest are factors. Indeed, the parameters of the model will usually give you the intercept value and then the "effect" of each factor level (how different it is from the intercept). Marginal means can be used to directly give you the mean value of the response variable at all the levels of a factor. Moreover, it can also be used to control, or average over predictors, which is useful in the case of multiple predictors with or without interactions.
#' \item **Marginal contrasts**, obtained via [estimate_contrasts()], are themselves at extension of marginal means, in that they allow to investigate the difference (i.e., the contrast) between the marginal means. This is, again, often used to get all pairwise differences between all levels of a factor. It works also for continuous predictors, for instance one could also be interested in whether the difference at two extremes of a continuous predictor is significant.
#' \item Finally, **marginal effects**, obtained via [estimate_slopes()], are different in that their focus is not values on the response variable, but the model's parameters. The idea is to assess the effect of a predictor at a specific configuration of the other predictors. This is relevant in the case of interactions or non-linear relationships, when the effect of a predictor variable changes depending on the other predictors. Moreover, these effects can also be "averaged" over other predictors, to get for instance the "general trend" of a predictor over different factor levels.
#' }
#' **Example:** let's imagine the following model `lm(y ~ condition * x)` where `condition` is a factor with 3 levels A, B and C and `x` a continuous variable (like age for example). One idea is to see how this model performs, and compare the actual response y to the one predicted by the model (using [estimate_response()]). Another idea is evaluate the average mean at each of the condition's levels (using [estimate_means()]), which can be useful to visualize them. Another possibility is to evaluate the difference between these levels (using [estimate_contrasts()]). Finally, one could also estimate the effect of x averaged over all conditions, or instead within each condition (`using [estimate_slopes]`).
#'
#'
#' @examples
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' slopes <- estimate_slopes(model, trend = "Petal.Length", at = "Species")
#' slopes
#' plot(slopes)
#' effectsize::standardize(slopes)
#'
#' if (require("mgcv")) {
#'   model <- mgcv::gam(Sepal.Width ~ s(Petal.Length), data = iris)
#'   slopes <- estimate_slopes(model, at = "Petal.Length", length = 50)
#'   summary(slopes)
#'   plot(slopes)
#' }
#' @return A data.frame of class `estimate_slopes`.
#' @export
estimate_slopes <- function(model,
                            trend = NULL,
                            at = NULL,
                            ci = 0.95,
                            ...) {

  # Sanitize arguments
  estimated <- model_emtrends(model, trend, at, ...)
  info <- attributes(estimated)

  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    trends <- bayestestR::describe_posterior(estimated, ci = ci, ...)
    trends <- cbind(estimated@grid, trends)
    trends$`.wgt.` <- NULL # Drop the weight column
    trends <- .clean_names_bayesian(trends, model, transform = "none", type = "trend")
    trends <- datawizard::data_relocate(trends, c("CI_low", "CI_high"), after = "Coefficient")
  } else {
    trends <- parameters::parameters(estimated, ci = ci, ...)
  }
  # Remove the "1 - overall" column that can appear in cases like y ~ x
  trends <- trends[names(trends) != "1"]

  # Restore factor levels
  trends <- datawizard::data_restoretype(trends, insight::get_data(model))

  # Table formatting
  attr(trends, "table_title") <- c("Estimated Marginal Effects", "blue")
  attr(trends, "table_footer") <- c(paste("Marginal effects estimated for", info$trend), "blue")

  # Add attributes
  attr(trends, "model") <- model
  attr(trends, "response") <- insight::find_response(model)
  attr(trends, "ci") <- ci
  attr(trends, "trend") <- info$trend
  attr(trends, "at") <- info$at

  # Output
  class(trends) <- c("estimate_slopes", class(trends))
  trends
}

# Alias -------------------------------------------------------------------

#' @export
#' @rdname estimate_slopes
est_slopes <- estimate_slopes

# Summary Method ----------------------------------------------------------


#' @export
summary.estimate_slopes <- function(object, ...) {
  object$Confidence <- .estimate_slopes_sig(object, ...)

  vars <- c(attributes(object)$levels, attributes(object)$modulate)

  # TODO: deal with factors (group by levels)

  # Loop through groups of "significance"
  groups <- list()
  group <- object[1, ] # First row
  for (i in 2:nrow(object)) {
    if (object$Confidence[i] == object$Confidence[i - 1]) {
      group <- rbind(group, object[i, ])
    } else {
      groups[[length(groups) + 1]] <- group # Store current group
      group <- object[i, ] # reset
    }
  }
  groups[[length(groups) + 1]] <- group # Store last one

  # Summarize
  groups <- lapply(groups, function(x) {
    out <- data.frame(Confidence = unique(x$Confidence))

    for (var in vars) {
      if (is.numeric(object[[var]])) {
        out[[paste0(var, "_Min")]] <- min(x[[var]], na.rm = TRUE)
        out[[paste0(var, "_Max")]] <- max(x[[var]], na.rm = TRUE)
      } else {
        out[[var]] <- paste0(unique(x[[var]]), collapse = ", ")
      }
    }
    if ("Coefficient" %in% names(object)) out$Coefficient_Mean <- mean(x$Coefficient, na.rm = TRUE)
    if ("SE" %in% names(object)) out$SE_Mean <- mean(x$SE, na.rm = TRUE)
    if ("SD" %in% names(object)) out$SD_Mean <- mean(x$SD, na.rm = TRUE)
    if ("MAD" %in% names(object)) out$MAD_Mean <- mean(x$MAD, na.rm = TRUE)
    out
  })

  groups <- do.call(rbind, groups)
  groups
}



# Utilities ---------------------------------------------------------------

.estimate_slopes_sig <- function(x, confidence = "auto", ...) {
  if (confidence == "auto") {
    # TODO: make sure all of these work
    if ("BF" %in% names(x)) confidence <- "BF"
    if ("p" %in% names(x)) confidence <- "p"
    if ("pd" %in% names(x)) confidence <- "pd"
  }



  if (confidence == "p") {
    sig <- tools::toTitleCase(effectsize::interpret_p(x$p, ...))
  } else if (confidence == "BF") {
    sig <- tools::toTitleCase(effectsize::interpret_bf(x$BF, ...))
  } else if (confidence == "pd") {
    sig <- tools::toTitleCase(effectsize::interpret_pd(x$pd, ...))
  } else {
    # Based on CI
    sig <- ifelse((x$CI_high < 0 & x$CI_low < 0) | (x$CI_high > 0 & x$CI_low > 0), "Significant", "Uncertain")
    sig <- factor(sig, levels = c("Uncertain", "Significant"))
  }
  sig
}
