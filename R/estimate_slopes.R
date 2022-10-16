#' Estimate Marginal Effects
#'
#' Estimate the slopes (i.e., the coefficient) of a predictor over or within
#' different factor levels, or alongside a numeric variable . In other words, to
#' assess the effect of a predictor *at* specific configurations data. Other
#' related functions based on marginal estimations includes
#' [estimate_contrasts()] and [estimate_means()].
#' \cr\cr
#'
#' See the **Details** section below, and don't forget to also check out the
#' [Vignettes](https://easystats.github.io/modelbased/articles/estimate_slopes.html)
#' and [README examples](https://easystats.github.io/modelbased/index.html#features) for
#' various examples, tutorials and use cases.
#'
#' @inheritParams get_emmeans
#' @inheritParams estimate_means
#'
#' @details
#'
#' The [estimate_slopes()], [estimate_means()] and [estimate_contrasts()]
#' functions are forming a group, as they are all based on *marginal*
#' estimations (estimations based on a model). All three are also built on the
#' \pkg{emmeans} package, so reading its documentation (for instance for
#' [emmeans::emmeans()] and [emmeans::emtrends()]) is recommended to understand
#' the idea behind these types of procedures.
#'
#' - Model-based **predictions** is the basis for all that follows. Indeed,
#' the first thing to understand is how models can be used to make predictions
#' (see [estimate_link()]). This corresponds to the predicted response (or
#' "outcome variable") given specific predictor values of the predictors (i.e.,
#' given a specific data configuration). This is why the concept of [`reference
#' grid()`][visualisation_matrix] is so important for direct predictions.
#'
#' - **Marginal "means"**, obtained via [estimate_means()], are an extension
#' of such predictions, allowing to "average" (collapse) some of the predictors,
#' to obtain the average response value at a specific predictors configuration.
#' This is typically used when some of the predictors of interest are factors.
#' Indeed, the parameters of the model will usually give you the intercept value
#' and then the "effect" of each factor level (how different it is from the
#' intercept). Marginal means can be used to directly give you the mean value of
#' the response variable at all the levels of a factor. Moreover, it can also be
#' used to control, or average over predictors, which is useful in the case of
#' multiple predictors with or without interactions.
#'
#' - **Marginal contrasts**, obtained via [estimate_contrasts()], are
#' themselves at extension of marginal means, in that they allow to investigate
#' the difference (i.e., the contrast) between the marginal means. This is,
#' again, often used to get all pairwise differences between all levels of a
#' factor. It works also for continuous predictors, for instance one could also
#' be interested in whether the difference at two extremes of a continuous
#' predictor is significant.
#'
#' - Finally, **marginal effects**, obtained via [estimate_slopes()], are
#' different in that their focus is not values on the response variable, but the
#' model's parameters. The idea is to assess the effect of a predictor at a
#' specific configuration of the other predictors. This is relevant in the case
#' of interactions or non-linear relationships, when the effect of a predictor
#' variable changes depending on the other predictors. Moreover, these effects
#' can also be "averaged" over other predictors, to get for instance the
#' "general trend" of a predictor over different factor levels.
#'
#' **Example:** Let's imagine the following model `lm(y ~ condition * x)` where
#' `condition` is a factor with 3 levels A, B and C and `x` a continuous
#' variable (like age for example). One idea is to see how this model performs,
#' and compare the actual response y to the one predicted by the model (using
#' [estimate_response()]). Another idea is evaluate the average mean at each of
#' the condition's levels (using [estimate_means()]), which can be useful to
#' visualize them. Another possibility is to evaluate the difference between
#' these levels (using [estimate_contrasts()]). Finally, one could also estimate
#' the effect of x averaged over all conditions, or instead within each
#' condition (`using [estimate_slopes]`).
#'
#' @examples
#' if (require("emmeans")) {
#'   # Get an idea of the data
#'   if (require("ggplot2")) {
#'     ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
#'       geom_point(aes(color = Species)) +
#'       geom_smooth(color = "black", se = FALSE) +
#'       geom_smooth(aes(color = Species), linetype = "dotted", se = FALSE) +
#'       geom_smooth(aes(color = Species), method = "lm", se = FALSE)
#'   }
#'
#'   # Model it
#'   model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'   # Compute the marginal effect of Petal.Length at each level of Species
#'   slopes <- estimate_slopes(model, trend = "Petal.Length", at = "Species")
#'   slopes
#'   if (require("see")) {
#'     plot(slopes)
#'   }
#'   standardize(slopes)
#'
#'   \dontrun{
#'   # TODO: fails with latest emmeans (1.8.0)
#'   if (require("mgcv") && require("see")) {
#'     model <- mgcv::gam(Sepal.Width ~ s(Petal.Length), data = iris)
#'     slopes <- estimate_slopes(model, at = "Petal.Length", length = 50)
#'     summary(slopes)
#'     plot(slopes)
#'
#'     model <- mgcv::gam(Sepal.Width ~ s(Petal.Length, by = Species), data = iris)
#'     slopes <- estimate_slopes(model,
#'       trend = "Petal.Length",
#'       at = c("Petal.Length", "Species"), length = 20
#'     )
#'     summary(slopes)
#'     plot(slopes)
#'   }}
#' }
#' @return A data.frame of class `estimate_slopes`.
#' @export
estimate_slopes <- function(model,
                            trend = NULL,
                            at = NULL,
                            ci = 0.95,
                            ...) {
  # Sanitize arguments
  estimated <- get_emtrends(model, trend, at, ...)
  info <- attributes(estimated)

  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    trends <- parameters::parameters(estimated, ci = ci, ...)
    trends <- .clean_names_bayesian(trends, model, transform = "none", type = "trend")
    trends <- cbind(estimated@grid, trends)
    trends$`.wgt.` <- NULL # Drop the weight column
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
  class(trends) <- c("estimate_slopes_summary", "estimate_slopes", class(trends))
  trends
}

# Engine ===============================================================


# Summary Method ===============================================================


#' @export
summary.estimate_slopes <- function(object, ...) {
  data <- as.data.frame(object)
  trend <- attributes(object)$trend

  # Add "Confidence" col based on the sig index present in the data
  data$Confidence <- .estimate_slopes_sig(data, ...)

  # Grouping variables
  vars <- attributes(object)$at
  vars <- vars[!vars %in% trend]

  # If no grouping variables, summarize all
  if (length(vars) == 0) {
    out <- .estimate_slopes_summarize(data, trend = trend)
  } else {
    out <- data.frame()
    # Create vizmatrix of grouping variables
    groups <- as.data.frame(insight::get_datagrid(data[vars], factors = "all", numerics = "all"))
    # Summarize all of the chunks
    for (i in seq_len(nrow(groups))) {
      g <- datawizard::data_match(data, groups[i, , drop = FALSE])
      out <- rbind(out, .estimate_slopes_summarize(g, trend = trend))
    }
    out <- datawizard::data_relocate(out, vars)
  }

  # Clean and sanitize
  out$Confidence <- NULL # Drop significance col
  attributes(out) <- utils::modifyList(attributes(object), attributes(out))
  class(out) <- c("estimate_slopes", class(out))
  attr(out, "table_title") <- c("Average Marginal Effects", "blue")
  out
}



# Utilities ---------------------------------------------------------------
.estimate_slopes_summarize <- function(data, trend, ...) {
  # Find beginnings and ends -----------------------
  # First row - starting point
  signs <- sign(data[[datawizard::data_find(data, c("Coefficient", "Median", "Mean", "MAP_Estimate"), verbose = FALSE)]])
  sign <- signs[1]
  sig <- data$Confidence[1]
  starts <- 1
  ends <- nrow(data)
  # Iterate through all rows to find blocks
  for (i in 2:nrow(data)) {
    if ((data$Confidence[i] != sig) || ((signs[i] != sign) && data$Confidence[i] == "Uncertain")) {
      sign <- signs[i]
      sig <- data$Confidence[i]
      starts <- c(starts, i)
      ends <- c(ends, i - 1)
    }
  }
  ends <- sort(ends)

  # Summarize these groups -----------------------
  out <- data.frame()
  for (g in seq_len(length(starts))) {
    dat <- data[starts[g]:ends[g], ]
    dat <- as.data.frame(insight::get_datagrid(dat, at = NULL, factors = "mode"))
    dat <- cbind(data.frame("Start" = data[starts[g], trend], "End" = data[ends[g], trend]), dat)
    out <- rbind(out, dat)
  }
  out
}



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
