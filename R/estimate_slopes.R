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
#' @param trend A character indicating the name of the variable for which to
#' compute the slopes.
#' @inheritParams estimate_means
#' @inheritParams parameters::model_parameters.default
#'
#' @details
#' The [estimate_slopes()], [estimate_means()] and [estimate_contrasts()]
#' functions are forming a group, as they are all based on *marginal*
#' estimations (estimations based on a model). All three are built on the
#' **emmeans** or **marginaleffects** package (depending on the `backend`
#' argument), so reading its documentation (for instance [emmeans::emmeans()],
#' [emmeans::emtrends()] or this [website](https://marginaleffects.com/)) is
#' recommended to understand the idea behind these types of procedures.
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
#' [estimate_expectation()]). Another idea is evaluate the average mean at each of
#' the condition's levels (using [estimate_means()]), which can be useful to
#' visualize them. Another possibility is to evaluate the difference between
#' these levels (using [estimate_contrasts()]). Finally, one could also estimate
#' the effect of x averaged over all conditions, or instead within each
#' condition (`using [estimate_slopes]`).
#'
#' @return A data.frame of class `estimate_slopes`.
#'
#' @examplesIf all(insight::check_if_installed(c("emmeans", "mgcv", "ggplot2", "see"), quietly = TRUE))
#' library(ggplot2)
#' # Get an idea of the data
#' ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
#'   geom_point(aes(color = Species)) +
#'   geom_smooth(color = "black", se = FALSE) +
#'   geom_smooth(aes(color = Species), linetype = "dotted", se = FALSE) +
#'   geom_smooth(aes(color = Species), method = "lm", se = FALSE)
#'
#' # Model it
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' # Compute the marginal effect of Petal.Length at each level of Species
#' slopes <- estimate_slopes(model, trend = "Petal.Length", by = "Species")
#' slopes
#'
#' # Plot it
#' plot(slopes)
#' standardize(slopes)
#'
#' model <- mgcv::gam(Sepal.Width ~ s(Petal.Length), data = iris)
#' slopes <- estimate_slopes(model, by = "Petal.Length", length = 50)
#' summary(slopes)
#' plot(slopes)
#'
#' model <- mgcv::gam(Sepal.Width ~ s(Petal.Length, by = Species), data = iris)
#' slopes <- estimate_slopes(model,
#'   trend = "Petal.Length",
#'   by = c("Petal.Length", "Species"), length = 20
#' )
#' summary(slopes)
#' plot(slopes)
#' @export
estimate_slopes <- function(model,
                            trend = NULL,
                            by = NULL,
                            ci = 0.95,
                            backend = getOption("modelbased_backend", "emmeans"),
                            ...) {

  if (backend == "emmeans") {
    # Emmeans ------------------------------------------------------------------
    estimated <- get_emtrends(model, trend, by, ...)
    info <- attributes(estimated)
    trends <- .format_emmeans_slopes(model, estimated, ci, ...)
  } else {
    trends <- get_marginaltrends(model, trend, by, ...)
    ## TODO: needs to be fixed
    info <- list(by = by, trend = trend)
  }

  # Table formatting
  attr(trends, "table_title") <- c("Estimated Marginal Effects", "blue")
  attr(trends, "table_footer") <- c(paste("Marginal effects estimated for", info$trend), "blue")

  # Add attributes
  attr(trends, "model") <- model
  attr(trends, "response") <- insight::find_response(model)
  attr(trends, "ci") <- ci
  attr(trends, "trend") <- info$trend
  attr(trends, "at") <- info$by
  attr(trends, "by") <- info$by

  # Output
  class(trends) <- c("estimate_slopes_summary", "estimate_slopes", class(trends))
  trends
}


# Engine ===============================================================


.format_emmeans_slopes <- function(model, estimated, ci, ...) {
  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    trends <- parameters::parameters(estimated, ci = ci, ...)
    trends <- .clean_names_bayesian(trends, model, predict = "none", type = "trend")
    em_grid <- as.data.frame(estimated@grid)
    em_grid[[".wgt."]] <- NULL # Drop the weight column
    colums_to_add <- setdiff(colnames(em_grid), colnames(trends))
    if (length(colums_to_add)) {
      trends <- cbind(em_grid[colums_to_add], trends)
    }
  } else {
    trends <- parameters::parameters(estimated, ci = ci, ...)
  }
  # Remove the "1 - overall" column that can appear in cases like y ~ x
  trends <- trends[names(trends) != "1"]

  # Restore factor levels
  datawizard::data_restoretype(trends, insight::get_data(model, verbose = FALSE))
}


# Summary Method ===============================================================


#' @export
summary.estimate_slopes <- function(object, ...) {
  my_data <- as.data.frame(object)
  trend <- attributes(object)$trend

  # Add "Confidence" col based on the sig index present in the data
  my_data$Confidence <- .estimate_slopes_sig(my_data, ...)

  # Grouping variables
  vars <- attributes(object)$at
  vars <- vars[!vars %in% trend]

  # If no grouping variables, summarize all
  if (length(vars) == 0) {
    out <- .estimate_slopes_summarize(my_data, trend = trend)
  } else {
    out <- data.frame()
    # Create vizmatrix of grouping variables
    groups <- as.data.frame(insight::get_datagrid(my_data[vars], factors = "all", numerics = "all"))
    # Summarize all of the chunks
    for (i in seq_len(nrow(groups))) {
      g <- datawizard::data_match(my_data, groups[i, , drop = FALSE])
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
  centrality_columns <- datawizard::extract_column_names(
    data,
    c("Coefficient", "Median", "Mean", "MAP_Estimate"),
    verbose = FALSE
  )
  centrality_signs <- sign(data[[centrality_columns]])
  centrality_sign <- centrality_signs[1]
  sig <- data$Confidence[1]
  starts <- 1
  ends <- nrow(data)
  # Iterate through all rows to find blocks
  for (i in 2:nrow(data)) {
    if ((data$Confidence[i] != sig) || ((centrality_signs[i] != centrality_sign) && data$Confidence[i] == "Uncertain")) {
      centrality_sign <- centrality_signs[i]
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
    dat <- as.data.frame(insight::get_datagrid(dat, by = NULL, factors = "mode"))
    dat <- cbind(data.frame(Start = data[starts[g], trend], End = data[ends[g], trend]), dat)
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

  switch(confidence,
    p = tools::toTitleCase(effectsize::interpret_p(x$p, ...)),
    BF = tools::toTitleCase(effectsize::interpret_bf(x$BF, ...)),
    pd = tools::toTitleCase(effectsize::interpret_pd(x$pd, ...)),
    {
      # Based on CI
      out <- ifelse((x$CI_high < 0 & x$CI_low < 0) | (x$CI_high > 0 & x$CI_low > 0), "Significant", "Uncertain")
      factor(out, levels = c("Uncertain", "Significant"))
    }
  )
}
