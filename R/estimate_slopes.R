#' Estimate Marginal Effects
#'
#' Estimate the slopes (i.e., the coefficient) of a predictor over different
#' factor levels or alongside a numeric variable. See also other related
#' functions such as \code{\link{estimate_contrasts}} and
#' \code{\link{estimate_means}}.
#'
#' @inheritParams model_emtrends
#' @inheritParams estimate_means
#'
#' @examples
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' slopes <- estimate_slopes(model, trend = "Petal.Length", levels = "Species")
#' slopes
#' plot(slopes)
#' effectsize::standardize(slopes)
#'
#' if (require("mgcv")) {
#'   model <- mgcv::gam(Sepal.Width ~ s(Petal.Length), data = iris)
#'   slopes <- estimate_slopes(model, modulate = "Petal.Length", length = 30)
#'   summary(slopes)
#'   plot(slopes)
#' }
#' @return A data.frame.
#' @export
estimate_slopes <- function(model,
                            trend = NULL,
                            levels = NULL,
                            modulate = NULL,
                            ci = 0.95,
                            ...) {

  # Sanitize arguments
  estimated <- model_emtrends(model, trend, levels, modulate, ...)
  args <- attributes(estimated)$args

  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    trends <- bayestestR::describe_posterior(estimated, ci = ci, ...)
    trends <- cbind(estimated@grid, trends)
    trends$`.wgt.` <- NULL # Drop the weight column
    trends <- .clean_names_bayesian(trends, model, transform = "none", type = "trend")
    trends <- insight::data_relocate(trends, c("CI_low", "CI_high"), after = "Coefficient")
  } else {
    trends <- parameters::parameters(estimated, ci = ci, ...)
  }

  # Restore factor levels
  trends <- insight::data_restoretype(trends, insight::get_data(model))

  # Table formatting
  attr(trends, "table_title") <- c("Estimated Marginal Effects", "blue")
  attr(trends, "table_footer") <- c(paste("Marginal effects estimated for", args$trend), "blue")

  # Add attributes
  attr(trends, "model") <- model
  attr(trends, "response") <- insight::find_response(model)
  attr(trends, "ci") <- ci
  attr(trends, "levels") <- args$levels
  attr(trends, "trend") <- args$trend
  attr(trends, "modulate") <- args$modulate


  # Output
  class(trends) <- c("estimate_slopes", class(trends))
  trends
}


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
