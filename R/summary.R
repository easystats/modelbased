#' @export
summary.estimate_slopes <- function(object, ...) {
  out <- as.data.frame(object)

  # Add "Confidence" col based on the sig index present in the data
  out$Confidence <- .estimate_slopes_significance(out, ...)
  out$Direction <- .estimate_slopes_direction(out, ...)

  attributes(out) <- utils::modifyList(attributes(object), attributes(out))
  class(out) <- c("summary_estimate_slopes", "data.frame")
  attr(out, "table_title") <- c("Average Marginal Effects", "blue")

  out
}


#' @export
summary.reshape_grouplevel <- function(object, ...) {
  x <- object[!duplicated(object), ]
  row.names(x) <- NULL
  x
}


# Utilities ===============================================================


.estimate_slopes_direction <- function(data, ...) {
  centrality_columns <- datawizard::extract_column_names(
    data,
    c("Coefficient", "Slope", "Median", "Mean", "MAP_Estimate"),
    verbose = FALSE
  )
  ifelse(data[[centrality_columns]] < 0, "negative", "positive")
}


.estimate_slopes_significance <- function(x, confidence = "auto", ...) {
  insight::check_if_installed("effectsize")

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
