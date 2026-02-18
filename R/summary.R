#' @export
summary.estimate_slopes <- function(object, verbose = TRUE, ...) {
  out <- as.data.frame(object, preserve_names = TRUE)
  by <- attributes(object)$by

  if (verbose && nrow(out) < 50) {
    insight::format_alert(
      "There might be too few data to accurately determine intervals. Consider setting `length = 100` (or larger) in your call to `estimate_slopes()`."
    ) # nolint
  }

  # Add "Confidence" col based on the sig index present in the data
  out$Confidence <- .estimate_slopes_significance(out, ...)
  out$Direction <- .estimate_slopes_direction(out, ...)

  # if we have more than one variable in `by`, group result table and
  # add group name as separate column
  if (length(by) > 1) {
    parts <- split(out, out[[by[2]]])
    out <- do.call(rbind, lapply(parts, .estimate_slope_parts, by = by[1]))
    out <- datawizard::rownames_as_column(out, "Group")
    out$Group <- gsub("\\.\\d+$", "", out$Group)
  } else {
    out <- .estimate_slope_parts(out, by)
  }

  if (anyNA(out$Confidence) && verbose) {
    insight::format_warning("Significance could not be determined for some intervals.")
  }

  attributes(out) <- utils::modifyList(attributes(object), attributes(out))
  class(out) <- c("summary_estimate_slopes", "data.frame")
  attr(out, "table_title") <- c("Johnson-Neymann Intervals", "blue")

  out
}


#' @export
summary.reshape_grouplevel <- function(object, ...) {
  x <- object[!duplicated(object), ]
  row.names(x) <- NULL
  x
}


# Utilities ===============================================================

.estimate_slope_parts <- function(out, by) {
  # mark all "changes" from negative to positive and vice versa
  index <- 1
  out$switch <- index
  index <- index + 1

  for (i in 2:nrow(out)) {
    # we only proceed if we have non-missing values
    do_switch <- (!is.na(out$Direction[i - 1]) & !is.na(out$Direction[i])) &&
      out$Direction[i] != out$Direction[i - 1]

    if (!do_switch) {
      do_switch <- (!is.na(out$Confidence[i - 1]) & !is.na(out$Confidence[i])) &&
        out$Confidence[i] != out$Confidence[i - 1]
    }

    if (do_switch) {
      out$switch[i:nrow(out)] <- index # styler: off
      index <- index + 1
    }
  }

  # split into "switches"
  parts <- split(out, out$switch)

  do.call(
    rbind,
    lapply(parts, function(i) {
      data.frame(
        Start = i[[by]][1],
        End = i[[by]][nrow(i)],
        Direction = i$Direction[1],
        Confidence = i$Confidence[1]
      )
    })
  )
}


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
    if ("BF" %in% names(x)) {
      confidence <- "BF"
    }
    if ("p" %in% names(x)) {
      confidence <- "p"
    }
    if ("pd" %in% names(x)) confidence <- "pd"
  }

  switch(
    confidence,
    p = tools::toTitleCase(effectsize::interpret_p(x$p, ...)),
    BF = tools::toTitleCase(effectsize::interpret_bf(x$BF, ...)),
    pd = tools::toTitleCase(effectsize::interpret_pd(x$pd, ...)),
    {
      # Based on CI
      out <- ifelse(
        (x$CI_high < 0 & x$CI_low < 0) | (x$CI_high > 0 & x$CI_low > 0),
        "Significant",
        "Uncertain"
      )
      factor(out, levels = c("Uncertain", "Significant"))
    }
  )
}
