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


#' @export
summary.reshape_grouplevel <- function(object, ...) {
  x <- object[!duplicated(object), ]
  row.names(x) <- NULL
  x
}


# Utilities ===============================================================


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
