#' @rdname estimate_grouplevel
#' @param x The output of `estimate_grouplevel()`.
#' @param indices A list containing the indices to extract (e.g., "Coefficient").
#' @param group A list containing the random factors to select.
#' @export
reshape_grouplevel <- function(x, indices = "all", group = "all", ...) {
  UseMethod("reshape_grouplevel")
}


#' @export
reshape_grouplevel.estimate_grouplevel <- function(x, indices = "all", group = "all", ...) {
  # Find indices
  if (any(indices == "all")) {
    indices <- names(x)[!names(x) %in% c("Group", "Level", "Parameter", "CI")]
  }
  if ("Coefficient" %in% indices) {
    indices <- c(indices, "Median", "Mean", "MAP") # Accommodate Bayesian
  }
  if ("SE" %in% indices) {
    indices <- c(indices, "SD") # Accommodate Bayesian
  }
  indices <- names(x)[names(x) %in% unique(indices)]


  # Get original dataframe of random
  data <- attributes(x)$data

  # Random parameters
  if (all(group == "all")) group <- unique(x$Group)

  # Loop through all groups
  for (g in group) {
    # Subset coefficients
    data_group <- x[x$Group == g, ]

    # If no data, skip
    if (nrow(data_group) == 0) next

    # Clean subset of random factors
    data_group[[g]] <- data_group$Level
    newvars <- paste0(g, "_", indices)
    names(data_group)[names(data_group) %in% indices] <- newvars

    # Reshape
    data_group$Parameter <- ifelse(data_group$Parameter == "(Intercept)",
      "Intercept",
      data_group$Parameter
    )

    data_wide <- datawizard::data_to_wide(
      data_group[c(g, newvars, "Parameter")],
      id_cols = g,
      values_from = newvars,
      names_from = "Parameter",
      names_sep = "_"
    )

    # If nested, separate groups
    if (grepl(":", g, fixed = TRUE)) {
      groups <- as.data.frame(t(sapply(strsplit(data_wide[[g]], ":", fixed = TRUE), function(x) as.data.frame(t(x)))))
      names(groups) <- unlist(strsplit(g, ":", fixed = TRUE))
      data_wide <- cbind(groups, data_wide)
      data_wide[g] <- NULL
      g <- names(groups)
    }

    # Merge while preserving order of original random
    data[["__sort_id"]] <- seq_len(nrow(data))
    data <- merge(data, data_wide, by = g, sort = FALSE)
    data <- data[order(data[["__sort_id"]]), ]
    data[["__sort_id"]] <- NULL
  }

  # Clean
  row.names(data) <- NULL

  class(data) <- c("reshape_grouplevel", class(data))
  data
}
