#' @rdname estimate_random
#' @param x The output of \code{estimate_random()}.
#' @param indices A list containing the indices to extract.
#' @export
reshape_random <- function(x, indices = "all", ...) {
  UseMethod("reshape_random")
}


#' @export
reshape_random.estimate_random <- function(x, indices = "all", ...) {

  # Find indices
  if(any(indices == "all")) {
    indices <- names(x)[!names(x) %in% c("Group", "Level", "Parameter", "CI")]
  }
  if ("Coefficient" %in% indices) {
    indices <- c(indices, "Median", "Mean", "MAP") # Accommodate Bayesian
  }
  if ("SE" %in% indices) {
    indices <- c(indices, "SD") # Accommodate Bayesian
  }
  indices <- names(x)[names(x) %in% unique(indices)]


  # Get original data frame of random
  data <- attributes(x)$data

  # Random parameters
  groups <- unique(x$Group)

  # Loop through all groups
  for (group in groups) {

    # Subset coefficients
    data_group <- x[x$Group == group, ]

    # If no data, skip
    if (nrow(data_group) == 0) next

    # Clean subset of random factors
    data_group[[group]] <- data_group$Level
    newvars <- paste0(group, "_", indices)
    names(data_group)[names(data_group) %in% indices] <- newvars

    # Reshape
    data_group$Parameter <- ifelse(data_group$Parameter == "(Intercept)",
                                   "Intercept",
                                   data_group$Parameter)

    data_wide <- insight::data_to_wide(
      data_group[c(group, newvars, "Parameter")],
      rows_from = group,
      values_from = newvars,
      colnames_from = "Parameter",
      sep = "_")

    # If nested, separate groups
    if (grepl(":", group)) {
      groups <- as.data.frame(t(sapply(strsplit(data_wide[[group]], ":"), function(x) as.data.frame(t(x)))))
      names(groups) <- unlist(strsplit(group, ":"))
      data_wide <- cbind(groups, data_wide)
      data_wide[group] <- NULL
      group <- names(groups)
    }

    # Merge while preserving order of original random
    data[["__sort_id"]] <- 1:nrow(data)
    data <- merge(data, data_wide, by = group, sort = FALSE)
    data <- data[order(data[["__sort_id"]]), ]
    data[["__sort_id"]] <- NULL
  }

  # Clean
  row.names(data) <- NULL

  class(data) <- c("reshape_random", class(data))
  data
}


#' @export
summary.reshape_random <- function(object, ...) {
  x <- object[!duplicated(object), ]
  row.names(x) <- NULL
  x
}
