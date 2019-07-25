#' @keywords internal
.restore_factor_levels <- function(predictions, data) {
  original_factors <- data[sapply(data, is.factor)]

  for (i in names(predictions)) {
    if (i %in% names(original_factors)) {
      predictions[[i]] <- factor(predictions[[i]], levels = levels(original_factors[[i]]))
    }
  }

  predictions
}





#' Convert to Numeric if Possible
#'
#' Tries to convert vector to numeric if possible. Otherwise, leaves it as is.
#' @param x A vector to be converted.
#'
#' @examples
#' as.numeric_ifnumeric(c("1", "2"))
#' as.numeric_ifnumeric(c("1", "2", "A"))
#' @export
as.numeric_ifnumeric <- function(x) {
  x <- tryCatch(as.numeric(as.character(x)), error = function(e) x, warning = function(w) x)
  x
}








#' @keywords internal
.remove_name_level <- function(x) {
  name <- .find_name_level(x)
  x <- sub(name, "", x)
  x <- trimws(x)
  x
}







#' @keywords internal
.find_name_level <- function(x, data, fixed, modulate) {
  splitted <- strsplit(as.character(x), " ")
  splitted <- data.frame(do.call(rbind, splitted), stringsAsFactors = FALSE)
  uniques <- sapply(splitted, unique)

  lengths <- sapply(uniques, length)
  if (lengths[1] == 1) {
    return(as.character(uniques[[1]]))
  } else if (all(grepl(" - ", x))) {
    return("Contrast")
  } else {
    warning("Couldn't find consistent level name.")
    if (is.null(names(x))) {
      return("X")
    } else {
      return(names(x))
    }
  }
}





#' @keywords internal
.summarize_posteriors <- function(posteriors, centrality = "median", ci = 0.89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1, bf_prior = NULL) {
  x <-
    bayestestR::describe_posterior(posteriors,
      ci = ci, ci_method = ci_method,
      centrality = centrality,
      test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior
    )
  x <- bayestestR::reshape_ci(x)
  if ("CI" %in% names(x) & length(unique(x$CI)) == 1) x$CI <- NULL
  if ("ROPE_CI" %in% names(x) & length(unique(x$ROPE_CI)) == 1) x$ROPE_CI <- NULL
  x$ROPE_low <- x$ROPE_high <- NULL
  x
}
