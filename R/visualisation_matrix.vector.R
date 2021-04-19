# visualisation_matrix.vector <- function(x, target = "all", length = 10, factors = "reference", numerics = "mean", preserve_range = FALSE, standardize = FALSE, standardize_robust = FALSE, reference = x, na.rm = TRUE, ...) {
#
# }

#' @importFrom stats median sd mad
#' @export
visualisation_matrix.numeric <- function(x, length = 10, standardize = FALSE, standardize_robust = FALSE, reference = x, ...) {

  if (!is.numeric(length)) {
    stop("`length` argument should be an integer or 'SD' or 'MAD'.")
  }

  # Regular spread
  if (standardize == FALSE) {
    out <- seq(
      min(x, na.rm = TRUE),
      max(x, na.rm = TRUE),
      length = length
    )
    # Standardize spread
  } else {
    if ((length %% 2) == 0) {
      warning(paste0("`length` argument should be an odd number when `standardize` is TRUE (so that the mid-value is the centre). Selecting `length = ", length + 1, "`."))
      length <- length + 1
    }
    # Standardized vector
    out <- seq(
      -(length - 1) / 2,
      (length - 1) / 2,
      by = 1
    )
    # Check reference
    if (!is.numeric(reference)) {
      stop("`reference` argument must be a numeric vector or dataframe.")
    }
    # Reverse standardization
    if (standardize_robust) {
      out <- out * stats::mad(reference, na.rm = TRUE) + stats::median(reference, na.rm = TRUE)
    } else {
      out <- out * stats::sd(reference, na.rm = TRUE) + mean(reference, na.rm = TRUE)
    }
  }

  names(out) <- NULL
  out
}

#' @export
visualisation_matrix.double <- visualisation_matrix.vector



# Factors & Characters ----------------------------------------------------



#' @export
visualisation_matrix.factor <- function(x, target = NULL, ...) {
  if(is.factor(x)) {
    out <- factor(levels(droplevels(x)), levels = levels(droplevels(x)))
  } else {
    out <- unique(x)
  }
  names(out) <- NULL

  if(!is.null(target) && is.character(target)) {
    if(grepl("=", target)) {
      parts <- trimws(unlist(strsplit(target, "=", fixed = TRUE)))
      out <- eval(parse(text = parts[2]))
    } else if(grepl("\\[.*\\]", target)) {
      parts <- trimws(unlist(regmatches(target, gregexpr("\\[.+?\\]", target))))
      parts <- gsub("\\[|\\]", "", parts)
      out <- trimws(unlist(strsplit(parts, ",")))
      if(all(grepl("\\'.*\\'", out))) out <- gsub("'", "", out)
      if(all(grepl('\\".*\\"', out))) out <- gsub('"', "", out)
      if(is.factor(x)) out <- as.factor(out)
    } else {
      out <- tryCatch({
        parse(text = target)
      }, error = function(r) {
        stop(paste0("The `target` argument (", target, ") cannot be read"))
      })
    }
  }
  out
}

#' @export
visualisation_matrix.character <- visualisation_matrix.factor

#' @export
visualisation_matrix.logical <- visualisation_matrix.character
