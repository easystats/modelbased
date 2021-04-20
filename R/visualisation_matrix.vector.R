# -------------------------------------------------------------------------
# Visualisation_matrix functions that work on a vector (a single column)
# See tests/test-visualisation_matrix.R for examples
# -------------------------------------------------------------------------



# Numeric -----------------------------------------------------------------


#' @importFrom stats median sd mad
#' @export
visualisation_matrix.numeric <- function(x, target = NULL, length = 7, standardize = FALSE, reference = x, ...) {

  # Sanity check
  if (!is.numeric(length)) {
    stop("`length` argument should be an number.")
  }

  # Regular spread
  if (standardize == FALSE) {
    out <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = length)

    # Standardize spread
  } else {

    # Fix length
    if ((length %% 2) == 0) {
      warning(paste0("`length` argument should be an odd number when `standardize` is TRUE (so that the mid-value is the mean). Selecting `length = ", length + 1, "`."))
      length <- length + 1
    }
    # Check reference
    if (is.null(reference) || !is.numeric(reference)) {
      stop("`reference` argument must be a numeric vector or dataframe.")
    }

    # Standardized vector
    out <- seq(-(length - 1) / 2, (length - 1) / 2)

    # Reverse standardization
    out <- out * stats::sd(reference, na.rm = TRUE) + mean(reference, na.rm = TRUE)
  }


  # If a target character is detected
  if(!is.null(target) && is.character(target)) {

    # If there is an equal sign
    if(grepl("=", target)) {
      parts <- trimws(unlist(strsplit(target, "=", fixed = TRUE)))  # Split and clean
      out <- eval(parse(text = parts[2]))  # Eval right-hand part

      # If brackets are detected [a, b]
    } else if(grepl("\\[.*\\]", target)) {
      # Keep the content
      parts <- trimws(unlist(regmatches(target, gregexpr("\\[.+?\\]", target))))
      # Drop the brackets
      parts <- gsub("\\[|\\]", "", parts)
      # Split by a separator like ','
      out <- trimws(unlist(strsplit(parts, ",")))
      # If the elements have quotes around them, drop them
      if(all(grepl("\\'.*\\'", out))) out <- gsub("'", "", out)
      if(all(grepl('\\".*\\"', out))) out <- gsub('"', "", out)
      # If only two, it's probably the range
      if(length(out) == 2) {
        out <- seq(out[1], out[2], length.out = length)
        # If more, it's probably the vector
      } else if(length(out) > 2){
        out <- as.numeric(out)
      } else{
        stop(paste0("The `target` argument (", target, ") should indicate the min and the max."))
      }
      # Else, try to directly eval the content
    } else {
      out <- tryCatch({
        eval(parse(text = target))
      }, error = function(r) {
        stop(paste0("The `target` argument (", target, ") cannot be read."))
      })
    }
  }

  names(out) <- NULL
  out
}

#' @export
visualisation_matrix.double <- visualisation_matrix.numeric



# Factors & Characters ----------------------------------------------------



#' @export
visualisation_matrix.factor <- function(x, target = NULL, ...) {
  # Keep only unique levels
  if(is.factor(x)) {
    out <- factor(levels(droplevels(x)), levels = levels(droplevels(x)))
  } else {
    out <- unique(x)
  }
  names(out) <- NULL  # Just in case

  # If a target character is detected
  if(!is.null(target) && is.character(target)) {

    # If there is an equal sign
    if(grepl("=", target)) {
      parts <- trimws(unlist(strsplit(target, "=", fixed = TRUE)))  # Split and clean
      out <- eval(parse(text = parts[2]))  # Eval right-hand part

      # If brackets are detected [a, b]
    } else if(grepl("\\[.*\\]", target)) {
      # Keep the content
      parts <- trimws(unlist(regmatches(target, gregexpr("\\[.+?\\]", target))))
      # Drop the brackets
      parts <- gsub("\\[|\\]", "", parts)
      # Split by a separator like ','
      out <- trimws(unlist(strsplit(parts, ",")))
      # If the elements have quotes around them, drop them
      if(all(grepl("\\'.*\\'", out))) out <- gsub("'", "", out)
      if(all(grepl('\\".*\\"', out))) out <- gsub('"', "", out)
      # Convert to factor if factor
      if(is.factor(x)) out <- as.factor(out)

      # Else, try to directly eval the content
    } else {
      out <- tryCatch({
        parse(text = target)
      }, error = function(r) {
        stop(paste0("The `target` argument (", target, ") cannot be read."))
      })
    }
  }
  out
}

#' @export
visualisation_matrix.character <- visualisation_matrix.factor

#' @export
visualisation_matrix.logical <- visualisation_matrix.character




# Summary -----------------------------------------------------------------




#' @importFrom stats na.omit
#' @keywords internal
.visualisation_matrix_summary <- function(x, numerics = "mean", factors = "reference", na.rm = TRUE) {
  if (na.rm == TRUE) x <- stats::na.omit(x)

  if (is.numeric(x)) {
    fun <- paste0(numerics, "(x)")
    out <- eval(parse(text = fun))
  } else {
    if (factors == "mode") {
      # Get mode
      out <- names(sort(table(x), decreasing = TRUE)[1])
    } else {
      # Get reference
      if (is.factor(x)) {
        out <- levels(x)[1]
      } else if (is.character(x) || is.logical(x)) {
        out <- unique(x)[1]
      } else {
        stop(paste0("Argument is not numeric nor factor but ",
                    class(x),
                    ". Please report the bug at https://github.com/easystats/modelbased/issues"))
      }
    }
  }
  out
}
