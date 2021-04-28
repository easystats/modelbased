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

  # Check and clean the target argument
  specs <- .visualisation_matrix_clean_target(x, target)


  if(is.na(specs$expression)) {

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

  } else {

    # Run the expression cleaned from target
    out <- tryCatch({
      eval(parse(text = specs$expression))
    }, error = function(r) {
      stop(paste0("The `target` argument (", target, ") cannot be read and could be mispecified."))
    })
  }

  out
}

#' @export
visualisation_matrix.double <- visualisation_matrix.numeric



# Factors & Characters ----------------------------------------------------



#' @export
visualisation_matrix.factor <- function(x, target = NULL, ...) {

  # Check and clean the target argument
  specs <- .visualisation_matrix_clean_target(x, target)

  if(is.na(specs$expression)) {

    # Keep only unique levels
    if(is.factor(x)) {
      out <- factor(levels(droplevels(x)), levels = levels(droplevels(x)))
    } else {
      out <- unique(x)
    }

  } else {

    # Run the expression cleaned from target
    out <- tryCatch({
      eval(parse(text = specs$expression))
    }, error = function(r) {
      stop(paste0("The `target` argument (", target, ") cannot be read and could be mispecified."))
    })

  }
  out
}

#' @export
visualisation_matrix.character <- visualisation_matrix.factor

#' @export
visualisation_matrix.logical <- visualisation_matrix.character




# Summary -----------------------------------------------------------------

#' @keywords internal
.visualisation_matrix_clean_target <- function(x, target = NULL) {
  expression <- NA
  varname <- NA

  if(!is.null(target)) {

    # If there is an equal sign
    if(grepl("=", target)) {
      parts <- trimws(unlist(strsplit(target, "=", fixed = TRUE)))  # Split and clean
      varname <- parts[1]  # left-hand part is probably the name of the variable
      target <- parts[2]  # right-hand part is the real target
    }

    # If brackets are detected [a, b]
    if(grepl("\\[.*\\]", target)) {

      # Clean --------------------
      # Keep the content
      parts <- trimws(unlist(regmatches(target, gregexpr("\\[.+?\\]", target))))
      # Drop the brackets
      parts <- gsub("\\[|\\]", "", parts)
      # Split by a separator like ','
      parts <- trimws(unlist(strsplit(parts, ",")))
      # If the elements have quotes around them, drop them
      if(all(grepl("\\'.*\\'", parts))) parts <- gsub("'", "", parts)
      if(all(grepl('\\".*\\"', parts))) parts <- gsub('"', "", parts)

      # Make expression ----------
      if(is.factor(x) || is.character(x)) {  # Factor
        # Add quotes around them
        parts <- paste0("'", parts, "'")
        # Convert to character
        expression <- paste0("as.factor(c(", paste0(parts, collapse = ", "), "))")

      } else {  # Numeric
        # If only two, it's probably the range
        if(length(parts) == 2) {
          expression <- paste0("seq(", parts[1], ", ", parts[2], ", length.out = length)")
          # If more, it's probably the vector
        } else if(length(parts) > 2){
          parts <- as.numeric(parts)
          expression <- paste0("c(", paste0(parts, collapse = ", "), ")")
        } else{
          stop(paste0("The `target` argument (", target, ") should indicate the min and the max."))
        }
      }
      # Else, try to directly eval the content
    } else {
      expression <- target
    }
  }

  list(varname = varname, expression = expression)
}




