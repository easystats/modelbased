#' Create a reference grid
#'
#' Create a reference matrix, useful for visualisation, with evenly spread and combined values.
#'
#' @param x An object from which to construct the reference grid.
#' @param target Can be "all" or list of characters indicating columns of interest. Can also contain assignments (e.g., \code{target = "Sepal.Length = 2"} or \code{target = c("Sepal.Length = 2", "Species = 'setosa'")} - note the usage of single and double quotes to assign strings within strings). The remaining variables will be fixed.
#' @param length Length of numeric target variables.
#' @param range Can be one of \code{c("range", "iqr", "ci", "hdi", "eti")}. If \code{"range"} (default), will use the min and max of the original vector as end-points. If any other interval, will spread within the range (the default CI width is 95\% but this can be changed by setting something else, e.g., \code{ci = 0.90}). See \code{\link{IQR}} and \code{\link[bayestestR]{ci}}.
#' @param factors Type of summary for factors. Can be "reference" (set at the reference level), "mode" (set at the most common level) or "all" to keep all levels.
#' @param numerics Type of summary for numeric values. Can be "all" (will duplicate the grid for all unique values), any function ("mean", "median", ...) or a value (e.g., \code{numerics = 0}).
#' @param preserve_range In the case of combinations between numeric variables and factors, setting \code{preserve_range = TRUE} will drop the observations where the value of the numeric variable is originally not present in the range of its factor level. This leads to an unbalanced grid. Also, if you want the minimum and the maximum to closely match the actual ranges, you should increase the \code{length} argument.
#' @param ... Arguments passed to or from other methods (for instance, \code{length} or \code{range} to control the spread of numeric variables.).
#' @inheritParams effectsize::format_standardize
#' @inheritParams estimate_response
#'
#'
#' @return Reference grid data frame.
#'
#' @examples
#' library(modelbased)
#'
#' # Add one row to change the "mode" of Species
#' data <- rbind(iris, iris[149, ], make.row.names = FALSE)
#'
#' # Single variable is of interest; all others are "fixed"
#' visualisation_matrix(data, target = "Sepal.Length")
#' visualisation_matrix(data, target = "Sepal.Length", length = 3)
#' visualisation_matrix(data, target = "Sepal.Length", range = "ci", ci = 0.90)
#' visualisation_matrix(data, target = "Sepal.Length", factors = "mode")
#'
#' # Multiple variables are of interest, creating a combination
#' visualisation_matrix(data, target = c("Sepal.Length", "Species"), length = 3)
#' visualisation_matrix(data, target = c(1, 3), length = 3)
#' visualisation_matrix(data, target = c("Sepal.Length", "Species"), preserve_range = TRUE)
#' visualisation_matrix(data, target = c("Sepal.Length", "Species"), numerics = 0)
#' visualisation_matrix(data, target = c("Sepal.Length = 3", "Species"))
#' visualisation_matrix(data, target = c("Sepal.Length = c(3, 1)", "Species = 'setosa'"))
#'
#' # Standardize
#' vizdata <- visualisation_matrix(data, target = "Sepal.Length")
#' effectsize::standardize(vizdata)
#' @export
visualisation_matrix <- function(x, ...) {
  UseMethod("visualisation_matrix")
}





# -------------------------------------------------------------------------
# Below are visualisation_matrix functions for DataFrames
# -------------------------------------------------------------------------

#' @rdname visualisation_matrix
#' @export
visualisation_matrix.data.frame <- function(x, target = "all", factors = "reference", numerics = "mean", preserve_range = FALSE, reference = x, ...) {

  # Valid target argument
  if (all(target == "all") || ncol(x) == 1 || all(names(x) %in% c(target))) {
    target <- names(x)
  }

  if (is.numeric(target)) {
    target <- names(x)[target]
  }

  # Deal with targets ==========================================================

  # Find eventual user-defined specifications for each target
  specs <- do.call(rbind, lapply(target, .visualisation_matrix_clean_target, x = x))
  specs$is_factor <- sapply(x[specs$varname], function(x) is.factor(x) || is.character(x))

  # Create target list of factors -----------------------------------------
  facs <- list()
  for (fac in specs[specs$is_factor == TRUE, "varname"]) {
    facs[[fac]] <- visualisation_matrix(x[[fac]], target = specs[specs$varname == fac, "expression"])
  }

  # Create target list of numerics ----------------------------------------
  nums <- list()
  for (num in specs[specs$is_factor == FALSE, "varname"]) {
    nums[[num]] <- visualisation_matrix(x[[num]],
      target = specs[specs$varname == num, "expression"],
      reference = reference[[num]],
      ...
    )
  }
  # Assemble the two
  targets <- expand.grid(c(nums, facs))

  # Preserve range ---------------------------------------------------------
  if (preserve_range == TRUE && length(facs) > 0 && length(nums) > 0) {

    # Loop through the combinations of factors
    facs_combinations <- expand.grid(facs)
    for (i in 1:nrow(facs_combinations)) {
      # Query subset of original dataset
      subset <- x[datawizard::data_match(x, to = facs_combinations[i, , drop = FALSE]), , drop = FALSE]
      idx <- datawizard::data_match(targets, to = facs_combinations[i, , drop = FALSE])

      # Skip if no instance of factor combination, drop the chunk
      if (nrow(subset) == 0) {
        targets <- targets[-idx, ]
        break
      }

      # Else, filter given the range of numerics
      rows_to_remove <- c()
      for (num in names(nums)) {
        mini <- min(subset[[num]], na.rm = TRUE)
        maxi <- max(subset[[num]], na.rm = TRUE)
        rows_to_remove <- c(rows_to_remove, which(targets[[num]] < mini | targets[[num]] > maxi))
      }
      if(length(rows_to_remove) > 0) {
        targets <- targets[-idx[idx %in% rows_to_remove], ] # Drop incompatible rows
        row.names(targets) <- NULL # Reset row.names
      }
    }

    if (nrow(targets) == 0) {
      stop("No data left was left after range preservation. Try increasing `length` or setting `preserve_range` to FALSE.")
    }
  }

  # Deal with the rest =========================================================
  rest_vars <- names(x)[!names(x) %in% names(targets)]
  if (length(rest_vars) >= 1) {
    rest_df <- lapply(x[rest_vars], .visualisation_matrix_summary, numerics = numerics, factors = factors, ...)
    rest_df <- expand.grid(rest_df, stringsAsFactors = FALSE)
    targets <- merge(targets, rest_df, sort = FALSE)
  }

  # Prepare output =============================================================
  # Reset row names
  row.names(targets) <- NULL

  # Attributes
  attr(targets, "adjusted_for") <- ifelse(length(rest_vars) >= 1, rest_vars, NA)
  attr(targets, "target_specs") <- specs
  attr(targets, "target") <- target
  attr(targets, "preserve_range") <- preserve_range
  attr(targets, "reference") <- reference
  attr(targets, "data") <- x

  # Printing decorations
  attr(targets, "table_title") <- c("Visualisation Grid", "blue")
  if (length(rest_vars) >= 1) attr(targets, "table_footer") <- paste0("\nMaintained constant: ", paste0(rest_vars, collapse = ", "))
  if (!is.null(attr(targets, "table_footer"))) attr(targets, "table_footer") <- c(attr(targets, "table_footer"), "blue")

  class(targets) <- c("visualisation_matrix", class(targets))
  targets
}












# Utils -------------------------------------------------------------------

#' @keywords internal
.visualisation_matrix_summary <- function(x, numerics = "mean", factors = "reference", na.rm = TRUE, ...) {
  if (na.rm == TRUE) x <- stats::na.omit(x)

  if (is.numeric(x)) {
    if (is.numeric(numerics)) {
      out <- numerics
    } else {
      if (numerics %in% c("all", "combination")) {
        out <- unique(x)
      } else {
        out <- eval(parse(text = paste0(numerics, "(x)")))
      }
    }
  } else {
    if (factors %in% c("all", "combination")) {
      out <- unique(x)
    } else if (factors == "mode") {
      # Get mode
      out <- names(sort(table(x), decreasing = TRUE)[1])
    } else {
      # Get reference
      if (is.factor(x)) {
        out <- levels(x)[1]
      } else if (is.character(x) || is.logical(x)) {
        out <- unique(x)[1]
      } else {
        stop(paste0(
          "Argument is not numeric nor factor but ",
          class(x),
          ". Please report the bug at https://github.com/easystats/modelbased/issues"
        ))
      }
    }
  }
  out
}





# -------------------------------------------------------------------------
# Below are visualisation_matrix functions that work on a vector (a single column)
# See tests/test-visualisation_matrix.R for examples
# -------------------------------------------------------------------------



# Numeric -----------------------------------------------------------------

#' @rdname visualisation_matrix
#' @export
visualisation_matrix.numeric <- function(x, length = 10, range = "range", ...) {

  # Sanity check
  if (!is.numeric(length)) {
    stop("`length` argument should be an number.")
  }

  # Check and clean the target argument
  specs <- .visualisation_matrix_clean_target(x, ...)

  if (is.na(specs$expression)) {
    # Create a spread
    out <- .create_spread(x, length = length, range = range, ...)
  } else {
    # Run the expression cleaned from target
    out <- eval(parse(text = specs$expression))
  }

  out
}

#' @export
visualisation_matrix.double <- visualisation_matrix.numeric





# Factors & Characters ----------------------------------------------------


#' @rdname visualisation_matrix
#' @export
visualisation_matrix.factor <- function(x, ...) {

  # Check and clean the target argument
  specs <- .visualisation_matrix_clean_target(x, ...)

  if (is.na(specs$expression)) {

    # Keep only unique levels
    if (is.factor(x)) {
      out <- factor(levels(droplevels(x)), levels = levels(droplevels(x)))
    } else {
      out <- unique(x)
    }
  } else {
    # Run the expression cleaned from target
    out <- eval(parse(text = specs$expression))
  }
  out
}

#' @export
visualisation_matrix.character <- visualisation_matrix.factor

#' @export
visualisation_matrix.logical <- visualisation_matrix.character



#' @keywords internal
.create_spread <- function(x, length = 10, range = "range", ci = 0.95, ...) {
  range <- match.arg(tolower(range), c("range", "iqr", "ci", "hdi", "eti"))

  if (range == "iqr") {
    mini <- stats::quantile(x, (1 - ci) / 2, ...)
    maxi <- stats::quantile(x, (1 + ci) / 2, ...)
  } else if (range == "ci") {
    out <- bayestestR::ci(x, ci = ci, ...)
    mini <- out$CI_low
    maxi <- out$CI_high
  } else if (range == "eti") {
    out <- bayestestR::eti(x, ci = ci, ...)
    mini <- out$CI_low
    maxi <- out$CI_high
  } else if (range == "hdi") {
    out <- bayestestR::hdi(x, ci = ci, ...)
    mini <- out$CI_low
    maxi <- out$CI_high
  } else {
    mini <- min(x, na.rm = TRUE)
    maxi <- max(x, na.rm = TRUE)
  }
  seq(mini, maxi, length.out = length)
}


# Utilities -----------------------------------------------------------------

#' @keywords internal
.visualisation_matrix_clean_target <- function(x, target = NULL, ...) {
  expression <- NA
  varname <- NA
  original_target <- target

  if (!is.null(target)) {
    if (is.data.frame(x) && target %in% names(x)) {
      return(data.frame(varname = target, expression = NA))
    }

    # If there is an equal sign
    if (grepl("length.out =", target)) {
      expression <- target # This is an edgecase
    } else if (grepl("=", target)) {
      parts <- trimws(unlist(strsplit(target, "=", fixed = TRUE))) # Split and clean
      varname <- parts[1] # left-hand part is probably the name of the variable
      target <- parts[2] # right-hand part is the real target
    }

    if (is.na(expression) && is.data.frame(x)) {
      if (!is.na(varname)) {
        x <- x[[varname]]
      } else {
        stop("Couldn't find which variable were selected in `target`. Check spelling and specification.")
      }
    }

    # If brackets are detected [a, b]
    if (is.na(expression) && grepl("\\[.*\\]", target)) {

      # Clean --------------------
      # Keep the content
      parts <- trimws(unlist(regmatches(target, gregexpr("\\[.+?\\]", target))))
      # Drop the brackets
      parts <- gsub("\\[|\\]", "", parts)
      # Split by a separator like ','
      parts <- trimws(unlist(strsplit(parts, ",")))
      # If the elements have quotes around them, drop them
      if (all(grepl("\\'.*\\'", parts))) parts <- gsub("'", "", parts)
      if (all(grepl('\\".*\\"', parts))) parts <- gsub('"', "", parts)

      # Make expression ----------
      if (is.factor(x) || is.character(x)) { # Factor
        # Add quotes around them
        parts <- paste0("'", parts, "'")
        # Convert to character
        expression <- paste0("as.factor(c(", paste0(parts, collapse = ", "), "))")
      } else { # Numeric
        # If only two, it's probably the range
        if (length(parts) == 2) {
          expression <- paste0("seq(", parts[1], ", ", parts[2], ", length.out = length)")
          # If more, it's probably the vector
        } else if (length(parts) > 2) {
          parts <- as.numeric(parts)
          expression <- paste0("c(", paste0(parts, collapse = ", "), ")")
        } else {
          stop(paste0("The `target` argument (", target, ") should indicate the min and the max."))
        }
      }
      # Else, try to directly eval the content
    } else {
      expression <- target
      # Try to eval and make sure it works
      tryCatch(
        {
          # This is just to make sure that an expression with `length` in
          # it doesn't fail because of this undefined var
          length <- 10
          eval(parse(text = target))
        },
        error = function(r) {
          stop(paste0("The `target` argument (`", original_target, "`) cannot be read and could be mispecified."))
        }
      )
    }
  }
  data.frame(varname = varname, expression = expression)
}
