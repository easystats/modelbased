#' Create a reference grid.
#'
#'
#' @param x An object from which to contruct the reference grid.
#' @param target Can be "all" or list of characters indicating columns of interest. The remaining variables will be fixed.
#' @param length Length of numeric target variables.
#' @param factors Type of summary for factors. Can be "combination" (include all unique values), "reference" (set at the reference level) or "mode" (set at the most common level).
#' @param numerics Type of summary for numerics Can be "combination" (include all unique values), any function ("mean", "median", ...) or a value (e.g., \code{numerics = 0}).
#' @param preserve_range In the case of combinations between numeric variables and factors, setting \code{preserve_range = TRUE} removes observerations where the value of the numeric variable is originally not present in the range of its factor level.
#' @param standardize The numeric target value is spread as deviations from the mean, with the central value being the mean (or the median if \code{standardize_robust} is TRUE). For instance, if \code{x} is a vector of mean 1 and SD 2.5, and a standardized grid is required of length 3, the result will be \code{c(Mean-1*SD, Mean, Mean+1*SD)}, i.e., \code{c(-1.5, 1, 3.5)}. Each value represents deviations (in terms of SD or MAD) from the central value. This needs the \code{length} argument to be an even integer, so that the central value represent the mean.
#' @param standardize_robust Standardization based on median and MAD (a robust equivalent of the SD).
#' @param na.rm Remove NaNs.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams parameters::format_standardize
#'
#'
#' @examples
#' library(estimate)
#'
#' visualisation_matrix(iris, target = "Sepal.Length")
#' visualisation_matrix(iris, target = "Sepal.Length", factors = "combinations")
#' visualisation_matrix(iris, target = c("Sepal.Length", "Species"), length = 3)
#' visualisation_matrix(iris, target = c("Sepal.Length", "Species"), numerics = 0)
#' visualisation_matrix(iris, target = "Sepal.Length", standardize = TRUE, length = 3)
#' @importFrom stats na.omit
#' @export
visualisation_matrix <- function(x, target = "all", length = 10, factors = "reference", numerics = "mean", preserve_range = FALSE, standardize = FALSE, standardize_robust = FALSE, reference = x, na.rm = TRUE, ...) {
  UseMethod("visualisation_matrix")
}



# Models ------------------------------------------------------------------


#' @export
visualisation_matrix.stanreg <- function(x, target = "all", length = 10, factors = "reference", numerics = "mean", preserve_range = FALSE, standardize = FALSE, standardize_robust = FALSE, reference = x, na.rm = TRUE, random = TRUE, ...) {
  data <- insight::get_data(x)
  if (random == FALSE) {
    data <- data[insight::find_predictors(x, effects = "fixed", flatten = TRUE)]
  }
  data <- visualisation_matrix(data, target = target, length = length, factors = factors, numerics = numerics, preserve_range = preserve_range, standardize = standardize, standardize_robust = standardize_robust, reference = data, na.rm = na.rm, random = TRUE, ...)
  data
}


#' @export
visualisation_matrix.brmsfit <- visualisation_matrix.stanreg
#' @export
visualisation_matrix.lm <- visualisation_matrix.stanreg
#' @export
visualisation_matrix.glm <- visualisation_matrix.stanreg
#' @export
visualisation_matrix.merMod <- visualisation_matrix.stanreg
#' @export
visualisation_matrix.lmerMod <- visualisation_matrix.stanreg










# dataframes ---------------------------------------------------------------

#' @export
visualisation_matrix.data.frame <- function(x, target = "all", length = 10, factors = "reference", numerics = "mean", preserve_range = FALSE, standardize = FALSE, standardize_robust = FALSE, reference = x, na.rm = TRUE, ...) {

  # Target
  if (all(target == "all") | ncol(x) == 1 | all(names(x) %in% c(target))) {
    grid <- .visualisation_matrix_target(x, length = length, standardize = standardize, standardize_robust = standardize_robust, reference = reference)
    return(.preserve_range(grid, x, preserve_range))
  }

  target_df <- .visualisation_matrix_target(x[c(target)], length = length, standardize = standardize, standardize_robust = standardize_robust, reference = reference)

  # Rest
  df_rest <- x[!names(x) %in% c(target)]
  var_order <- names(df_rest)

  facs <- df_rest[!sapply(df_rest, is.numeric)]
  facs <- as.data.frame(sapply(facs, as.factor))
  nums <- df_rest[sapply(df_rest, is.numeric)]


  if (factors %in% c("reference", "mode")) {
    facs <- as.data.frame(sapply(facs, .smart_summary, factors = factors, na.rm = na.rm, simplify = FALSE))
  } else {
    facs <- .visualisation_matrix_target(facs)
  }

  if (is.numeric(numerics)) {
    nums[1, ] <- numerics
    nums <- nums[1, ]
  } else if (numerics == "combination") {
    nums <- .visualisation_matrix_target(nums, length = length, standardize = FALSE, standardize_robust = standardize_robust, reference = reference)
  } else {
    nums <- as.data.frame(sapply(nums, .smart_summary, numerics = numerics, na.rm = na.rm, simplify = FALSE))
  }


  if (nrow(facs) == 0 | ncol(facs) == 0) {
    refrest <- nums
  } else if (nrow(nums) == 0 | ncol(nums) == 0) {
    refrest <- facs
  } else {
    refrest <- merge(facs, nums)
  }

  refrest <- refrest[var_order]
  grid <- merge(target_df, refrest)

  # Preserve range
  grid <- .preserve_range(grid, x, preserve_range)
  grid
}





#' @keywords internal
.preserve_range <- function(grid, x, preserve_range = TRUE) {
  if (preserve_range == FALSE) {
    return(grid)
  }

  cols <- names(grid)[sapply(grid, function(x) length(unique(x)) > 1)]

  nums <- names(grid[cols])[sapply(grid[cols], is.numeric)]
  facs <- names(grid[cols])[!names(grid[cols]) %in% nums]

  if (length(facs) == 0 | length(nums) == 0) {
    return(grid)
  }

  rows_to_keep <- row.names(grid)
  for (fac in facs) {
    for (num in nums) {
      for (level in unique(grid[[fac]])) {
        max_value <- max(x[x[[fac]] == level, num])
        min_value <- min(x[x[[fac]] == level, num])
        rows_to_remove <- c(row.names(grid[grid[[fac]] == level & grid[[num]] < min_value, ]))
        rows_to_remove <- c(rows_to_remove, row.names(grid[grid[[fac]] == level & grid[[num]] > max_value, ]))
        rows_to_keep <- rows_to_keep[!rows_to_keep %in% rows_to_remove]
      }
    }
  }
  grid <- grid[rows_to_keep, ]
  row.names(grid) <- NULL
  grid
}





#' @keywords internal
.smart_summary <- function(x, numerics = "mean", factors = "reference", na.rm = TRUE) {
  if (na.rm == TRUE) x <- na.omit(x)

  if (is.numeric(x)) {

    fun <- paste0(numerics, "(x)")
    out <- eval(parse(text = fun))

  } else{

    if(factors == "mode"){
      # Get mode
      out <- names(sort(table(x),decreasing=TRUE)[1])
    } else{
      # Get reference
      if (is.factor(x)) {
        out <- levels(x)[1]
      } else if (is.character(x)) {
        out <- unique(x)[1]
      } else if (is.logical(x)) {
        out <- unique(x)[1]
      } else {
        warning("Argument is not numeric nor factor: returning NA.")
        out <- NA
      }
    }
  }
  out
}




#' @keywords internal
.visualisation_matrix_target <- function(x, length = 10, standardize = FALSE, standardize_robust = FALSE, reference = x) {
  varnames <- names(x)
  vars <- list()
  for (i in varnames) {
    vars[[i]] <- visualisation_matrix(x[[i]], length = length, standardize = standardize, standardize_robust = standardize_robust, reference = as.data.frame(reference)[[i]])
  }

  grid <- data.frame()
  for (i in varnames) {
    var <- data.frame(vars[[i]])
    names(var) <- i
    if (nrow(grid) == 0) {
      grid <- var
    } else {
      grid <- merge(grid, var)
    }
  }
  grid
}









#' @export
visualisation_matrix.vector <- function(x, target = "all", length = 10, factors = "reference", numerics = "mean", preserve_range = FALSE, standardize = FALSE, standardize_robust = FALSE, reference = x, na.rm = TRUE, ...) {
  if (is.factor(x)) {
    out <- as.factor(levels(droplevels(x)))
  } else if (is.character(x)) {
    x <- as.factor(x)
    out <- as.factor(levels(droplevels(x)))
  } else if (is.logical(x)) {
    x <- as.factor(x)
    out <- as.factor(levels(droplevels(x)))
  } else if (length(unique(x)) < 3) {
    x <- as.factor(x)
    out <- as.factor(levels(droplevels(x)))
  } else if (is.numeric(x)) {
    if (is.numeric(length)) {

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
          out <- out * mad(reference, na.rm = TRUE) + median(reference, na.rm = TRUE)
        } else {
          out <- out * sd(reference, na.rm = TRUE) + mean(reference, na.rm = TRUE)
        }
      }
    } else {
      warning("`length` argument should be an integer or 'SD' or 'MAD'.")
      return(NA)
    }
  } else {
    warning("Vector is neither numeric nor factor: returning NA.")
    return(NA)
  }

  names(out) <- NULL
  out
}

#' @export
visualisation_matrix.numeric <- visualisation_matrix.vector

#' @export
visualisation_matrix.double <- visualisation_matrix.vector

#' @export
visualisation_matrix.factor <- visualisation_matrix.vector

#' @export
visualisation_matrix.logical <- visualisation_matrix.vector

#' @export
visualisation_matrix.character <- visualisation_matrix.vector
