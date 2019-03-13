#' Create a reference grid.
#'
#'
#' @param x An object from which to contruct the reference grid.
#' @param target Can be "all" or list of characters indicating columns of interest. The remaining variables will be fixed.
#' @param length Length of numeric target variables.
#' @param factors Type of summary for factors. Can be "combination" or "reference".
#' @param numerics Type of summary for numerics Can be "combination", any function ("mean", "median", ...) or a value.
#' @param preserve_range This removes the originally non-existing target numeric values of each factor levels.
#' @param na.rm Remove NaNs.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' newdata <- data_grid(iris, target = "Sepal.Length")
#' newdata <- data_grid(iris, target = "Sepal.Length", factors = "combinations")
#' newdata <- data_grid(iris, target = c("Sepal.Length", "Species"), length = 3)
#' newdata <- data_grid(iris, target = c("Sepal.Length", "Species"), numerics = 0)
#' @importFrom stats na.omit
#' @export
data_grid <- function(x, target = "all", length = 100, factors = "reference", numerics = "mean", preserve_range = preserve_range, na.rm = TRUE, ...) {
  UseMethod("data_grid")
}








# Models ------------------------------------------------------------------


#' @export
data_grid.stanreg <- function(x, target = "all", length = 100, factors = "reference", numerics = "mean", preserve_range = FALSE, na.rm = TRUE, random = TRUE, ...) {
  data <- insight::get_data(x)
  if (random == FALSE) {
    data <- data[insight::find_predictors(x, effects = "fixed", flatten = TRUE)]
  }
  data <- data_grid(data, target = target, length = length, factors = factors, numerics = numerics, preserve_range = preserve_range, na.rm = na.rm, random = TRUE, ...)
  return(data)
}


#' @export
data_grid.brmsfit <- data_grid.stanreg
#' @export
data_grid.lm <- data_grid.stanreg
#' @export
data_grid.glm <- data_grid.stanreg
#' @export
data_grid.merMod <- data_grid.stanreg
#' @export
data_grid.lmerMod <- data_grid.stanreg










# dataframes ---------------------------------------------------------------

#' @rdname data_grid
#' @examples
#' x <- iris
#' target <- c("Sepal.Length", "Species")
#' @export
data_grid.data.frame <- function(x, target = "all", length = 10, factors = "reference", numerics = "mean", preserve_range = FALSE, na.rm = TRUE, ...) {
  # Target
  if (all(target == "all") | ncol(x) == 1 | all(names(x) %in% c(target))) {
    return(.data_grid_target(x, length = length))
  }

  target_df <- .data_grid_target(x[c(target)], length = length)

  # Rest
  df_rest <- x[!names(x) %in% c(target)]
  var_order <- names(df_rest)

  facs <- df_rest[!sapply(df_rest, is.numeric)]
  facs <- as.data.frame(sapply(facs, as.factor))
  nums <- df_rest[sapply(df_rest, is.numeric)]


  if (factors == "reference") {
    facs <- as.data.frame(sapply(facs, .smart_summary, na.rm = na.rm, simplify = FALSE))
  } else {
    facs <- .data_grid_target(facs)
  }

  if (is.numeric(numerics)) {
    nums[1, ] <- numerics
    nums <- nums[1, ]
  } else if (numerics == "combination") {
    nums <- .data_grid_target(nums, length = length)
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

  # Remove non-existing values in factor levels
  # TODO: this code is very ugly
  factors <- names(grid)[sapply(grid, is.factor)]
  if (length(factors) > 0 & preserve_range == TRUE) {
    rows_to_keep <- row.names(grid)
    for (fac in factors) {
      if (length(unique(grid[[fac]])) > 1) {
        for (var in target[target != fac]) {
          for (level in unique(grid[[fac]])) {
            max_value <- max(x[x[[fac]] == level, var])
            min_value <- min(x[x[[fac]] == level, var])
            rows_to_remove <- c()
            rows_to_remove <- c(rows_to_remove, row.names(grid[grid[[fac]] == level & grid[[var]] <= min_value, ]))
            rows_to_remove <- c(rows_to_remove, row.names(grid[grid[[fac]] == level & grid[[var]] >= max_value, ]))
            print(rows_to_keep)
            rows_to_keep <- rows_to_keep[!rows_to_keep %in% rows_to_remove]
          }
        }
      }
    }
    grid <- grid[rows_to_keep, ]
  }





  return(grid)
}



#' @keywords internal
.smart_summary <- function(x, numerics = "mean", na.rm = TRUE) {
  if (na.rm == TRUE) x <- na.omit(x)

  if (is.numeric(x)) {
    fun <- paste0(numerics, "(x)")
    out <- eval(parse(text = fun))
  } else if (is.factor(x)) {
    out <- levels(x)[1]
  } else if (is.character(x)) {
    out <- unique(x)[1]
  } else if (is.logical(x)) {
    out <- unique(x)[1]
  } else {
    warning("Argument is not numeric nor factor: returning NA.")
    out <- NA
  }
  return(out)
}




#' @keywords internal
.data_grid_target <- function(x, length = 10) {
  vars <- sapply(x, data_grid, length = length, simplify = FALSE)
  varnames <- names(x)
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
  return(grid)
}












#' Create a reference grid.
#'
#' @inheritParams data_grid
#' @export
data_grid.vector <- function(x, target = "all", length = 10, ...) {
  if (is.factor(x)) {
    out <- as.factor(levels(x))
  } else if (is.character(x)) {
    x <- as.factor(x)
    out <- as.factor(levels(x))
  } else if (is.logical(x)) {
    x <- as.factor(x)
    out <- as.factor(levels(x))
  } else if (length(unique(x)) < 3) {
    x <- as.factor(x)
    out <- as.factor(levels(x))
  } else if (is.numeric(x)) {
    out <- seq(min(x, na.rm = TRUE),
      max(x, na.rm = TRUE),
      length = length
    )
  } else {
    warning("Argument is not numeric nor factor: returning NA.")
    out <- NA
    return(out)
  }

  names(out) <- NULL
  return(out)
}

#' @export
data_grid.numeric <- data_grid.vector

#' @export
data_grid.double <- data_grid.vector

#' @export
data_grid.factor <- data_grid.vector

#' @export
data_grid.logical <- data_grid.vector

#' @export
data_grid.character <- data_grid.vector
