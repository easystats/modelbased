
# x <- iris
# x$Sepal.Length <- x$Sepal.Width <- NULL
# x$Dupa <- rep(c("A", "B", "C"), length.out = nrow(x))
# target = "all"; length = 7; factors = "reference"; numerics = "mean"; preserve_range = TRUE; reference = x; standardize = FALSE; na.rm = TRUE


#' @export
visualisation_matrix_data.frame <- function(x, target = "all", length = 7, factors = "reference", numerics = "mean", preserve_range = TRUE, reference = x, standardize = FALSE, na.rm = TRUE, ...) {

  # Sanity checks
  if(standardize == TRUE && preserve_range == TRUE) {
    warning("The range will not be preserved if `standardize = TRUE`. This is because the numeric variables are spread according to the SD of the whole sample.")
  }

  # Valid target argument
  if (all(target == "all") || ncol(x) == 1 || all(names(x) %in% c(target))) {
    target <- names(x)
  }

  # Deal with targets ==========================================================

  # Find eventual user-defined specifications for each target
  specs <- do.call(.visualisation_matrix_clean_target, list(x = x, target = target))
  specs$is_factor <- sapply(x[specs$varname], function(x) is.factor(x) || is.character(x))
  targets_with_expression <- specs[!is.na(specs$expression), ]
  targets_to_make <- specs[is.na(specs$expression), ]

  # Create target dataframe of factors -----------------------------------------
  facs <- list()
  for(fac in specs[specs$is_factor == TRUE, "varname"]) {
    if(is.na(specs[specs$varname == fac, "expression"])) {
      facs[[fac]] <- visualisation_matrix(x[[fac]])
    } else{
      facs[[fac]] <- eval(parse(text = specs[specs$varname == fac, "expression"]))
    }
  }
  facs <- expand.grid(facs)

  # Create target dataframe of numerics ----------------------------------------
  targets <- data.frame()
  # Loop through the combinations of factors of factors
  for(i in 1:nrow(facs)) {

    # Find subset of original data corresponding to that combination (or original dataframe)
    if(preserve_range == TRUE) {
      subset <- .match_dataframe(x, to = facs[i, , drop = FALSE])
      if(nrow(subset) == 0) next  # Skip if no instance of combination
    } else {
      subset <- x
    }

    # Loop through all numerics
    nums <- list()
    for(num in specs[specs$is_factor == FALSE, "varname"]) {
      if(is.na(specs[specs$varname == num, "expression"])) {
        facs[[num]] <- visualisation_matrix(subset[[num]], length = length, standardize = standardize, reference = reference[[num]])
      } else{
        facs[[num]] <- eval(parse(text = specs[specs$varname == num, "expression"]))
      }
    }
    nums <- expand.grid(nums)
    targets <- rbind(targets, cbind(facs[i, ], nums, row.names = NULL))
  }

  # Deal with the rest =========================================================



}












# Utils -------------------------------------------------------------------


#' @keywords internal
.match_dataframe <- function(x, to) {
  for(col in names(to)) {
    x <- x[x[[col]] %in% to[[col]], ]
  }
  x
}







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



#' @keywords internal
.visualisation_matrix_preserve_range <- function(grid, x) {

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


