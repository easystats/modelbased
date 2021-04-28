
# x <- iris
# x$Sepal.Length <- x$Sepal.Width <- NULL
# x$Dupa <- rep(c("A", "B", "C"), length.out = nrow(x))


#' @export
visualisation_matrix_data.frame <- function(x, target = "all", length = 7, factors = "reference", numerics = "mean", preserve_range = TRUE, reference = x, standardize = FALSE, na.rm = TRUE, ...) {
  # Something

}







#' @keywords internal
.visualisation_matrix_targets <- function(x, length = 7, preserve_range = TRUE, standardize = FALSE, reference = x) {

  if(standardize == TRUE && preserve_range == TRUE) {
    warning("The range will not be preserved if `standardize = TRUE`. This is because the numeric variables are spread according to the SD of the whole sample.")
  }

  # Get all factors / characters
  facs <- lapply(x[!sapply(x, is.numeric)], visualisation_matrix)

  # If no range preservation, then no need to bother with the factors
  if(preserve_range == FALSE) {
    nums <- .visualisation_matrix_dataframe_nums(x, length = length, standardize = standardize, reference = reference)
    out <- expand.grid(c(facs, nums))  # Just combine them

    # If the range by factor combination must be preserved
  } else {
    facs_clusters <- expand.grid(facs)  # Get combinations
    out <- data.frame()
    # Loop through clusters of factors
    for(i in 1:nrow(facs_clusters)) {
      # Find subset of original data corresponding to that combination
      subset <- .match_dataframe(x, to = facs_clusters[i, , drop = FALSE])
      if(nrow(subset) == 0) next  # Skip if no instance of combination
      # Get the matrix of numerics for that particular subset (based on its min and max)
      nums <- .visualisation_matrix_dataframe_nums(subset, length = length, standardize = standardize, reference = reference)
      # rbind it with the rest
      out <- rbind(out, expand.grid(c(as.list(facs_clusters[i, ]), nums)))
    }
  }

  # Reorder columns
  out[names(reference)]
}



# Utils -------------------------------------------------------------------

#' @importFrom stats na.omit
#' @keywords internal
.visualisation_matrix_dataframe_nums <-  function(x, length = 7, standardize = FALSE, reference = x) {
  out <- list()
  for(num in names(x)[sapply(x, is.numeric)]) {
    out[[num]] <- visualisation_matrix(x[[num]], length = length, standardize = standardize, reference = reference[[num]])
  }
  out
}

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





