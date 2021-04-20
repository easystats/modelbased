
# x <- iris
# x$Sepal.Length <- x$Sepal.Width <- NULL
# x$Dupa <- rep(c("A", "B", "C"), length.out = nrow(x))




#' @keywords internal
.visualisation_matrix_dataframe <- function(x, length = 7, standardize = FALSE, reference = x, preserve_range = TRUE) {

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
      subset <- .match_dataframe(x, to = facs_clusters[i, ])
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






