#' @keywords internal
.remove_name_level <- function(x, ...) {
  name <- .find_name_level(x, ...)
  x <- sub(name, "", x)
  x <- trimws(x)
  x
}







#' @keywords internal
.find_name_level <- function(x) {
  if (length(unique(x)) == 1) {
    return("Contrast")
  }

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
