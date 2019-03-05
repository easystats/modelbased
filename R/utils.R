#' @keywords internal
.restore_factor_levels <- function(means, data){
  original_factors <- data[sapply(data, is.factor)]

  for(i in names(means)){
    if(i %in% names(original_factors)){
      means[[i]] <- factor(means[[i]], levels = levels(original_factors[[i]]))
    }
  }

  return(means)
}













#' @keywords internal
.remove_name_level <- function(x){
  name <- .find_name_level(x)
  x <- sub(name, "", x)
  x <- trimws(x)
  return(x)
}







#' @keywords internal
.find_name_level <- function(x){
  splitted <- strsplit(as.character(x), " ")
  splitted <- data.frame(do.call(rbind, splitted), stringsAsFactors = FALSE)
  uniques <- sapply(splitted, unique)

  lengths <-  sapply(uniques, length)
  if(lengths[1] == 1){
    return(as.character(uniques[[1]]))
  } else{
    warning("Couldn't find consistent level name.")
    if(is.null(names(x))){
      return("X")
    } else{
      return(names(x))
    }
  }
}