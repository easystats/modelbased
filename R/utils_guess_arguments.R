#' @importFrom insight find_predictors get_data
#' @keywords internal
.guess_arguments <- function(model, levels = NULL, fixed = NULL, modulate = NULL) {
  x <- .guess_arguments_levels(model, levels = levels)
  levels <- x$levels

  if (!is.null(fixed)) {
    fixed <- unique(c(fixed, x$fixed))
    levels <- levels[!levels %in% c(fixed)]
  }


  # Prevent repetitions
  if (!is.null(modulate)) {
    if (!is.null(fixed)) {
      fixed <- fixed[!fixed %in% c(modulate)]
    }
    levels <- levels[!levels %in% c(modulate)]
  }

  list(
    "levels" = levels,
    "fixed" = fixed,
    "modulate" = modulate
  )
}








#' @keywords internal
.guess_arguments_levels <- function(model, levels = NULL) {

  # Initialize a fixed part that might be modified below
  fixed <- NULL

  # If NULL: get predictors that are factors
  if (is.null(levels)) {
    levels <- insight::find_predictors(model)$conditional
    levels <- levels[!sapply(insight::get_data(model)[levels], is.numeric)]

    # If levels is formula
  } else if (class(levels) == "formula") {

    # Transform to string and keep predictors
    levels <- as.character(tail(as.list(levels), 1))
    # Remove white spaces
    levels <- gsub(" ", "", levels, fixed = TRUE)
    # Separate fixed from interactions
    components <- unlist(strsplit(levels, "+", fixed = TRUE))
    interactions <- components[grepl("*", components, fixed = TRUE)]
    fixed <- components[!grepl("*", components, fixed = TRUE)]

    levels <- unlist(strsplit(interactions, c("*", ":", "/"), fixed = TRUE))
  }

  if (length(levels) == 0) {
    stop("No suitable factor levels detected.")
  }

  list("levels" = levels, "fixed" = fixed)
}


#' @keywords internal
.clean_argument <- function(arg) {
  if (!is.null(arg)) {
    unlist(lapply(strsplit(arg, "=", fixed = TRUE), function(i) i[[1]]))
  } else {
    arg
  }
}
