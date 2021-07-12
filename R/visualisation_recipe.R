#' Prepare objects for visualisation
#'
#' This function prepares objects for visualisation by returning a list of layers with data and geoms that can be easily plotted using for instance \code{ggplot2}. See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=visualisation_recipe.estimate_predicted]{modelbased} (\code{estimate_means}, \code{estimate_contrasts}, \code{estimate_slopes}, \code{estimate_predicted}, \code{estimate_grouplevel})}
#' }
#'
#' @param x An easystats object.
#' @param ... Other arguments passed to other functions.
#'
#' @export
visualisation_recipe <- function(x, ...) {
  UseMethod("visualisation_recipe")
}


#' @export
print.visualisation_recipe <- function(x, ...) {
  for (i in 1:length(x)) {
    l <- x[[paste0("l", i)]]
    insight::print_color(paste0("Layer ", i, "\n--------\n"), "blue")
    insight::print_color(paste0("Geom type: ", l$geom, "\n"), "yellow")

    elements <- names(l)[!sapply(l, is.null)]

    # Loop through all elements of list
    for (element in elements[elements != "geom"]) {

      # Print element name
      if (element == "aes") {
        cat("aes_string(\n")
      } else {
        cat(paste0(element, " = "))
      }

      # Print element
      if (element == "data") {
        cat(paste0("[", paste0(dim(l$data), collapse = " x "), "]"))
      } else if (element == "aes") {
        for (aes in names(l$aes)) {
          if (!is.null(l$aes[[aes]])) {
            if (is.character(l$aes[[aes]])) {
              cat(paste0("  ", aes, " = '", l$aes[[aes]], "'\n"))
            } else {
              cat(paste0("  ", aes, " = ", l$aes[[aes]], "\n"))
            }
          }
        }
        cat(")")
      } else {
        if (is.character(l[[element]]) || is.numeric(l[[element]]) || is.factor(l[[element]])) {
          if (is.character(l[[element]])) {
            cat(paste0("'", l[[element]], "'"))
          } else {
            cat(l[[element]])
          }
        } else {
          cat(paste0("class: ", class(l[[element]]), collapse = "/"))
        }
      }
      cat("\n")
    }
    cat("\n")
  }
}

#' @importFrom graphics plot
#' @export
plot.visualisation_recipe <- function(x, ...) {
  insight::check_if_installed("see")

  ggplot2::ggplot(data = attributes(x)$data) +
    see::geoms_from_list(x, ...)
}
