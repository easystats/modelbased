#' @importFrom insight export_table
#' @export
print.estimate_contrasts <- function(x, ...) {

  info <- attributes(x)

  # P-value adjustment title
  if("adjust" %in% names(info)){
    if(info$adjust == "none"){
      footer <- "p-values are uncorrected."
    } else{

      # Because it's a new function
      footer = tryCatch({
        paste0("p-value adjustment method: ", parameters::format_p_adjust(info$adjust))
      }, error = function(e) {
        paste0("p-value adjustment method: ", info$adjust)
      })
    }
  } else{
    footer <- NULL
  }

  # Out
  cat(insight::export_table(format(x), footer=footer, ...))
  invisible(x)
}

#' @export
print.estimate_means <- print.estimate_contrasts

#' @export
print.estimate_slopes <- print.estimate_contrasts

#' @export
print.estimate_smooth <- print.estimate_contrasts



# Format ------------------------------------------------------------------



#' @importFrom insight format_value parameters_table
#' @export
format.estimate_contrasts <- function(x, ...) {
  orig_x <- x
  if ("Size" %in% names(x)) x$Size <- ifelse(x$Size < 1, paste0(insight::format_value(x$Size * 100), "%"), "100%")
  if ("Part" %in% names(x)) x$Part <- insight::format_value(x$Part, protect_integers = TRUE)
  ## TODO change to "format_table()" after insight 0.11.1 or higher on CRAN
  insight::parameters_table(x, ...)
}

#' @export
format.estimate_means <- format.estimate_contrasts

#' @export
format.estimate_slopes <- format.estimate_contrasts

#' @export
format.estimate_smooth <- format.estimate_contrasts
