#' @importFrom insight export_table
#' @export
print.estimate_contrasts <- function(x, ...) {
  cat(insight::export_table(format(x), ...))
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
  # Colnames
  if ("Size" %in% names(x)) x$Size <- ifelse(x$Size < 1, paste0(insight::format_value(x$Size * 100), "%"), "100%")
  if ("Part" %in% names(x)) x$Part <- insight::format_value(x$Part, protect_integers = TRUE)

  # Title etc.
  info <- attributes(x)

  ## TODO change to "format_table()" after insight 0.11.1 or higher on CRAN
  out <- insight::parameters_table(x, ...)

  # P-value adjustment footer
  if ("adjust" %in% names(info)) {
    if (info$adjust == "none") {
      attr(out, "table_footer") <- c("\np-values are uncorrected.", "blue")
    } else{
      # TODO: activate that once parmaeters is on CRAN
      # footer <- paste0("p-value adjustment method: ", parameters::format_p_adjust(info$adjust))
      attr(out, "table_footer") <- c(paste0("\np-value adjustment method: ", info$adjust), "blue")
    }
  }
  out
}

#' @export
format.estimate_means <- format.estimate_contrasts

#' @export
format.estimate_slopes <- format.estimate_contrasts

#' @export
format.estimate_smooth <- format.estimate_contrasts
