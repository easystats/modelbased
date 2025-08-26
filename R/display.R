#' @rdname print.estimate_contrasts
#' @export
display.estimate_contrasts <- function(object,
                                       select = NULL,
                                       include_grid = NULL,
                                       full_labels = NULL,
                                       format = "markdown",
                                       ...) {
  # Process argument ---------------------------------------------------------
  # --------------------------------------------------------------------------

  format <- .display_default_format(format)

  # set defaults
  if (is.null(select)) {
    select <- getOption("modelbased_select", NULL)
  }
  if (is.null(include_grid)) {
    include_grid <- getOption("modelbased_include_grid", FALSE)
  }
  if (is.null(full_labels)) {
    full_labels <- getOption("modelbased_full_labels", TRUE)
  }

  fun_args <- list(
    x = object,
    select = select,
    include_grid = include_grid,
    full_labels = full_labels
  )

  if (format %in% c("html", "tt")) {
    fun_args$backend <- format
    do.call(print_html, c(fun_args, list(...)))
  } else {
    do.call(print_md, c(fun_args, list(...)))
  }
}

#' @export
display.estimate_means <- display.estimate_contrasts

#' @export
display.estimate_slopes <- display.estimate_contrasts

#' @export
display.estimate_smooth <- display.estimate_contrasts

#' @export
display.estimate_predicted <- display.estimate_contrasts

#' @export
display.visualisation_matrix <- display.estimate_contrasts

#' @export
display.estimate_grouplevel <- display.estimate_contrasts

.display_default_format <- function(format) {
  format <- getOption("easystats_display_format", "markdown")
  insight::validate_argument(format, c("markdown", "html", "md", "tt"))
}
