#' Easy Creation of 'emmeans' Marginal Means Objects
#'
#' The \code{model_emmeans} function is a wrapper to facilitate the usage of \code{emmeans::emmeans()}, providing a somewhat simpler and smart API to find the variables of interest.
#'
#' @param model A statistical model.
#' @param levels A character vector or formula specifying the names of the
#'   predicting factors over which to estimate means or contrasts.
#' @param fixed A character vector indicating the names of the predictors to be
#'   "fixed" (i.e., maintained), so that the estimation is made at these values.
#' @param modulate A character vector indicating the names of a numeric variable
#'   along which the means or the contrasts will be estimated. Other arguments
#'   from \code{\link{visualisation_matrix}}, such as \code{length} to adjust the
#'   number of data points.
#' @param transform Is passed to the \code{type} argument in \code{emmeans::emmeans()}. See \href{https://cran.r-project.org/web/packages/emmeans/vignettes/transformations.html}{this vignette}. Can be \code{"none"} (default for contrasts),
#'   \code{"response"} (default for means), \code{"mu"}, \code{"unlink"},
#'   \code{"log"}. \code{"none"} will leave the values on scale of the linear
#'   predictors. \code{"response"} will transform them on scale of the response
#'   variable. Thus for a logistic model, \code{"none"} will give estimations
#'   expressed in log-odds (probabilities on logit scale) and \code{"response"}
#'   in terms of probabilities.
#' @param ... Other arguments passed for instance to \code{\link{visualisation_matrix}}.
#'
#' @return An \code{emmeans} object.
#' @examples
#' library(modelbased)
#'
#' model <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)
#'
#' # By default, 'levels' is set to "Species"
#' model_emmeans(model)
#'
#' # One can estimate marginal means at several values of a 'modulate' variable
#' model_emmeans(model, modulate = "Petal.Width", length = 3)
#'
#' @export
model_emmeans <- function(model,
                          levels = NULL,
                          fixed = NULL,
                          modulate = NULL,
                          transform = "response",
                          ...) {

  # Guess arguments
  args <- .guess_arguments(model, levels = levels, fixed = fixed, modulate = modulate)
  levels <- args$levels

  # check if available
  insight::check_if_installed("emmeans")

  data <- insight::get_data(model)

  # Sanitize fixed
  fixed <- c(args$fixed, args$modulate)
  fixed_vars <- .clean_argument(fixed)

  # Remove factors from fixed
  fixed_factors <- NULL
  fixed_factors_vars <- NULL
  if (!is.null(fixed)) {
    isfactor <- !sapply(data[fixed_vars], is.numeric)
    fixed_factors <- fixed[isfactor]
    fixed_factors_vars <- fixed_vars[isfactor]
    fixed_vars <- fixed_vars[!fixed_vars %in% fixed_factors_vars]
    if (length(fixed_vars) == 0) fixed_vars <- NULL
    if (length(fixed_factors_vars) == 0) fixed_factors_vars <- NULL
  }

  if (!is.null(fixed_factors_vars)) {
    for (i in 1:length(fixed_factors_vars)) {
      if (fixed_factors[i] != fixed_factors_vars[i]) {
        fixed_factors_vars[i] <- fixed_factors[i]
      } else {
        fixed_factors_vars[i] <- paste0(fixed_factors_vars[i], "='", unique(data[[fixed_factors_vars[i]]])[1], "'")
      }
    }
    levels <- c(levels, fixed_factors_vars)
  }


  # Sanitize levels
  levels <- c(levels, modulate)
  levels_vars <- .clean_argument(levels)


  # Get refgrid for each variable separately
  at <- list()
  for (i in 1:length(levels)) {
    at[[levels_vars[i]]] <- visualisation_matrix(data, levels[[i]], ...)[[levels_vars[i]]]
  }


  # Fix for some edgecases (https://github.com/easystats/modelbased/issues/60)
  formula <- insight::find_terms(model, flatten = TRUE)
  for (name in names(at)) {
    if (any(grepl(paste0("as.factor(", name), formula, fixed = TRUE))) {
      at[[name]] <- as.numeric(levels(at[[name]]))
    }
  }

  # Get emmeans refgrid
  suppressMessages(refgrid <- emmeans::ref_grid(
    model,
    at = at,
    data = data,
    nesting = NULL,
    ...
  ))

  # Run emmeans
  means <- emmeans::emmeans(refgrid, levels_vars, by = fixed_vars, type = transform, ...)

  means
}


# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================


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

#' @importFrom utils tail
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
    levels <- as.character(utils::tail(as.list(levels), 1))
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



# Cleaning ----------------------------------------------------------------


#' @keywords internal
.clean_argument <- function(arg) {
  if (!is.null(arg)) {
    unlist(lapply(strsplit(arg, "=", fixed = TRUE), function(i) i[[1]]))
  } else {
    arg
  }
}




