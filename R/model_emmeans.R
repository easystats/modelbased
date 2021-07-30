#' Easy 'emmeans' and 'emtrends'
#'
#' The \code{model_emmeans} function is a wrapper to facilitate the usage of
#' \code{emmeans::emmeans()} and \code{emmeans::emtrends()}, providing a
#' somewhat simpler and intuitive API to find the specifications and variables of interest.
#' It is meanly made to for the developpers to facilitate the organization and debugging, and end-users should rather use the \code{estimate_*} series of functions.
#'
#' @param model A statistical model.
#' @param fixed A character vector indicating the names of the predictors to be
#'   "fixed" (i.e., maintained), so that the estimation is made at these values.
#' @param transform Is passed to the \code{type} argument in
#'   \code{emmeans::emmeans()}. See
#'   \href{https://CRAN.R-project.org/package=emmeans/vignettes/transformations.html}{this
#'   vignette}. Can be \code{"none"} (default for contrasts), \code{"response"}
#'   (default for means), \code{"mu"}, \code{"unlink"}, \code{"log"}.
#'   \code{"none"} will leave the values on scale of the linear predictors.
#'   \code{"response"} will transform them on scale of the response variable.
#'   Thus for a logistic model, \code{"none"} will give estimations expressed in
#'   log-odds (probabilities on logit scale) and \code{"response"} in terms of
#'   probabilities.
#' @param levels,modulate Deprecated, use \code{at} instead.
#' @param at The predictor variable(s) \emph{at} which to evaluate the desired effect / mean / contrasts. Other predictors of the model that are not included here will be collapsed and "averaged" over (the effect will be estimated across them).
#' @param ... Other arguments passed for instance to \code{\link{visualisation_matrix}}.
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)
#'
#' # By default, 'at' is set to "Species"
#' model_emmeans(model)
#'
#' # Overall mean (close to 'mean(iris$Sepal.Length)')
#' model_emmeans(model, at = NULL)
#'
#' # One can estimate marginal means at several values of a 'modulate' variable
#' model_emmeans(model, at = "Petal.Width", length = 3)
#'
#' # Interactions
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#' model_emmeans(model)
#' model_emmeans(model, at = c("Species", "Petal.Length"), length = 2)
#' @export
model_emmeans <- function(model,
                          at = "auto",
                           fixed = NULL,
                           transform = "response",
                           levels = NULL,
                           modulate = NULL,
                           ...) {

  # Deprecation
  if(!is.null(levels) | !is.null(modulate)) {
    warning("The `levels` and `modulate` arguments are deprecated. Please use `at` instead.")
    at <- c(levels, modulate)
  }

  # check if available
  insight::check_if_installed("emmeans")

  # Guess arguments
  args <- .guess_emmeans_arguments(model, at, fixed, ...)

  # Run emmeans
  estimated <- emmeans::emmeans(
    model,
    specs = args$emmeans_specs,
    at = args$emmeans_at,
    type = transform,
    ...
  )

  attr(estimated, "at") <- args$at
  attr(estimated, "fixed") <- args$fixed
  estimated
}

# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================

#' @keywords internal
.format_emmeans_arguments <- function(model, args, ...) {

  # Create the data_matrix
  # ---------------------------
  data <- insight::get_data(model)
  data <- data[, names(data) != insight::find_response(model), drop = FALSE]

  # Deal with 'at'
  if(is.null(args$at) && is.null(args$contrast)) {
    args$data_matrix <- NULL
  } else {
    if(is.data.frame(args$at)){
      args$data_matrix <- args$at
      args$at <- names(args$at)
    } else if(is.list(args$at)) {
      args$data_matrix <- expand.grid(args$at)
      args$at <- names(args$data_matrix)
    } else {
      if(!is.null(args$at) && all(args$at == "all")) {
        target <- insight::find_predictors(model, effects = "fixed", flatten = TRUE)
        target <- target[!target %in% args$fixed]
      } else {
        target <- c(args$at, args$contrast)
      }
      grid <- visualisation_matrix(data, target = target, ...)
      vars <- attributes(grid)$target_specs$varname
      args$data_matrix <- as.data.frame(grid[vars])
      args$at <- vars[!vars %in% args$contrast] # Replace by cleaned varnames
    }
  }
  # Deal with 'fixed'
  if(!is.null(args$fixed)) {
    fixed <- visualisation_matrix(data[args$fixed], target = NULL, ...)
    if(is.null(args$data_matrix)) {
      args$data_matrix <- fixed
    } else {
      args$data_matrix <- merge(args$data_matrix, fixed)
    }
  }

  # Get 'specs' and 'at'
  # --------------------
  if(is.null(args$data_matrix)) {
    args$emmeans_specs <- ~1
    args$emmeans_at <- NULL
  } else {
    args$emmeans_specs <- names(args$data_matrix)
    args$emmeans_at <- sapply(as.list(args$data_matrix), unique, simplify = FALSE)
  }

  args
}




#' @keywords internal
.guess_emmeans_arguments <- function(model,
                                     at = NULL,
                                     fixed = NULL,
                                     ...) {

  # Gather info
  predictors <- insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)
  data <- insight::get_data(model)

  # Guess arguments
  if (!is.null(at) && is.character(at) && at == "auto") {
    at <- predictors[!sapply(data[predictors], is.numeric)]
    if (!length(at) || is.na(at)) {
      stop("Model contains no categorical factor. Please specify 'at'.")
    }
    message('We selected `at = c(', paste0(paste0('"', at, '"'), collapse = ", "), ')`.')
  }

  args <- list(at = at, fixed = fixed)
  .format_emmeans_arguments(model, args, ...)
}