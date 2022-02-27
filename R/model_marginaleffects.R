#' Easy marginaleffects
#'
#' Modelbased-like API to create \pkg{marginaleffects} objects. This is Work-in-progress.
#'
#' @inheritParams model_emtrends
#'
#' @examples
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#' model_marginaleffects(model, trend = "Petal.Length", at = "Species")
#' model_marginaleffects(model, trend = "Petal.Length", at = "Petal.Length")
#' model_marginaleffects(model, trend = "Petal.Length", at = c("Species", "Petal.Length"))
#'
#'
#' @export
model_marginaleffects <- function(model,
                          trend = NULL,
                          at = NULL,
                          fixed = NULL,
                          ...) {


  # check if available
  insight::check_if_installed("marginaleffects")

  # Guess arguments
  if(is.null(trend)) {
    trend <- insight::find_predictors(model, effects = "fixed", flatten = TRUE)[1]
    warning(paste0("'trend' cannot be NULL when using marginaleffects to compute overall effects. Selecting '", trend, "'"))
  }
  if(is.null(at)) {
    at <- insight::find_predictors(model, effects = "fixed", flatten = TRUE)
    at <- at[!at %in% trend]
  }
  newdata <- insight::get_datagrid(model, at = at, ...)

  fixed <- names(newdata)[!names(newdata) %in% c(at, trend)]
  if(length(fixed) == 0) fixed <- NULL

  # Compute stuff
  estimated <- marginaleffects::marginaleffects(model, variables = trend, newdata = newdata, ...)

  attr(estimated, "trend") <- trend
  attr(estimated, "at") <- at
  attr(estimated, "fixed") <- fixed
  estimated
}