#' Generates predictions for Frequentist models
#'
#' @inheritParams estimate_response.stanreg
#'
#' @examples
#' library(modelbased)
#'
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' estimate_response(model)
#' estimate_link(model)
#'
#' if (require("lme4")) {
#'   model <- lmer(Sepal.Width ~ Petal.Length + (1 | Species), data = iris)
#'   estimate_response(model)
#'   estimate_link(model)
#' }
#' @return A dataframe of predicted values.
#' @export
estimate_response.glm <- function(model, data = NULL, transform = "response", random = TRUE, length = 25, preserve_range = TRUE, predict = "response", ci = 0.95, ...) {
  args <- .estimate_response_init(model, data, transform, random, length, preserve_range, predict, ...)

  # Avoid failure in predict()
  if(is.null(ci)){
    ci <- 0
  }

  prediction <- predict_wrapper(model,
    newdata = args$data,
    ci = ci,
    re.form = args$re.form,
    transform = args$transform,
    interval = args$interval,
    ...
  )

  # Rename
  names(prediction)[names(prediction) == "fit"] <- "Predicted"
  names(prediction)[names(prediction) == "lwr"] <- "CI_low"
  names(prediction)[names(prediction) == "upr"] <- "CI_high"

  # Restore ci=NULL
  if(ci == 0){
    prediction$CI_low <- prediction$CI_high <- ci <- NULL
  }



  # Add predictors
  out <- cbind(args$data, prediction)

  # Restore factor levels
  out <- .restore_factor_levels(out, insight::get_data(model))

  attributes(out) <- c(
    attributes(out),
    list(
      predict = predict,
      ci = ci,
      transform = transform,
      random = random,
      response = insight::find_response(model)
    )
  )

  class(out) <- c("estimate_response", "see_estimate_response", class(out))
  out
}





#' @rdname estimate_response.glm
#' @export
estimate_link.glm <- function(model, data = "grid", transform = "response", random = FALSE, length = 25, preserve_range = TRUE, predict = "link", ci = 0.95, ...) {
  estimate_response(model, data = data, transform = transform, random = random, length = length, preserve_range = preserve_range, predict = predict, ci = ci, ...)
}


#' @export
estimate_response.lm <- estimate_response.glm
#' @export
estimate_link.lm <- estimate_link.glm

#' @export
estimate_response.merMod <- estimate_response.glm
#' @export
estimate_link.merMod <- estimate_link.glm

#' @export
estimate_response.glmmTMB <- estimate_response.glm
#' @export
estimate_link.glmmTMB <- estimate_link.glm

#' @export
estimate_response.polr <- estimate_response.glm
#' @export
estimate_link.polr <- estimate_link.glm

#' @export
estimate_response.gamm <- estimate_response.glm
#' @export
estimate_link.gamm <- estimate_link.glm
