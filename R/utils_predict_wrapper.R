#' @keywords internal
predict_wrapper <- function(model, ...) {
  UseMethod("predict_wrapper")
}








#' @importFrom stats predict
#' @keywords internal
predict_wrapper.lm <- function(model, newdata = NULL, ci = 0.95, transform = "response", interval = "confidence", ...) {
  prediction <- as.data.frame(
    stats::predict(model,
      newdata = newdata,
      interval = interval,
      type = transform,
      level = ci,
      ...
    )
  )
  if (ncol(prediction) == 1) {
    names(prediction) <- "Predicted"
  }
  prediction
}



#' @importFrom stats qnorm predict
#' @keywords internal
predict_wrapper.glm <- function(model, newdata = NULL, ci = 0.95, transform = "response", ...) {
  transform <- ifelse(transform == FALSE, "link", "response")
  prediction <- as.data.frame(
    stats::predict(model,
      se.fit = TRUE,
      newdata = newdata,
      type = transform,
      level = ci,
      ...
    )
  )

  critval <- stats::qnorm(1 - ((1 - ci) / 2))
  prediction$upr <- prediction$fit + (critval * prediction$se.fit)
  prediction$lwr <- prediction$fit - (critval * prediction$se.fit)

  prediction[c("fit", "lwr", "upr")]
}







#' @keywords internal
predict_wrapper.polr <- function(model, newdata = NULL, ...) {
  as.data.frame(stats::predict(model, newdata, "probs"))
}












#' @keywords internal
predict_wrapper.merMod <- function(model, newdata = NULL, ci = NULL, re.form = NULL, transform = "response", interval = "confidence", ...) {
  # if (!is.null(re.form) && is.na(re.form)) {
  #   if (!is.null(ci)) {
  #     warning("CI cannot be computed for mixed models when no random effects present in data. Provide some data with random effects, or set `ci = NULL`.")
  #     ci <- NULL
  #   }
  # }

  if (is.null(ci)) {
    # type <- ifelse(transform == "response", TRUE, FALSE)

    prediction <- data.frame(
      Predicted = stats::predict(model,
        newdata = newdata,
        re.form = re.form,
        type = transform
      ),
      CI_low = NA,
      CI_high = NA
    )
  } else {
    if (!requireNamespace("merTools", quietly = TRUE)) {
      stop("This function needs `merTools` to be installed. Please install it by running `install.packages('merTools')`.")
    }

    if (transform == "response" && !insight::model_info(model)$is_linear) {
      type <- "probability"
    } else {
      type <- "linear.prediction"
    }

    refgrid <- emmeans::ref_grid(model, at = as.list(newdata), data = newdata)
    prediction <- as.data.frame(predict(refgrid, transform = transform, ci = ci, interval = interval))

    # Clean
    prediction[names(newdata)] <- NULL
    prediction$Predicted <- prediction[, 1]
    prediction$CI_low <- prediction[, grepl("lower.|LCL", names(prediction))]
    prediction$CI_high <- prediction[, grepl("upper.|UCL", names(prediction))]
    prediction[!names(prediction) %in% c("Predicted", "CI_low", "CI_high")] <- NULL

    # prediction <- as.data.frame(
    #   merTools::predictInterval(model,
    #     which = "fixed",
    #     newdata = newdata,
    #     type = type,
    #     stat = "median",
    #     level = ci
    #   )
    # )
  }
  prediction
}



#' @importFrom insight link_inverse
#' @importFrom stats predict
#' @keywords internal
predict_wrapper.glmmTMB <- function(model, newdata = NULL, ci = NULL, re.form = NULL, transform = "response", ...) {
  if (!is.null(re.form)) {
    newdata[insight::find_variables(model, effects = "random")$random] <- NA
  }

  if (is.null(ci)) {
    prediction <- data.frame(
      Predicted = stats::predict(model,
        newdata = newdata,
        re.form = re.form,
        type = transform
      ),
      CI_low = NA,
      CI_high = NA
    )
  } else {
    pr <- stats::predict(model,
      newdata = newdata,
      re.form = re.form,
      type = transform,
      se.fit = TRUE
    )

    ## TODO check if we need linkinverse
    if (transform != "zprob" && transform != "disp") {
      linkinverse <- insight::link_inverse(model)
    }

    prediction <- data.frame(
      Predicted = pr$fit,
      CI_low = pr$fit - (pr$se.fit * stats::qnorm((1 + ci) / 2)),
      CI_high = pr$fit + (pr$se.fit * stats::qnorm((1 + ci) / 2))
    )
  }
  prediction
}


#' @importFrom stats predict
#' @keywords internal
predict_wrapper.gamm <- function(model, newdata = NULL, ci = 0.95, re.form = NULL, transform = "response", interval = "confidence", ...) {
  if (is.null(ci)) {
    prediction <- data.frame(
      Predicted = stats::predict(model$gam,
        newdata = newdata,
        re.form = re.form,
        type = transform
      ),
      CI_low = NA,
      CI_high = NA
    )
  } else {
    pr <- stats::predict(model$gam,
      newdata = newdata,
      re.form = re.form,
      type = transform,
      se.fit = TRUE
    )


    prediction <- data.frame(
      Predicted = pr$fit,
      CI_low = pr$fit - (pr$se.fit * stats::qnorm((1 + ci) / 2)),
      CI_high = pr$fit + (pr$se.fit * stats::qnorm((1 + ci) / 2))
    )
  }
  prediction
}

#' @keywords internal
predict_wrapper.list <- predict_wrapper.gamm  # gamm4