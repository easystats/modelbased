# back-transform from link-scale? this functions is...
# - only called for means, not contrasts, because for contrasts we rely on
#   the delta-method for SEs on the response scale
# - only called when `type` (i.e. `predict`) is "response" AND the model class
#   has a "link" prediction type
.backtransform_predictions <- function(means, model, predict_args, ci, df = Inf) {
  # In `.get_marginaleffects_type_argument()`, where we set the `predict`
  # aergument, we check whether response-scale is requested - if so, we set
  # predict-type to "link" and now backtransform predictions and CIs
  if (isTRUE(predict_args$backtransform) && all(c("conf.low", "conf.high", "estimate") %in% colnames(means))) {
    # extract link-inverse
    linv <- predict_args$link_inverse
    # if we have a standard error column, we need to back-transform this as well
    if ("std.error" %in% colnames(means)) {
      # define factor for multiplying SE
      alpha <- (1 + ci) / 2
      t_crit <- stats::qt(alpha, df = df)
      # first transform CI, then SE and finally estimates. this order is
      # required - e.g., SE would be wrong if we backtransform estimate first
      means$conf.low <- linv(means$estimate - t_crit * means$std.error)
      means$conf.high <- linv(means$estimate + t_crit * means$std.error)
      means$std.error <- exp(means$estimate) * means$std.error
    } else {
      # for some models, we have no std.error, so just transform CIs
      means$conf.low <- linv(means$conf.low)
      means$conf.high <- linv(means$conf.high)
    }
    means$estimate <- linv(means$estimate)
  }

  means
}


# internal to return possibly bias correct link-function
.link_inverse <- function(model = NULL,
                          bias_correction = FALSE,
                          residual_variance = NULL,
                          verbose = TRUE,
                          ...) {
  if (isTRUE(bias_correction)) {
    dots <- list(...)
    if (!is.null(dots$sigma) && !is.na(dots$sigma)) {
      residual_variance <- dots$sigma^2
    }
    l <- .bias_correction(model, residual_variance, verbose, ...)$linkinv
    if (is.null(l)) {
      l <- insight::link_inverse(model)
    }
  } else {
    l <- insight::link_inverse(model)
  }
  l
}


# apply bias-correction for back-transformation of predictions on the link-scale
# we want sigma^2 (residual_variance) here to calculate the correction
.bias_correction <- function(model = NULL, residual_variance = NULL, verbose = TRUE, ...) {
  # we need a model object
  if (is.null(model)) {
    return(NULL)
  }
  # extract residual variance, if not provided
  if (is.null(residual_variance)) {
    residual_variance <- .get_residual_variance(model, ...) # returns sigma^2
  }
  # we need residual variance
  if (is.null(residual_variance)) {
    if (verbose) {
      insight::format_alert("Could not extract residual variance to apply bias correction. No bias adjustment carried out.") # nolint
    }
    return(NULL)
  }

  # extract current link function
  link <- .safe(insight::get_family(model))
  # we need a link function
  if (is.null(link)) {
    if (verbose) {
      insight::format_alert("Could not extract information about the model's link-function to apply bias correction. No bias adjustment carried out.") # nolint
    }
    return(NULL)
  }

  link$inv <- link$linkinv
  link$der <- link$mu.eta
  link$residual_variance <- residual_variance / 2

  link$der2 <- function(eta) {
    with(link, 1000 * (der(eta + 5e-4) - der(eta - 5e-4)))
  }
  link$linkinv <- function(eta) {
    with(link, inv(eta) + residual_variance * der2(eta))
  }
  link$mu.eta <- function(eta) {
    with(link, der(eta) + 1000 * residual_variance * (der2(eta + 5e-4) - der2(eta - 5e-4)))
  }
  link
}


.get_residual_variance <- function(x, tolerance = 1e-10, ...) {
  if (insight::is_mixed_model(x)) {
    out <- .safe(insight::get_variance_residual(x, tolerance = tolerance))
  } else {
    out <- .safe(insight::get_sigma(x, ci = NULL, no_recursion = TRUE, verbose = FALSE)^2, 0)
    if (!length(out)) {
      out <- 0
    }
  }
  out
}
