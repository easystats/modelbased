# back-transform from link-scale? this functions is...
# - only called for means, not contrasts, because for contrasts we rely on
#   the delta-method for SEs on the response scale
# - only called when `type` (i.e. `predict`) is "response" AND the model class
#   has a "link" prediction type
.backtransform_predictions <- function(means, model, predict_args, ci, df = Inf) {
  # In `.get_marginaleffects_type_argument()`, where we set the `predict`
  # aergument, we check whether response-scale is requested - if so, we set
  # predict-type to "link" and now backtransform predictions and CIs
  if (isTRUE(predict_args$backtransform)) {
    # extract link-inverse
    linv <- predict_args$link_inverse
    # define factor for multiplying SE
    alpha <- (1 + ci) / 2
    t_crit <- stats::qt(alpha, df = df)
    # first transform CI, then SE and finally estimates. this order is
    # required - e.g., SE would be wrong if we backtransform estimate first
    means$conf.low <- linv(means$estimate - t_crit * means$std.error)
    means$conf.high <- linv(means$estimate + t_crit * means$std.error)
    means$std.error <- exp(means$estimate) * means$std.error
    means$estimate <- linv(means$estimate)
  }

  means
}
