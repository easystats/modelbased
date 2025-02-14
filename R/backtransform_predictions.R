.backtransform_predictions <- function(means, model, predict_args, ci, df = Inf) {
  if (isTRUE(predict_args$backtransform)) {
    linv <- predict_args$link_inverse
    alpha <- (1 + ci) / 2
    t_crit <- stats::qt(alpha, df = df)
    means$conf.low <- linv(means$estimate - t_crit * means$std.error)
    means$conf.high <- linv(means$estimate + t_crit * means$std.error)
    means$std.error <- exp(means$estimate) * means$std.error
    means$estimate <- linv(means$estimate)
  }

  means
}
