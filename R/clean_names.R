# Clean names -------------------------------------------------------------


#' @keywords internal
.clean_names_frequentist <- function(means, predict = NULL, info = NULL) {
  names(means)[names(means) == "emmean"] <- .guess_estimate_name(predict, info)
  names(means)[names(means) == "response"] <- .guess_estimate_name(predict, info)
  names(means)[names(means) == "prob"] <- "Probability"
  names(means)[names(means) == "estimate"] <- "Difference"
  names(means)[names(means) == "odds.ratio"] <- "Odds_ratio"
  names(means)[names(means) == "ratio"] <- "Ratio"
  names(means)[names(means) == "rate"] <- "Rate"
  names(means)[names(means) == "t.ratio"] <- "t"
  names(means)[names(means) == "z.ratio"] <- "z"
  names(means)[names(means) == "p.value"] <- "p"
  names(means)[names(means) == "lower.CL"] <- "CI_low"
  names(means)[names(means) == "upper.CL"] <- "CI_high"
  names(means)[names(means) == "asymp.LCL"] <- "CI_low"
  names(means)[names(means) == "asymp.UCL"] <- "CI_high"
  means
}


#' @keywords internal
.clean_names_bayesian <- function(means, model, predict, type = "mean") {
  vars <- names(means)[names(means) %in% c("Median", "Mean", "MAP")]
  minfo <- insight::model_info(model, response = 1)

  if (length(vars) == 1) {
    if (type == "contrast") {
      if (minfo$is_logit && predict == "response") {
        names(means)[names(means) == vars] <- "Odds_ratio"
      } else if (minfo$is_poisson && predict == "response") {
        names(means)[names(means) == vars] <- "Ratio"
      } else {
        names(means)[names(means) == vars] <- "Difference"
      }
    } else if (type == "mean") {
      if (minfo$is_logit && predict == "response") {
        names(means)[names(means) == vars] <- "Probability"
      } else if (!is.null(predict) && predict %in% .brms_aux_elements(model)) {
        names(means)[names(means) == vars] <- tools::toTitleCase(predict)
      } else {
        names(means)[names(means) == vars] <- "Mean"
      }
    } else if (predict %in% .brms_aux_elements(model)) {
      names(means)[names(means) == vars] <- tools::toTitleCase(predict)
    } else {
      names(means)[names(means) == vars] <- "Coefficient"
    }
  }

  means$CI <- NULL
  means$ROPE_CI <- NULL
  means$ROPE_low <- NULL
  means$ROPE_high <- NULL
  means$Parameter <- NULL
  means
}
