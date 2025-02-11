# Clean names -------------------------------------------------------------


#' @keywords internal
.clean_names_frequentist <- function(means) {
  names(means)[names(means) == "emmean"] <- "Mean"
  names(means)[names(means) == "response"] <- "Mean"
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

  if (length(vars) == 1) {
    if (type == "contrast") {
      if (insight::model_info(model)$is_logit && predict == "response") {
        names(means)[names(means) == vars] <- "Odds_ratio"
      } else if (insight::model_info(model)$is_poisson && predict == "response") {
        names(means)[names(means) == vars] <- "Ratio"
      } else {
        names(means)[names(means) == vars] <- "Difference"
      }
    } else if (type == "mean") {
      if (insight::model_info(model)$is_logit && predict == "response") {
        names(means)[names(means) == vars] <- "Probability"
      } else if (!is.null(predict) && predict %in% .brms_aux_elements()) {
        names(means)[names(means) == vars] <- tools::toTitleCase(predict)
      } else {
        names(means)[names(means) == vars] <- "Mean"
      }
    } else if (predict %in% .brms_aux_elements()) {
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
