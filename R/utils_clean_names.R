# Utils frequentist cleaning ----------------------------------------------



#' @keywords internal
.clean_names_frequentist <- function(means) {
  names(means)[names(means) == "emmean"] <- "Mean"
  names(means)[names(means) == "response"] <- "Mean"
  names(means)[names(means) == "prob"] <- "Probability"
  names(means)[names(means) == "estimate"] <- "Difference"
  names(means)[names(means) == "odds.ratio"] <- "Odds_ratio"
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
.clean_names_bayesian <- function(means, model, transform, type = "mean") {
  vars <- names(means)[names(means) %in% c("Median", "Mean", "MAP")]
  if (length(vars) == 1) {
    if (type == "contrast") {
      if (insight::model_info(model)$is_logit & transform == "response") {
        names(means)[names(means) == vars] <- "Odds_Ratio"
      } else {
        names(means)[names(means) == vars] <- "Difference"
      }
    } else {
      if (insight::model_info(model)$is_logit & transform == "response") {
        names(means)[names(means) == vars] <- "Probability"
      } else {
        names(means)[names(means) == vars] <- "Mean"
      }
    }
  }

  means
}

#' @keywords internal
.format_names_contrasts <- function(model, levelcols, transform = "response") {
  if (transform == "response" & insight::model_info(model)$is_logit) {
    levelcols <- strsplit(as.character(levelcols$Contrast), "/")
  } else {
    levelcols <- strsplit(as.character(levelcols$Contrast), " - ")
  }
  levelcols <- lapply(levelcols, trimws)

  levelcols <- data.frame(do.call(rbind, levelcols))
  names(levelcols) <- c("Level1", "Level2")
  levelcols$Level1 <- gsub(",", " - ", levelcols$Level1)
  levelcols$Level2 <- gsub(",", " - ", levelcols$Level2)
  levelcols
}
