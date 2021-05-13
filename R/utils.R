# Find and deal with factor levels ----------------------------------------



#' @keywords internal
.remove_name_level <- function(x, ...) {
  name <- .find_name_level(x, ...)
  x <- sub(name, "", x)
  x <- trimws(x)
  x
}







#' @keywords internal
.find_name_level <- function(x) {
  if (length(unique(x)) == 1) {
    return("Contrast")
  }

  splitted <- strsplit(as.character(x), " ")
  splitted <- data.frame(do.call(rbind, splitted), stringsAsFactors = FALSE)
  uniques <- sapply(splitted, unique)

  lengths <- sapply(uniques, length)
  if (lengths[1] == 1) {
    return(as.character(uniques[[1]]))
  } else if (all(grepl(" - ", x))) {
    return("Contrast")
  } else {
    warning("Couldn't find consistent level name.")
    if (is.null(names(x))) {
      return("X")
    } else {
      return(names(x))
    }
  }
}


#' @keywords internal
.format_names_contrasts <- function(model, levelcols, transform = "response") {
  info <- insight::model_info(model)
  if (transform == "response" & (info$is_logit | info$is_count)) {
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

  # Drop columns
  means$CI <- NULL
  means$ROPE_CI <- NULL
  means$ROPE_low <- NULL
  means$ROPE_high <- NULL
  means$Parameter <- NULL
  means
}


#' @keywords internal
.get_variables_emmeans <- function(estimated) {
  out <- estimated@grid
  names(out)[names(out) == "contrast"] <- "Contrast"
  names(out)[names(out) == ".wgt."] <- "Weight"
  if("Weight" %in% names(out) && length(unique(out$Weight)) == 1) {
    out$Weight <- NULL
  }
  out
}
