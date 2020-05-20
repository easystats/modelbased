#' @importFrom insight find_predictors get_data
#' @keywords internal
.guess_arguments <- function(model, levels = NULL, fixed = NULL, modulate = NULL) {


  # Try to find factors if none is specified
  if (is.null(levels)) {
    levels <- insight::find_predictors(model)$conditional
    numeric <- levels[sapply(insight::get_data(model)[levels], is.numeric)]
    levels <- levels[!levels %in% numeric]
  } else {
    numeric <- NULL
  }

  # Fix all the other variables
  if (!is.null(fixed)) {
    fixed <- unique(c(fixed, numeric))
    levels <- levels[!levels %in% fixed]
    if (!is.null(modulate)) {
      fixed <- fixed[!fixed %in% c(modulate)]
    }
  }

  if (length(levels) == 0) {
    stop("No suitable factor levels detected.")
  }

  list(
    "levels" = levels,
    "fixed" = fixed,
    "modulate" = modulate
  )
}





# Emmeans wrapper ---------------------------------------------------------



#' @importFrom emmeans emmeans ref_grid
#' @keywords internal
# .emmeans_wrapper <- function(model, levels = NULL, fixed = NULL, modulate = NULL, transform = "response", length = 10, type = "mean", ...) {
#
#   if (is.null(modulate)) {
#     means <- emmeans::emmeans(model, levels, by = fixed, transform = transform, ...)
#
#   } else {
#     at <- insight::get_data(model)[c(levels, modulate)]
#     at <- sapply(at, visualisation_matrix, length = length, simplify = FALSE)
#     means <- emmeans::ref_grid(model, at = at, by = c(fixed, modulate))
#
#     if (type == "mean") {
#       means <- emmeans::emmeans(means, c(levels, modulate), transform = transform, ...)
#     } else {
#       means <- emmeans::emmeans(means, levels, by = c(fixed, modulate), transform = transform, ...)
#     }
#   }
#
#   means
# }


#' @importFrom emmeans emmeans ref_grid
#' @keywords internal
.emmeans_wrapper <- function(model, levels = NULL, fixed = NULL, modulate = NULL, transform = "response", length = 10, ...) {

  levels <- c(levels, modulate)
  fixed <- c(fixed, modulate)

  # Get emmeans refgrid
  at <- insight::get_data(model)[levels]
  at <- sapply(at, visualisation_matrix, length = length, simplify = FALSE)

  # Fix for some edgecases (https://github.com/easystats/modelbased/issues/60)
  formula <- insight::find_terms(model, flatten = TRUE)
  for(name in names(at)){
    if(any(grepl(paste0("as.factor(", name), formula, fixed = TRUE))){
      at[[name]] <- as.numeric(levels(at[[name]]))
    }
  }

  suppressMessages(refgrid <- emmeans::ref_grid(model, at = at))

  # Run emmeans
  means <- emmeans::emmeans(refgrid, levels, by = fixed, transform = transform, ...)

  means
}



# Utils frequentist cleaning ----------------------------------------------



#' @keywords internal
.clean_emmeans_frequentist <- function(means) {
  names(means)[names(means) == "emmean"] <- "Mean"
  names(means)[names(means) == "prob"] <- "Probability"
  names(means)[names(means) == "estimate"] <- "Difference"
  names(means)[names(means) == "t.ratio"] <- "t"
  names(means)[names(means) == "z.ratio"] <- "z"
  names(means)[names(means) == "p.value"] <- "p"
  names(means)[names(means) == "lower.CL"] <- "CI_low"
  names(means)[names(means) == "upper.CL"] <- "CI_high"
  names(means)[names(means) == "asymp.LCL"] <- "CI_low"
  names(means)[names(means) == "asymp.UCL"] <- "CI_high"
  means
}
