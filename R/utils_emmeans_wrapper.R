#' @importFrom emmeans emmeans ref_grid
#' @keywords internal
.emmeans_wrapper <- function(model, levels = NULL, fixed = NULL, modulate = NULL, transform = "response", length = 10, ...) {

  data <- insight::get_data(model)

  # Sanitize fixed
  fixed <- c(fixed, modulate)
  fixed_vars <- .clean_argument(fixed)

  # Remove factors from fixed
  fixed_factors <- NULL
  if(!is.null(fixed)){
    fixed_factors <- fixed_vars[!sapply(data[fixed_vars], is.numeric)]
    fixed_vars <- fixed_vars[!fixed_vars %in% fixed_factors]
  }

  if(!is.null(fixed_factors)){
    fixed_factors <- sapply(fixed_factors, function(i){
      paste0(i, "='", unique(data[[i]])[1], "'")
    }, simplify = TRUE)
    fixed_factors <- as.character(fixed_factors)
    # levels <- c(levels, fixed_factors)
  }


  # Sanitize levels
  levels <- c(levels, modulate)
  levels_vars <- .clean_argument(levels)


  # Get refgrid for each variable separately
  at <- list()
  for(i in 1:length(levels)){
    at[[levels_vars[i]]] <- visualisation_matrix(data, levels[[i]], length = length)[[levels_vars[i]]]
  }


  # Fix for some edgecases (https://github.com/easystats/modelbased/issues/60)
  formula <- insight::find_terms(model, flatten = TRUE)
  for(name in names(at)){
    if(any(grepl(paste0("as.factor(", name), formula, fixed = TRUE))){
      at[[name]] <- as.numeric(levels(at[[name]]))
    }
  }

  # Get emmeans refgrid
  suppressMessages(refgrid <- emmeans::ref_grid(model, at = at, data=data, ...))

  # Run emmeans
  means <- emmeans::emmeans(refgrid, levels_vars, by = fixed_vars, transform = transform, ...)
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
