#' @keywords internal
.get_marginalmeans <- function(model,
                               at = "auto",
                               fixed = NULL,
                               transform = "response",
                               ci = 0.95,
                               ...) {

  # check if available
  insight::check_if_installed("marginaleffects")

  # Guess arguments
  args <- .guess_emmeans_arguments(model, at, fixed, ...)

  # Run emmeans
  means <- marginaleffects::marginalmeans(model, variables=args$at, conf_level = ci)

  # TODO: this should be replaced by parameters::parameters(means)
  # Format names
  names(means)[names(means) %in% c("conf.low")] <- "CI_low"
  names(means)[names(means) %in% c("conf.high")] <- "CI_high"
  names(means)[names(means) %in% c("std.error")] <- "SE"
  names(means)[names(means) %in% c("marginalmean")] <- "Mean"
  names(means)[names(means) %in% c("p.value")] <- "p"
  names(means)[names(means) %in% c("statistic")] <- ifelse(insight::find_statistic(model) == "t-statistic", "t", "statistic")

  # Format terms
  term <- unique(means$term)  # Get name of variable
  if(length(term) > 1) {
    insight::format_error("marignalmeans backend can currently only deal with one 'at' variable.")
  }
  names(means)[names(means) %in% c("value")] <- term  # Replace 'value' col by var name
  means$term <- NULL

  means
}
