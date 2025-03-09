#' @rdname get_emmeans
#' @examplesIf insight::check_if_installed("emmeans", quietly = TRUE)
#' # Basic usage
#' model <- lm(Sepal.Width ~ Species, data = iris)
#' get_emcontrasts(model)
#'
#' \dontrun{
#' # Dealing with interactions
#' model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
#' # By default: selects first factor
#' get_emcontrasts(model)
#' # Or both
#' get_emcontrasts(model, contrast = c("Species", "Petal.Width"), length = 2)
#' # Or with custom specifications
#' get_emcontrasts(model, contrast = c("Species", "Petal.Width=c(1, 2)"))
#' # Or modulate it
#' get_emcontrasts(model, by = "Petal.Width", length = 4)
#' }
#' @export
get_emcontrasts <- function(model,
                            contrast = NULL,
                            by = NULL,
                            predict = NULL,
                            comparison = "pairwise",
                            transform = NULL,
                            keep_iterations = FALSE,
                            verbose = TRUE,
                            ...) {
  # check if available
  insight::check_if_installed("emmeans")

  ## TODO: remove deprecation warning later
  if (!is.null(transform)) {
    insight::format_warning("Argument `transform` is deprecated. Please use `predict` instead.")
    predict <- transform
  }

  # check whether contrasts should be made for numerics or categorical
  model_data <- insight::get_data(model, source = "mf", verbose = FALSE)
  on_the_fly_factors <- attributes(model_data)$factors

  # Guess arguments
  my_args <- .guess_emcontrasts_arguments(model, contrast, by, verbose, ...)

  # find default response-type
  predict <- .get_emmeans_type_argument(model, predict, type = "contrasts", ...)

  # extract first focal term
  first_focal <- my_args$contrast[1]

  # if first focal term is numeric, we contrast slopes
  if (is.numeric(model_data[[first_focal]]) &&
    !first_focal %in% on_the_fly_factors &&
    # if these are identical, only slopes are contrasted - we need emmeans then
    !identical(my_args$by, my_args$contrast)) {
    # sanity check - contrast for slopes only makes sense when we have a "by" argument
    if (is.null(my_args$by)) {
      insight::format_error("Please specify the `by` argument to calculate contrasts of slopes.") # nolint
    }
    # Run emmeans
    estimated <- emmeans::emtrends(
      model,
      specs = my_args$by,
      var = my_args$contrast,
      type = predict,
      ...
    )
    emm_by <- NULL
  } else {
    # Run emmeans
    estimated <- emmeans::emmeans(
      model,
      specs = my_args$emmeans_specs,
      at = my_args$emmeans_at,
      type = predict,
      ...
    )
    # Find by variables
    emm_by <- my_args$emmeans_specs[!my_args$emmeans_specs %in% my_args$contrast]
    if (length(emm_by) == 0) {
      emm_by <- NULL
    }
  }

  # If means are on the response scale (e.g., probabilities), need to regrid
  if (predict == "response") {
    estimated <- emmeans::regrid(estimated)
  }

  out <- emmeans::contrast(estimated, by = emm_by, method = comparison, ...)

  # for Bayesian model, keep iterations
  if (insight::model_info(model)$is_bayesian) {
    attr(estimated, "posterior_draws") <- insight::get_parameters(estimated)
  } else {
    keep_iterations <- FALSE
  }

  attr(out, "contrast") <- my_args$contrast
  attr(out, "predict") <- predict
  attr(out, "at") <- my_args$by
  attr(out, "by") <- my_args$by
  attr(out, "focal_terms") <- emm_by
  attr(out, "p_adjust") <- list(...)$adjust
  attr(out, "comparison") <- comparison
  attr(out, "transform") <- TRUE
  attr(out, "keep_iterations") <- keep_iterations

  out
}


# =========================================================================
# HELPERS (guess arguments) -----------------------------------------------
# =========================================================================

#' @keywords internal
.guess_emcontrasts_arguments <- function(model,
                                         contrast = NULL,
                                         by = NULL,
                                         verbose = TRUE,
                                         ...) {
  # Gather info
  model_data <- insight::get_data(model, source = "mf", verbose = FALSE)
  predictors <- intersect(
    colnames(model_data),
    insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)
  )

  # Guess arguments
  if (is.null(contrast)) {
    contrast <- predictors[!sapply(model_data[predictors], is.numeric)][1]
    if (!length(contrast) || is.na(contrast)) {
      contrast <- predictors[1]
    }
    if (verbose) {
      insight::format_alert(paste0("No variable was specified for contrast estimation. Selecting `contrast = \"", contrast, "\"`.")) # nolint
    }
  } else if (all(contrast == "all")) {
    contrast <- predictors
  }

  my_args <- list(contrast = contrast, by = by)
  .process_emmeans_arguments(model, args = my_args, data = model_data, ...)
}


# Table formatting emmeans ----------------------------------------------------


.format_emmeans_contrasts <- function(model, estimated, ci, p_adjust, ...) {
  predict <- attributes(estimated)$predict
  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    out <- cbind(estimated@grid, bayestestR::describe_posterior(estimated, ci = ci, verbose = FALSE, ...))
    out <- .clean_names_bayesian(out, model, predict, type = "contrast")
  } else {
    out <- as.data.frame(merge(
      as.data.frame(estimated),
      stats::confint(estimated, level = ci, adjust = p_adjust)
    ))
    out <- .clean_names_frequentist(out)
  }
  out$null <- NULL # introduced in emmeans 1.6.1 (#115)
  out <- datawizard::data_relocate(
    out,
    c("CI_low", "CI_high"),
    after = c("Difference", "Odds_ratio", "Ratio")
  )


  # Format contrasts names
  # Split by either " - " or "/"
  level_cols <- strsplit(as.character(out$contrast), " - |\\/")
  level_cols <- data.frame(do.call(rbind, lapply(level_cols, trimws)))

  # other comparison methods than "pairwise" do not return two columns
  if (ncol(level_cols) == 2) {
    colnames(level_cols) <- c("Level1", "Level2")
    level_cols$Level1 <- gsub(",", " - ", level_cols$Level1, fixed = TRUE)
    level_cols$Level2 <- gsub(",", " - ", level_cols$Level2, fixed = TRUE)
  } else {
    colnames(level_cols)[1] <- "Level"
  }

  # Merge levels and rest
  out$contrast <- NULL
  cbind(level_cols, out)
}
