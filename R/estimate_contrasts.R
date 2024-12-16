#' Estimate Marginal Contrasts
#'
#' Run a contrast analysis by estimating the differences between each level of a
#' factor. See also other related functions such as [estimate_means()]
#' and [estimate_slopes()].
#'
#' @inheritParams estimate_means
#' @inheritParams get_emcontrasts
#' @param p_adjust The p-values adjustment method for frequentist multiple
#' comparisons. Can be one of `"holm"` (default), `"tukey"`, `"hochberg"`,
#' `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"` or `"none"`. See the
#' p-value adjustment section in the `emmeans::test` documentation.
#'
#' @inherit estimate_slopes details
#'
#' @examplesIf all(insight::check_if_installed(c("lme4", "emmeans", "rstanarm"), quietly = TRUE))
#' \dontrun{
#' # Basic usage
#' model <- lm(Sepal.Width ~ Species, data = iris)
#' estimate_contrasts(model)
#'
#' # Dealing with interactions
#' model <- lm(Sepal.Width ~ Species * Petal.Width, data = iris)
#'
#' # By default: selects first factor
#' estimate_contrasts(model)
#'
#' # Can also run contrasts between points of numeric
#' estimate_contrasts(model, contrast = "Petal.Width", length = 4)
#'
#' # Or both
#' estimate_contrasts(model, contrast = c("Species", "Petal.Width"), length = 2)
#'
#' # Or with custom specifications
#' estimate_contrasts(model, contrast = c("Species", "Petal.Width=c(1, 2)"))
#'
#' # Or modulate it
#' estimate_contrasts(model, by = "Petal.Width", length = 4)
#'
#' # Standardized differences
#' estimated <- estimate_contrasts(lm(Sepal.Width ~ Species, data = iris))
#' standardize(estimated)
#'
#' # Other models (mixed, Bayesian, ...)
#' data <- iris
#' data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#'
#' model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)
#' estimate_contrasts(model)
#'
#' data <- mtcars
#' data$cyl <- as.factor(data$cyl)
#' data$am <- as.factor(data$am)
#'
#' model <- rstanarm::stan_glm(mpg ~ cyl * am, data = data, refresh = 0)
#' estimate_contrasts(model)
#' # fix `am` at value 1
#' estimate_contrasts(model, by = c("cyl", "am='1'"))
#'
#' model <- rstanarm::stan_glm(mpg ~ cyl * wt, data = data, refresh = 0)
#' estimate_contrasts(model)
#' estimate_contrasts(model, by = "wt", length = 4)
#'
#' model <- rstanarm::stan_glm(
#'   Sepal.Width ~ Species + Petal.Width + Petal.Length,
#'   data = iris,
#'   refresh = 0
#' )
#' estimate_contrasts(model, by = "Petal.Length", test = "bf")
#' }
#'
#' @return A data frame of estimated contrasts.
#' @export
estimate_contrasts <- function(model,
                               contrast = NULL,
                               by = NULL,
                               transform = "none",
                               ci = 0.95,
                               p_adjust = "holm",
                               method = "pairwise",
                               backend = "emmeans",
                               ...) {
  if (backend == "emmeans") {
    # Emmeans ------------------------------------------------------------------
    estimated <- get_emcontrasts(model,
      contrast = contrast,
      by = by,
      transform = transform,
      method = method,
      adjust = p_adjust,
      ...
    )
    out <- .format_emmeans_contrasts(model, estimated, ci, transform, p_adjust, ...)
    info <- attributes(estimated)
  } else {
    # Marginalmeans ------------------------------------------------------------
    estimated <- get_marginalcontrasts(model,
      contrast = contrast,
      by = by,
      transform = transform,
      method = method,
      p_adjust = p_adjust,
      ci = ci,
      ...
    )
    out <- .format_marginaleffects_contrasts(model, estimated, p_adjust, method, ...)
    ## TODO: needs to be fixed
    info <- list(contrast = contrast, by = by)
  }


  # Table formatting
  attr(out, "table_title") <- c("Marginal Contrasts Analysis", "blue")
  attr(out, "table_footer") <- .estimate_means_footer(
    out,
    info$contrast,
    type = "contrasts",
    p_adjust = p_adjust
  )

  # Add attributes
  attr(out, "model") <- model
  attr(out, "response") <- insight::find_response(model)
  attr(out, "ci") <- ci
  attr(out, "transform") <- transform
  attr(out, "at") <- info$by
  attr(out, "by") <- info$by
  attr(out, "contrast") <- info$contrast
  attr(out, "p_adjust") <- p_adjust

  # Output
  class(out) <- c("estimate_contrasts", "see_estimate_contrasts", class(out))
  out
}



# Table formatting emmeans ----------------------------------------------------


.format_emmeans_contrasts <- function(model, estimated, ci, transform, p_adjust, ...) {
  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    out <- cbind(estimated@grid, bayestestR::describe_posterior(estimated, ci = ci, verbose = FALSE, ...))
    out <- .clean_names_bayesian(out, model, transform, type = "contrast")
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
  names(level_cols) <- c("Level1", "Level2")
  level_cols$Level1 <- gsub(",", " - ", level_cols$Level1, fixed = TRUE)
  level_cols$Level2 <- gsub(",", " - ", level_cols$Level2, fixed = TRUE)

  # Merge levels and rest
  out$contrast <- NULL
  cbind(level_cols, out)
}


.format_marginaleffects_contrasts <- function(model, estimated, p_adjust, method, ...) {
  groups <- attributes(estimated)$by
  contrast <- attributes(estimated)$contrast
  focal_terms <- attributes(estimated)$focal_terms

  estimated <- .p_adjust(model, estimated, p_adjust, ...)

  valid_methods <- c(
    "pairwise", "reference", "sequential", "meandev","meanotherdev",
    "revpairwise", "revreference", "revsequential"
  )
  ## TODO: split Parameter column into levels indicated in "contrast", and filter by "by"
  if (!is.null(method) && is.character(method) && method %in% valid_methods) {

  }

  estimated
}
