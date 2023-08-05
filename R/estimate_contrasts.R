#' Estimate Marginal Contrasts
#'
#' Run a contrast analysis by estimating the differences between each level of a
#' factor. See also other related functions such as [estimate_means()]
#' and [estimate_slopes()].
#'
#' @inheritParams estimate_means
#' @inheritParams get_emcontrasts
#' @param p_adjust The p-values adjustment method for frequentist multiple
#'   comparisons. Can be one of "holm" (default), "tukey", "hochberg", "hommel",
#'   "bonferroni", "BH", "BY", "fdr" or "none". See the p-value adjustment
#'   section in the `emmeans::test` documentation.
#' @param adjust Deprecated in favour of `p_adjust`.
#' @param effectsize Desired measure of standardized effect size, one of "none"
#' (default), "emmeans", "marginal", or "bootES".
#' @param bootES_type Specifies the type of effect-size measure to
#' estimate when using `effectsize = "bootES"`. One of `c("unstandardized",
#' "cohens.d", "hedges.g", "cohens.d.sigma", "r", "akp.robust.d")`. See`
#' effect.type` argument of [bootES::bootES] for details.
#' @param bootstraps The number of bootstrap resamples to perform.
#'
#' @inherit estimate_slopes details
#'
#' @section Effect Size: By default, `estimate_contrasts` reports no
#' standardized effect size on purpose. Should one request one, some things
#' are to keep in mind. As the authors of `emmeans` write, "There is
#' substantial disagreement among practitioners on what is the appropriate
#' sigma to use in computing effect sizes; or, indeed, whether any effect-size
#' measure is appropriate for some situations. The user is completely
#' responsible for specifying appropriate parameters (or for failing to do
#' so)."
#'
#' In particular, effect size method `"bootES"` does not correct
#' for covariates in the model, so should probably only be used when there is
#' just one categorical predictor (with however many levels). Some believe that
#' if there are multiple predictors or any covariates, it is important to
#' re-compute sigma adding back in the response variance associated with the
#' variables that aren't part of the contrast.
#'
#' `effectsize = "emmeans"` uses [emmeans::eff_size] with
#' `sigma = stats::sigma(model)`, `edf = stats::df.residual(model)` and
#' `method = "identity")`. This standardizes using the MSE (sigma). Some believe
#' this works when the contrasts are the only predictors in the model, but not
#' when there are covariates. The response variance accounted for by the
#' covariates should not be removed from the SD used to standardize. Otherwise,
#' _d_ will be overestimated.
#'
#' `effectsize = "marginal"` uses the following formula to compute effect
#' size: `d_adj <- t * se_b / sigma * sqrt(1 - R2_cov)`. This standardized
#' using the response SD with only the between-groups variance on the focal
#' factor/contrast removed. This allows for groups to be equated on their
#' covariates, but creates an appropriate scale for standardizing the response.
#'
#' `effectsize = "bootES"` uses bootstrapping (defaults to a low value of
#' 200) through [bootES::bootES]. Adjust for contrasts, but not for covariates.
#'
#' @examplesIf require("emmeans", quietly = TRUE)
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
#' # Can fixate the numeric at a specific value
#' estimate_contrasts(model, fixed = "Petal.Width")
#'
#' # Or modulate it
#' estimate_contrasts(model, at = "Petal.Width", length = 4)
#'
#' # Standardized differences
#' estimated <- estimate_contrasts(lm(Sepal.Width ~ Species, data = iris))
#' standardize(estimated)
#'
#' @examplesIf require("lme4", quietly = TRUE) && require("emmeans", quietly = TRUE)
#' # Other models (mixed, Bayesian, ...)
#' data <- iris
#' data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#'
#' model <- lme4::lmer(Sepal.Width ~ Species + (1 | Petal.Length_factor), data = data)
#' estimate_contrasts(model)
#'
#' @examplesIf require("rstanarm", quietly = TRUE) && require("emmeans", quietly = TRUE)
#' library(rstanarm)
#'
#' data <- mtcars
#' data$cyl <- as.factor(data$cyl)
#' data$am <- as.factor(data$am)
#' \dontrun{
#' model <- stan_glm(mpg ~ cyl * am, data = data, refresh = 0)
#' estimate_contrasts(model)
#' estimate_contrasts(model, fixed = "am")
#'
#' model <- stan_glm(mpg ~ cyl * wt, data = data, refresh = 0)
#' estimate_contrasts(model)
#' estimate_contrasts(model, fixed = "wt")
#' estimate_contrasts(model, at = "wt", length = 4)
#'
#' model <- stan_glm(Sepal.Width ~ Species + Petal.Width + Petal.Length, data = iris, refresh = 0)
#' estimate_contrasts(model, at = "Petal.Length", test = "bf")
#' }
#'
#' @return A data frame of estimated contrasts.
#' @export
estimate_contrasts <- function(model,
                               contrast = NULL,
                               at = NULL,
                               fixed = NULL,
                               transform = "none",
                               ci = 0.95,
                               p_adjust = "holm",
                               method = "pairwise",
                               adjust = NULL,
                               effectsize = "none",
                               bootstraps = 200,
                               bootES_type = "cohens.d",
                               ...) {
  # Deprecation
  if (!is.null(adjust)) {
    insight::format_warning("The `adjust` argument is deprecated. Please write `p_adjust` instead.")
    p_adjust <- adjust
  }

  # Run emmeans
  estimated <- get_emcontrasts(model,
    contrast = contrast,
    at = at,
    fixed = fixed,
    transform = transform,
    method = method,
    adjust = p_adjust,
    ...
  )

  info <- attributes(estimated)

  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    contrasts <- bayestestR::describe_posterior(estimated, ci = ci, ...)
    contrasts <- cbind(estimated@grid, contrasts)
    contrasts <- .clean_names_bayesian(contrasts, model, transform, type = "contrast")
  } else {
    contrasts <- as.data.frame(merge(
      as.data.frame(estimated),
      stats::confint(estimated, level = ci, adjust = p_adjust)
    ))
    contrasts <- .clean_names_frequentist(contrasts)
  }
  contrasts$null <- NULL # introduced in emmeans 1.6.1 (#115)
  contrasts <- datawizard::data_relocate(
    contrasts,
    c("CI_low", "CI_high"),
    after = c("Difference", "Odds_ratio", "Ratio")
  )


  # Format contrasts names
  # Split by either " - " or "/"
  level_cols <- strsplit(as.character(contrasts$contrast), " - |\\/")
  level_cols <- data.frame(do.call(rbind, lapply(level_cols, trimws)))
  names(level_cols) <- c("Level1", "Level2")
  level_cols$Level1 <- gsub(",", " - ", level_cols$Level1, fixed = TRUE)
  level_cols$Level2 <- gsub(",", " - ", level_cols$Level2, fixed = TRUE)

  # Merge levels and rest
  contrasts$contrast <- NULL
  contrasts <- cbind(level_cols, contrasts)

  # Add standardized effect size
  if (!effectsize %in% c("none", "emmeans", "marginal", "bootES")) {
    message("Unsupported effect size '", effectsize, "', returning none.")
    }

  if (effectsize == "emmeans") {
    eff <- emmeans::eff_size(
      estimated, sigma = stats::sigma(model),
      edf = stats::df.residual(model), method = "identity")
    eff <- as.data.frame(eff)
    eff <- eff[c(2, 5:6)]
    names(eff) <- c("effect_size", "es_CI_low", "es_CI_high")
    contrasts <- cbind(contrasts, eff)

  } else if (effectsize == "marginal") {
    # d_adj <- t * se_b / sigma * sqrt(1 - R2_cov)
    R2_cov <- summary(model)$r.squared
    d_adj <- contrasts$t * contrasts$SE / sigma(model) * sqrt(1 - R2_cov)
    contrasts <- cbind(contrasts, marginal_d = d_adj)

    } else if (effectsize == "bootES") {
      if (bootstraps < 500) {
        message("Number of bootstraps probably too low. Consider increasing it.")
      }

      insight::check_if_installed("bootES")
      dat <- insight::get_data(model)
      resp <- insight::find_response(model)
      group <- names(estimated@model.info$xlev)
      contrast <- estimated@misc$con.coef

      contrast <- lapply(seq_len(nrow(contrast)), function(x) {
        z <- contrast[x, ]
        names(z) <- levels(as.factor(dat[[group]]))
        z
        })

      es.lists <- lapply(contrast, function(x) {
        y <- bootES::bootES(
          data = stats::na.omit(dat),
          R = bootstraps,
          data.col = resp,
          group.col = group,
          contrast = x,
          effect.type = bootES_type
          )
        y <- as.data.frame(summary(y))})

      eff <- do.call(rbind, es.lists)
      eff <- eff[1:3]
      names(eff) <- c(bootES_type, paste0(bootES_type, "_CI_low"),
                      paste0(bootES_type, "es_CI_high"))

      contrasts <- cbind(contrasts, eff)
  }

  # Table formatting
  attr(contrasts, "table_title") <- c("Marginal Contrasts Analysis", "blue")
  attr(contrasts, "table_footer") <- .estimate_means_footer(
    contrasts,
    info$contrast,
    type = "contrasts",
    p_adjust = p_adjust
  )

  # Add attributes
  attr(contrasts, "model") <- model
  attr(contrasts, "response") <- insight::find_response(model)
  attr(contrasts, "ci") <- ci
  attr(contrasts, "transform") <- transform
  attr(contrasts, "at") <- info$at
  attr(contrasts, "fixed") <- info$fixed
  attr(contrasts, "contrast") <- info$contrast
  attr(contrasts, "p_adjust") <- p_adjust


  # Output
  class(contrasts) <- c("estimate_contrasts", "see_estimate_contrasts", class(contrasts))
  contrasts
}
