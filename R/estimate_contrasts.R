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
#' size: `d_adj <- difference * (1- R2)/ sigma`. This standardized
#' using the response SD with only the between-groups variance on the focal
#' factor/contrast removed. This allows for groups to be equated on their
#' covariates, but creates an appropriate scale for standardizing the response.
#'
#' `effectsize = "bootES"` uses bootstrapping (defaults to a low value of
#' 200) through [bootES::bootES]. Adjust for contrasts, but not for covariates.
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
                               effectsize = "none",
                               bootstraps = 200,
                               bootES_type = "cohens.d",
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
    names(eff) <- c("partial_d", "es_CI_low", "es_CI_high")
    contrasts <- cbind(contrasts, eff)

  } else if (effectsize == "marginal") {
    # Original: d_adj <- t * se_b / sigma * sqrt(1 - R2_cov)
    # d_adj <- contrasts$t * contrasts$SE / sigma(model) * sqrt(1 - R2)
    # New: d_adj <- difference * (1- R2)/ sigma
    R2 <- summary(model)$r.squared
    d_adj <- contrasts$Difference * (1 - R2) / sigma(model)
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


# Table formatting marginal effects -------------------------------------------


.format_marginaleffects_contrasts <- function(model, estimated, p_adjust, method, ...) {
  groups <- attributes(estimated)$by
  contrast <- attributes(estimated)$contrast
  focal_terms <- attributes(estimated)$focal_terms

  estimated <- .p_adjust(model, estimated, p_adjust, ...)

  valid_methods <- c(
    "pairwise", "reference", "sequential", "meandev", "meanotherdev",
    "revpairwise", "revreference", "revsequential"
  )

  if (!is.null(method) && is.character(method) && method %in% valid_methods) {

    ## TODO: split Parameter column into levels indicated in "contrast", and filter by "by"

    # These are examples of what {marginaleffects} returns, a single parmater
    # column that includes all levels, comma- and dash-separated, or with /
    # see also https://github.com/easystats/modelbased/pull/280
    #
    #   estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects", p_adjust = "none")
    # #> Marginal Contrasts Analysis
    # #>
    # #> Parameter                              | Difference |          95% CI |      p
    # #> ------------------------------------------------------------------------------
    # #> morning, coffee - morning, control     |       5.78 | [  1.83,  9.73] | 0.004
    # #> morning, coffee - noon, coffee         |       1.93 | [ -2.02,  5.88] | 0.336
    #
    # estimate_contrasts(
    #   m,
    #   c("time", "coffee"),
    #   backend = "marginaleffects",
    #   p_adjust = "none",
    #   method = ratio ~ reference | coffee
    # )
    # #> Marginal Contrasts Analysis
    # #>
    # #> coffee  |              hypothesis | Difference |       95% CI |      p
    # #> ----------------------------------------------------------------------
    # #> coffee  |      (noon) / (morning) |       0.89 | [0.67, 1.11] | < .001
    # #> coffee  | (afternoon) / (morning) |       1.11 | [0.87, 1.36] | < .001
    #
    # We need to split the "Parameter" or "hypothesis" columns into one column
    # per level, as we do with the emmeans-backend. Else, we cannot use the "by"
    # argument, which is used for filtering by levels of given focal terms.
  }

  estimated
}
