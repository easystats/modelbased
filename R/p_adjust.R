# p-value adjustment --------------------------------------

.p_adjust <- function(model, params, p_adjust, verbose = TRUE, ...) {
  # exit on NULL, or if no p-adjustment requested
  if (is.null(p_adjust) || identical(p_adjust, "none")) {
    return(params)
  }

  # extract information
  datagrid <- attributes(params)$datagrid
  focal <- attributes(params)$contrast
  # Use .safe to handle cases where no statistic is extracted
  statistic <- .safe(insight::get_statistic(model)$Statistic)
  # extract degrees of freedom
  dof <- .safe(params$df[1])
  if (is.null(dof)) {
    dof <- insight::get_df(model, type = "wald", verbose = FALSE)
  }

  # harmonize argument
  p_adjust <- tolower(p_adjust)

  # we need to check for options provided by emmeans. We check them here, but
  # we have to print a different error message.
  emmeans_options <- c("scheffe", "mvt", "dunnettx")

  all_methods <- c(tolower(stats::p.adjust.methods), emmeans_options, "tukey", "sidak", "esarey", "sup-t")
  insight::validate_argument(p_adjust, all_methods)

  # emmeans methods? Then tell user
  if (p_adjust %in% emmeans_options) {
    insight::format_error(paste0(
      "`p_adjust = \"", p_adjust, "\"` is only available when `backend = \"emmeans\"."
    ))
  }

  # esarey is specifically for johnson-neyman intervals
  if (p_adjust == "esarey") {
    return(.p_adjust_esarey(params))
  }

  # sup-t is a longer subroutine, so we handle it separately
  if (p_adjust == "sup-t") {
    return(.p_adjust_supt(params, model))
  }

  # needed for rank adjustment
  focal_terms <- datagrid[focal]
  rank_adjust <- prod(vapply(focal_terms, insight::n_unique, numeric(1)))

  if (p_adjust %in% tolower(stats::p.adjust.methods)) {
    # base R adjustments
    params[["p"]] <- stats::p.adjust(params[["p"]], method = p_adjust)
  } else if (p_adjust == "tukey") {
    if (!is.null(statistic)) {
      # tukey adjustment
      params[["p"]] <- suppressWarnings(stats::ptukey(
        sqrt(2) * abs(statistic),
        rank_adjust,
        dof,
        lower.tail = FALSE
      ))
      # for specific contrasts, ptukey might fail, and the tukey-adjustement
      # could just be simple p-value calculation
      if (all(is.na(params[["p"]]))) {
        params[["p"]] <- 2 * stats::pt(abs(statistic), df = dof, lower.tail = FALSE)
      }
    } else if (verbose) {
      insight::format_alert("No test-statistic found. P-values were not adjusted.")
    }
  } else if (p_adjust == "sidak") {
    # sidak adjustment
    params[["p"]] <- 1 - (1 - params[["p"]])^rank_adjust
  }

  params
}


.p_adjust_supt <- function(params, model) {
  insight::check_if_installed("mvtnorm")
  # get correlation matrix, based on the covariance matrix
  vc <- .safe(stats::cov2cor(attributes(params)$vcov))
  if (is.null(vc)) {
    insight::format_alert("Could not calculate covariance matrix for `sup-t` adjustment.")
    return(params)
  }
  # get confidence interval level, or set default
  ci_level <- attributes(params)$ci
  if (is.null(ci_level)) {
    ci_level <- 0.95
  }
  # several sanity checks - we can either have a marginaleffects object, when
  # `estimate_slopes()` was called, or a modelbased object, when processing /
  # formatting was already done. So we check for both, and extract the required
  # columns.
  coef_column <- intersect(c(.valid_coefficient_names(), "estimate"), colnames(params))[1]
  if (is.na(coef_column)) {
    insight::format_alert("Could not find coefficient column to apply `sup-t` adjustment.")
    return(params)
  }
  se_column <- intersect(c("SE", "std.error"), colnames(params))[1]
  if (is.na(se_column)) {
    insight::format_alert("Could not extract standard errors to apply `sup-t` adjustment.")
    return(params)
  }
  p_column <- intersect(c("p", "p.value"), colnames(params))[1]
  if (is.na(p_column)) {
    insight::format_alert("Could not extract p-values to apply `sup-t` adjustment.")
    return(params)
  }
  ci_low_column <- intersect(c("CI_low", "conf.low"), colnames(params))[1]
  ci_high_column <- intersect(c("CI_high", "conf.high"), colnames(params))[1]
  if (is.na(ci_low_column) || is.na(ci_high_column)) {
    insight::format_alert("Could not extract confidence intervals to apply `sup-t` adjustment.")
    return(params)
  }
  df_column <- intersect(c("df", "df_error"), colnames(params))[1]
  if (is.na(df_column)) {
    df_column <- ".sup_df"
    params[[df_column]] <- Inf
  }
  # calculate updated confidence interval level, based on simultaenous
  # confidence intervals (https://onlinelibrary.wiley.com/doi/10.1002/jae.2656)
  crit <- mvtnorm::qmvt(ci_level, df = params[[df_column]][1], tail = "both.tails", corr = vc)$quantile
  # update confidence intervals
  params[[ci_low_column]] <- params[[coef_column]] - crit * params[[se_column]]
  params[[ci_high_column]] <- params[[coef_column]] + crit * params[[se_column]]
  # update p-values
  for (i in 1:nrow(params)) {
    params[[p_column]][i] <- 1 - mvtnorm::pmvt(
      lower = rep(-abs(stats::qt(params[[p_column]][i] / 2, df = params[[df_column]][i])), nrow(vc)),
      upper = rep(abs(stats::qt(params[[p_column]][i] / 2, df = params[[df_column]][i])), nrow(vc)),
      corr = vc,
      df = params[[df_column]][i]
    )
  }
  # clean up - remove temporary column
  params[[".sup_df"]] <- NULL

  params
}


.p_adjust_esarey <- function(x) {
  # only for slopes
  if (!inherits(x, c("estimate_slopes", "marginaleffects_slopes"))) {
    insight::format_error("The `esarey` p-value adjustment is only available for Johnson-Neyman intervals, i.e. when calling `estimate_slopes()` with an interaction term of two numeric predictors.") # nolint
  }
  # get names of interaction terms
  pred <- attributes(x)$trend
  mod <- attributes(x)$by

  # check for valid values - all must be numeric
  if (!all(vapply(attributes(x)$datagrid[c(pred, mod)], is.numeric, logical(1)))) {
    insight::format_error("The `esarey` p-value adjustment is only available for Johnson-Neyman intervals, i.e. when calling `estimate_slopes()` with an interaction term of two numeric predictors.") # nolint
  }

  int <- paste0(pred, ":", mod)
  model <- attributes(x)$model

  # variance-covariance matrix, to adjust p-values
  varcov <- insight::get_varcov(model)
  # Predictor variances
  vcov_pred <- varcov[pred, pred]
  vcov_int <- varcov[int, int]
  vcov_pred_int <- varcov[pred, int]

  # Generate sequence of numbers along range of moderator
  range_sequence <- seq(
    from = min(x[[mod]], na.rm = TRUE),
    to = max(x[[mod]], na.rm = TRUE),
    by = diff(range(x[[mod]], na.rm = TRUE)) / 1000
  )

  # get parameters, to manually calculate marginal effects
  params <- insight::get_parameters(model)
  beta_pred <- params$Estimate[params$Parameter == pred]
  beta_int <- params$Estimate[params$Parameter == int]

  # produces a sequence of marginal effects
  marginal_effects <- beta_pred + beta_int * range_sequence
  # SEs of those marginal effects
  me_ses <- sqrt(vcov_pred + (range_sequence^2) * vcov_int + 2 * range_sequence * vcov_pred_int)

  # t-values across range of marginal effects
  statistic <- marginal_effects / me_ses
  # degrees of freedom
  dof <- insight::get_df(model, type = "wald")
  # Get the minimum p values used in the adjustment
  pvalues <- 2 * pmin(stats::pt(statistic, df = dof), (1 - stats::pt(statistic, df = dof)))
  # Multipliers
  multipliers <- seq_along(marginal_effects) / length(marginal_effects)
  # Order the pvals
  ordered_pvalues <- order(pvalues)

  # Adapted from interactionTest package function fdrInteraction
  test <- 0
  i <- 1 + length(marginal_effects)
  alpha <- (1 - attributes(x)$ci) / 2

  while (test == 0 && i > 1) {
    i <- i - 1
    test <- min(pvalues[ordered_pvalues][1:i] <= multipliers[i] * (alpha * 2))
  }

  # updates test statistic
  tcrit <- abs(stats::qt(multipliers[i] * alpha, dof))
  # update confidence intervals
  x$conf.low <- x$estimate - tcrit * x$std.error
  x$conf.high <- x$estimate + tcrit * x$std.error

  # update p-values - we need to ensure that length of "statisic" matches number
  # of rows, so we pick just as many values from "statistic" as required
  range_mod <- floor((1:nrow(x)) * (nrow(x) / 100) * length(range_sequence))
  if (length(range_sequence) == length(statistic)) {
    statistic <- statistic[range_mod]
    # update p-values
    x$p.value <- 2 * stats::pt(abs(statistic), df = dof, lower.tail = FALSE)
  }
  x
}
