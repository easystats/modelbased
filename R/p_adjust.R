# p-value adjustment --------------------------------------

.p_adjust <- function(model, params, p_adjust, verbose = TRUE, ...) {
  # extract information
  datagrid <- attributes(params)$datagrid
  focal <- attributes(params)$contrast
  statistic <- insight::get_statistic(model)$Statistic
  dof <- insight::get_df(model, type = "wald", verbose = FALSE)

  # exit on NULL, or if no p-adjustment requested
  if (is.null(p_adjust) || identical(p_adjust, "none")) {
    return(params)
  }

  # harmonize argument
  p_adjust <- tolower(p_adjust)

  # we need to check for options provided by emmeans. We check them here, but
  # we have to print a different error message.
  emmeans_options <- c("scheffe", "mvt", "dunnettx")

  all_methods <- c(tolower(stats::p.adjust.methods), emmeans_options, "tukey", "sidak", "esarey")
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
