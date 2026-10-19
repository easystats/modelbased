# p-value adjustment --------------------------------------

.p_adjust <- function(model, params, p_adjust, verbose = TRUE, ...) {
  # exit on NULL, or if no p-adjustment requested
  if (is.null(p_adjust) || identical(p_adjust, "none")) {
    return(params)
  }

  # extract information
  datagrid <- attributes(params)$datagrid

  # `get_marginaltrends()` adjusts the unformatted marginaleffects output, with
  # the columns "p.value" and "statistic", and one test per row. Other callers
  # pass formatted output with the columns "p" and "t" or "z". A focal term can
  # have one of these names in either output.
  is_unformatted <- inherits(params, "marginaleffects_slopes")

  # when contrasting slopes, we need "by" as focal term
  if (inherits(params, "estimate_slopes")) {
    focal <- .safe(insight::trim_ws(gsub("=.*", "\\1", attributes(params)$by)))
  } else {
    focal <- .safe(insight::trim_ws(gsub("=.*", "\\1", attributes(params)$contrast)))
  }

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

  all_methods <- c(
    tolower(stats::p.adjust.methods),
    emmeans_options,
    "tukey",
    "sidak",
    "esarey",
    "sup-t"
  )
  insight::validate_argument(p_adjust, all_methods)

  # emmeans methods? Then tell user
  if (p_adjust %in% emmeans_options) {
    insight::format_error(paste0(
      "`p_adjust = \"",
      p_adjust,
      "\"` is only available when `backend = \"emmeans\"."
    ))
  }

  # slopes from `get_marginaltrends()` are each tested against 0, unless
  # `comparison` (or `hypothesis`, also when partly matched, like `hypo`) or
  # `post_process` is used. The Tukey adjustment is only for one set of
  # pairwise comparisons, so we keep Tukey only for pairwise comparisons of
  # slopes. Else, we use Sidak, as emmeans does. With one slope, or without
  # p-values, the code below says that no adjustment was made.
  dots <- list(...)
  n_slopes <- NULL
  slope_comparison <- NULL
  # number of tests in the family of each row. Comparisons of slopes with a
  # group term (like `~pairwise | group`) have one family per group, else
  # all rows are one family
  slope_family <- rep(nrow(params), nrow(params))
  if (is_unformatted) {
    dots_names <- names(dots)
    if (is.null(dots_names)) {
      dots_names <- character(0)
    }
    # names that are a prefix of "hypothesis", like `hypo`, which R matches
    # to the `hypothesis` argument of `avg_slopes()`
    hypothesis_arg <- dots_names[nzchar(dots_names) & startsWith("hypothesis", dots_names)]
    # `post_process` is applied last, so it defines the rows of the output
    slope_comparison <- c(
      dots["post_process"],
      dots["comparison"],
      dots[hypothesis_arg]
    )
    slope_comparison <- Filter(Negate(is.null), slope_comparison)
    if (length(slope_comparison)) {
      slope_family <- .p_adjust_family_size(
        params,
        by = .comparison_group_terms(slope_comparison[[1]])
      )
    }
  }
  if (p_adjust == "tukey" && is_unformatted && "p.value" %in% colnames(params)) {
    reason <- NULL
    if (!length(slope_comparison)) {
      if (nrow(params) > 1) {
        reason <- "Slopes are tested against 0"
      }
    } else if (is.numeric(slope_comparison[[1]]) && is.null(dim(slope_comparison[[1]]))) {
      reason <- paste0(
        "Slopes are tested against ",
        toString(insight::format_value(slope_comparison[[1]], protect_integers = TRUE))
      )
    } else if (.is_pairwise_comparison(slope_comparison[[1]])) {
      # for k slopes, we have k * (k - 1) / 2 pairwise comparisons, in each
      # group. Without the group columns, we cannot count them.
      group_terms <- .comparison_group_terms(slope_comparison[[1]])
      n_slopes <- (1 + sqrt(1 + 8 * slope_family)) / 2
      if (
        !all(group_terms %in% colnames(params)) ||
          anyNA(n_slopes) ||
          any(n_slopes != round(n_slopes))
      ) {
        n_slopes <- NULL
        reason <- "The number of compared slopes could not be found"
      }
    } else {
      reason <- "Slopes are not compared pairwise"
    }
    if (!is.null(reason)) {
      # with one test per family, Sidak does not change the p-values, so we
      # say nothing
      if (verbose && max(slope_family) > 1) {
        insight::format_alert(paste0(
          "The Tukey adjustment is only appropriate for pairwise comparisons. ",
          reason,
          ", so p-values were adjusted with the Sidak method instead."
        ))
      }
      p_adjust <- "sidak"
      attr(params, "p_adjust") <- "sidak"
    }
  }

  # contrasts (formatted output): the Tukey adjustment is only appropriate for
  # pairwise comparisons. For other comparisons, like "reference" or
  # "sequential", we use Sidak, as emmeans does.
  comparison <- attributes(params)$comparison
  # number of contrasts in the family of each row: the rows within each group
  # of the `by` variables, or of the group term of `comparison`
  if (!is_unformatted) {
    contrast_family <- .p_adjust_family_size(
      params,
      by = c(attributes(params)$by, .comparison_group_terms(comparison))
    )
  }
  if (
    p_adjust == "tukey" &&
      !is_unformatted &&
      !is.null(comparison) &&
      !.is_pairwise_comparison(comparison) &&
      "p" %in% colnames(params)
  ) {
    # with one contrast per family, Sidak does not change the p-values, so we
    # say nothing
    if (verbose && max(contrast_family) > 1) {
      insight::format_alert(
        "The Tukey adjustment is only appropriate for pairwise comparisons, so p-values were adjusted with the Sidak method instead."
      )
    }
    p_adjust <- "sidak"
    attr(params, "p_adjust") <- "sidak"
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
  focal_terms <- .safe(datagrid[focal])
  if (!is.null(n_slopes)) {
    # pairwise comparisons of slopes
    rank_adjust <- n_slopes
  } else if (is_unformatted) {
    rank_adjust <- nrow(params)
  } else if (is.null(focal_terms)) {
    rank_adjust <- 1
  } else {
    rank_adjust <- prod(vapply(focal_terms, insight::n_unique, numeric(1)))
    # pairwise contrasts: k levels give k * (k - 1) / 2 contrasts in each
    # family. This also counts the levels when a group term or filter makes
    # the focal terms differ from the compared levels.
    n_levels <- (1 + sqrt(1 + 8 * contrast_family)) / 2
    if (
      .is_pairwise_comparison(comparison) &&
        !anyNA(n_levels) &&
        all(n_levels == round(n_levels))
    ) {
      rank_adjust <- n_levels
    }
  }

  p_column <- if (is_unformatted) "p.value" else "p"
  if (!p_column %in% colnames(params)) {
    if (verbose) {
      insight::format_alert("No p-values found. P-values were not adjusted.")
    }
    return(params)
  }

  if (p_adjust %in% tolower(stats::p.adjust.methods)) {
    # base R adjustments - need to convert BH and BY back to uppercase before
    p_adjust <- switch(p_adjust, bh = "BH", by = "BY", p_adjust)
    params[[p_column]] <- stats::p.adjust(params[[p_column]], method = p_adjust)
  } else if (p_adjust == "tukey") {
    # find first occurence of one of the following columns: "t", "z", or "statistic"
    stat_columns <- if (is_unformatted) "statistic" else c("t", "z", "statistic")
    stat_col_name <- Find(
      function(col) col %in% colnames(params),
      stat_columns
    )
    if (!is.null(stat_col_name)) {
      statistic <- params[[stat_col_name]]
    } else {
      statistic <- NULL
    }
    if (!is.null(statistic)) {
      if (min(rank_adjust) < 2) {
        if (verbose) {
          insight::format_alert(
            "Tukey adjustment requires at least 2 groups. P-values were not adjusted."
          )
        }
      } else if (!is.null(dof) && is.finite(dof) && dof <= 0) {
        if (verbose) {
          insight::format_alert(
            "Tukey adjustment requires positive degrees of freedom. P-values were not adjusted."
          )
        }
      } else {
        # tukey adjustment
        params[[p_column]] <- stats::ptukey(
          sqrt(2) * abs(statistic),
          rank_adjust,
          dof,
          lower.tail = FALSE
        )
      }
    } else if (verbose) {
      insight::format_alert("No test-statistic found. P-values were not adjusted.")
    }
  } else if (p_adjust == "sidak") {
    # sidak adjustment. The family is the set of tests: for slopes, all rows,
    # and for contrasts, the rows within each group of the `by` variables
    # (like emmeans).
    if (is_unformatted) {
      family_size <- slope_family
    } else {
      family_size <- contrast_family
    }
    # 1 - (1 - p)^n, without losing precision for small p-values
    params[[p_column]] <- -expm1(family_size * log1p(-params[[p_column]]))
  }

  params
}


# TRUE if a comparison is a formula whose right side is `pairwise` or
# `revpairwise`, with or without a `| group` term, e.g. `difference ~ pairwise`
.is_pairwise_comparison <- function(comparison) {
  if (is.character(comparison)) {
    return(length(comparison) == 1 && comparison %in% c("pairwise", "revpairwise"))
  }
  if (!inherits(comparison, "formula")) {
    return(FALSE)
  }
  rhs <- comparison[[length(comparison)]]
  if (is.call(rhs) && identical(rhs[[1]], as.name("|"))) {
    rhs <- rhs[[2]]
  }
  is.name(rhs) && as.character(rhs) %in% c("pairwise", "revpairwise")
}


# variables of the group term of a comparison formula, like `h` in
# `~pairwise | h`, or NULL
.comparison_group_terms <- function(comparison) {
  if (!inherits(comparison, "formula")) {
    return(NULL)
  }
  rhs <- comparison[[length(comparison)]]
  if (is.call(rhs) && identical(rhs[[1]], as.name("|"))) {
    return(all.vars(rhs[[3]]))
  }
  NULL
}


# number of tests in the family of each row: the rows with the same values of
# the `by` variables (by default, the `by` attribute)
.p_adjust_family_size <- function(params, by = attributes(params)$by) {
  by <- .safe(insight::trim_ws(gsub("=.*", "", by)))
  by <- intersect(by, colnames(params))
  if (!length(by)) {
    return(rep(nrow(params), nrow(params)))
  }
  # missing values of a `by` variable form their own group
  groups <- interaction(lapply(params[by], addNA), drop = TRUE)
  as.vector(table(groups)[as.integer(groups)])
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
    insight::format_alert(
      "Could not find coefficient column to apply `sup-t` adjustment."
    )
    return(params)
  }
  se_column <- intersect(c("SE", "std.error"), colnames(params))[1]
  if (is.na(se_column)) {
    insight::format_alert(
      "Could not extract standard errors to apply `sup-t` adjustment."
    )
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
    insight::format_alert(
      "Could not extract confidence intervals to apply `sup-t` adjustment."
    )
    return(params)
  }
  df_column <- intersect(c("df", "df_error"), colnames(params))[1]
  if (is.na(df_column)) {
    df_column <- ".sup_df"
    params[[df_column]] <- Inf
  }
  # calculate updated confidence interval level, based on simultaenous
  # confidence intervals (https://onlinelibrary.wiley.com/doi/10.1002/jae.2656)
  crit <- mvtnorm::qmvt(
    ci_level,
    df = params[[df_column]][1],
    tail = "both.tails",
    corr = vc
  )$quantile
  # update confidence intervals
  params[[ci_low_column]] <- params[[coef_column]] - crit * params[[se_column]]
  params[[ci_high_column]] <- params[[coef_column]] + crit * params[[se_column]]
  # update p-values
  for (i in 1:nrow(params)) {
    params[[p_column]][i] <- 1 -
      mvtnorm::pmvt(
        lower = rep(
          -abs(stats::qt(params[[p_column]][i] / 2, df = params[[df_column]][i])),
          nrow(vc)
        ),
        upper = rep(
          abs(stats::qt(params[[p_column]][i] / 2, df = params[[df_column]][i])),
          nrow(vc)
        ),
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
    insight::format_error(
      "The `esarey` p-value adjustment is only available for Johnson-Neyman intervals, i.e. when calling `estimate_slopes()` with an interaction term of two numeric predictors."
    )
  }
  # get names of interaction terms
  pred <- attributes(x)$trend
  mod <- attributes(x)$by

  # check for valid values - all must be numeric
  if (!all(vapply(attributes(x)$datagrid[c(pred, mod)], is.numeric, logical(1)))) {
    insight::format_error(
      "The `esarey` p-value adjustment is only available for Johnson-Neyman intervals, i.e. when calling `estimate_slopes()` with an interaction term of two numeric predictors."
    )
  }

  int <- paste0(pred, ":", mod)
  if (inherits(x, c("marginaleffects_slopes", "slopes", "marginaleffects"))) {
    insight::check_if_installed("marginaleffects", minimum_version = "0.29.0")
    model <- marginaleffects::components(x, "model")
  } else {
    model <- insight::get_model(x)
  }

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
  me_ses <- sqrt(
    vcov_pred + (range_sequence^2) * vcov_int + 2 * range_sequence * vcov_pred_int
  )

  # t-values across range of marginal effects
  statistic <- marginal_effects / me_ses
  # degrees of freedom
  dof <- insight::get_df(model, type = "wald")
  # Get the minimum p values used in the adjustment
  pvalues <- 2 *
    pmin(stats::pt(statistic, df = dof), (1 - stats::pt(statistic, df = dof)))
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
