# Format ------------------------------------------------------------------

#' @export
format.estimate_contrasts <- function(x, format = NULL, ...) {
  # don't print columns of adjusted_for variables
  adjusted_for <- attr(x, "adjusted_for", exact = TRUE)
  if (!is.null(adjusted_for) && all(adjusted_for %in% colnames(x))) {
    # remove non-focal terms from data frame
    x[adjusted_for] <- NULL
  }

  # arrange columns (not for contrast now)
  by <- rev(attr(x, "focal_terms", exact = TRUE))
  if (!is.null(by) && all(by %in% colnames(x))) {
    x <- datawizard::data_arrange(x, select = by)
  }

  if (!is.null(format) && format %in% c("md", "markdown", "html")) {
    insight::format_table(x, ci_brackets = c("(", ")"), ...)
  } else {
    insight::format_table(x, ...)
  }
}

#' @export
format.estimate_means <- format.estimate_contrasts

#' @export
format.estimate_slopes <- format.estimate_contrasts

#' @export
format.estimate_predicted <- format.estimate_contrasts

#' @export
format.estimate_grouplevel <- format.estimate_contrasts


#' @export
format.estimate_smooth <- function(x, ...) {
  # Colnames
  if ("Size" %in% names(x)) x$Size <- ifelse(x$Size < 1, paste0(insight::format_value(x$Size * 100), "%"), "100%")
  if ("Part" %in% names(x)) x$Part <- insight::format_value(x$Part, protect_integers = TRUE)

  insight::format_table(x, ...)
}


#' @export
format.visualisation_matrix <- function(x, ...) {
  x
}


#' @export
format.marginaleffects_means <- function(x, model, ci = 0.95, ...) {
  # model information
  model_data <- insight::get_data(model)
  info <- insight::model_info(model, verbose = FALSE)
  non_focal <- setdiff(colnames(model_data), attr(x, "focal_terms"))
  is_contrast_analysis <- !is.null(list(...)$hypothesis)
  predict_type <- attributes(x)$predict

  # define all columns that should be removed
  remove_columns <- c("s.value", "S", "CI", "rowid_dedup", non_focal)

  # do we have contrasts? For contrasts, we want to keep p-values
  if (is_contrast_analysis) {
    estimate_name <- "Difference"
  } else {
    # for simple means, we don't want p-values
    remove_columns <- c(remove_columns, "p")
    # estimate name
    estimate_name <- .guess_estimate_name(predict_type, info)
  }

  # reshape and format columns
  params <- .standardize_marginaleffects_columns(
    x,
    remove_columns,
    model,
    model_data,
    info,
    ci,
    estimate_name,
    is_contrast_analysis
  )

  .set_back_attributes(x, params)
}


#' @export
format.marginaleffects_slopes <- function(x, model, ci = 0.95, ...) {
  # model information
  info <- insight::model_info(model, verbose = FALSE)
  model_data <- insight::get_data(model)
  # define all columns that should be removed
  remove_columns <- c("Parameter", "Predicted", "s.value", "S", "CI", "rowid_dedup")
  # reshape and format columns
  params <- .standardize_marginaleffects_columns(
    x,
    remove_columns,
    model,
    model_data,
    info,
    ci
  )

  .set_back_attributes(x, params)
}


#' @export
format.marginaleffects_contrasts <- function(x, model, p_adjust, comparison, ...) {
  predict <- attributes(x)$predict
  groups <- attributes(x)$by
  contrast <- attributes(x)$contrast
  focal_terms <- attributes(x)$focal_terms

  valid_options <- c(
    "pairwise", "reference", "sequential", "meandev", "meanotherdev",
    "revpairwise", "revreference", "revsequential"
  )

  if (!is.null(comparison) && is.character(comparison) && comparison %in% valid_options) {
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
    #   comparison = ratio ~ reference | coffee
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

  x
}


# Helper ----------------------------------------------------------------------
# -----------------------------------------------------------------------------


# This function renames columns to have a consistent naming scheme,
# and relocates columns to get a standardized column order across all
# outputs from {marginaleffects}

#' @keywords internal
.standardize_marginaleffects_columns <- function(x,
                                                 remove_columns,
                                                 model,
                                                 model_data,
                                                 info,
                                                 ci = 0.95,
                                                 estimate_name = NULL,
                                                 is_contrast_analysis = FALSE) {
  # tidy output
  params <- suppressWarnings(parameters::model_parameters(x, verbose = FALSE))
  coefficient_name <- intersect(
    c(attributes(params)$coefficient_name, "Coefficient", "Predicted"),
    colnames(params)
  )[1]

  # add back ci? these are missing when contrasts are computed
  params <- .add_contrasts_ci(is_contrast_analysis, params)

  # relocate columns
  relocate_columns <- intersect(
    c(coefficient_name, "Coefficient", "Predicted", "SE", "CI_low", "CI_high", "Statistic", "df", "df_error"),
    colnames(params)
  )
  params <- datawizard::data_relocate(params, relocate_columns, after = -1, verbose = FALSE) # nolint
  params <- datawizard::data_relocate(params, "p", after = -1, verbose = FALSE)

  # rename columns
  if (!is.null(estimate_name)) {
    params <- datawizard::data_rename(
      params,
      select = coefficient_name,
      replacement = estimate_name
    )
  }
  if ("Statistic" %in% colnames(params)) {
    params <- datawizard::data_rename(
      params,
      select = "Statistic",
      replacement = gsub("-statistic", "", insight::find_statistic(model), fixed = TRUE)
    )
  }

  # remove redundant columns
  params <- datawizard::data_remove(params, remove_columns, verbose = FALSE) # nolint

  # Rename for Categorical family
  if (info$is_categorical) {
    params <- datawizard::data_rename(params, "group", "Response")
  }

  # finally, make sure we have original data types
  data.frame(datawizard::data_restoretype(params, model_data))
}


# This function ensures that the formatted object still has all relevant
# information saved as attributes

#' @keywords internal
.set_back_attributes <- function(x, formatted_params) {
  attributes(formatted_params) <- utils::modifyList(
    attributes(formatted_params),
    attributes(x)[c("by", "at", "predict", "contrast", "trend", "datagrid", "focal_terms")]
  )
  formatted_params
}


# for contrasts analysis, CIs are not computed automatically. The `format()`
# methods adds back those CIs by calling this function.

#' @keywords internal
.add_contrasts_ci <- function(is_contrast_analysis, params) {
  if (is_contrast_analysis && !"CI_low" %in% colnames(params) && "SE" %in% colnames(params)) {
    # extract ci-level
    if ("CI" %in% colnames(params)) {
      ci <- params[["CI"]][1]
    } else {
      ci <- attributes(params)$ci
    }
    if (is.null(ci)) {
      ci <- 0.95
    }
    # get degrees of freedom
    if ("df" %in% colnames(params)) {
      dof <- params[["df"]]
    } else {
      dof <- Inf
    }
    # critical test value
    crit <- stats::qt((1 + ci) / 2, df = dof)
    # add CI
    params$CI_low <- params$Predicted - crit * params$SE
    params$CI_high <- params$Predicted + crit * params$SE
  }
  params
}


# this function tries to find the most approriate name of the estimates / predictions,
# based on on which scale predictions were requested

#' @keywords internal
.guess_estimate_name <- function(predict_type, info) {
  # estimate name
  if (!is.null(predict_type) && tolower(predict_type) %in% .brms_aux_elements()) {
    # for Bayesian models with distributional parameter
    estimate_name <- tools::toTitleCase(predict_type)
  } else if (!predict_type %in% c("none", "link") && (info$is_binomial || info$is_bernoulli)) {
    estimate_name <- "Probability"
  } else if (predict_type %in% c("zprob", "zero")) {
    estimate_name <- "Probability" ## TODO: could be renamed into ZI-Probability?
  } else {
    estimate_name <- "Mean"
  }
  estimate_name
}
