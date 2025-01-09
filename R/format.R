#' @export
format.marginaleffects_means <- function(x, model, ...) {
  predict <- attributes(x)$predict
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
    if (!is.null(predict_type) && tolower(predict_type) %in% .brms_aux_elements()) {
      # for Bayesian models with distributional parameter
      estimate_name <- tools::toTitleCase(predict_type)
    } else if (!predict %in% c("none", "link") && (info$is_binomial || info$is_bernoulli)) {
      estimate_name <- "Probability"
    } else {
      estimate_name <- "Mean"
    }
  }

  # tidy output
  params <- suppressWarnings(parameters::model_parameters(x, verbose = FALSE))
  # add back ci? these are missing when contrasts are computed
  params <- .add_contrasts_ci(is_contrast_analysis, params)

  # reshape and format columns
  params <- .standardize_marginaleffects_columns(
    params,
    remove_columns,
    model,
    model_data,
    info,
    estimate_name
  )

  .set_back_attributes(x, params)
}


#' @export
format.marginaleffects_slopes <- function(x, model, ci = 0.95, ...) {
  # model information
  info <- insight::model_info(model, verbose = FALSE)
  model_data <- insight::get_data(model)
  # Summarize and clean
  params <- parameters::parameters(x, ci = ci, ...)
  # define all columns that should be removed
  remove_columns <- c("s.value", "S", "CI", "rowid_dedup")
  # reshape and format columns
  params <- .standardize_marginaleffects_columns(
    params,
    remove_columns,
    model,
    model_data,
    info
  )

  .set_back_attributes(x, params)
}


# Helper ----------------------------------------------------------------------
# -----------------------------------------------------------------------------


# This function renames columns to have a consistent naming scheme,
# and relocates columns to get a standardized column order across all
# outputs from {marginaleffects}

#' @keywords internal
.standardize_marginaleffects_columns <- function(params,
                                                 remove_columns,
                                                 model,
                                                 model_data,
                                                 info,
                                                 estimate_name = NULL) {
  params <- datawizard::data_relocate(params, c("Predicted", "SE", "CI_low", "CI_high", "Statistic", "df", "df_error"), after = -1, verbose = FALSE) # nolint
  # move p to the end
  params <- datawizard::data_relocate(params, "p", after = -1, verbose = FALSE)
  # rename
  if (!is.null(estimate_name)) {
    params <- datawizard::data_rename(
      params,
      select = "Predicted",
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
  datawizard::data_restoretype(params, model_data)
}


# This function ensures that the formatted object still has all relevant
# information saved as attributes

#' @keywords internal
.set_back_attributes <- function(x, formatted_params) {
  attr(formatted_params, "at") <- attr(x, "by")
  attr(formatted_params, "by") <- attr(x, "by")
  attr(formatted_params, "predict") <- attr(x, "predict")
  attr(formatted_params, "contrast") <- attr(x, "contrast")
  attr(formatted_params, "trend") <- attr(x, "trend")
  attr(formatted_params, "focal_terms") <- attr(x, "focal_terms")
  attr(formatted_params, "datagrid") <- attr(x, "datagrid")

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
