#' @export
format.marginaleffects_means <- function(x, model, ...) {
  predict <- attributes(x)$predict
  # model information
  model_data <- insight::get_data(model)
  info <- insight::model_info(model, verbose = FALSE)
  non_focal <- setdiff(colnames(model_data), attr(x, "focal_terms"))
  is_contrast_analysis <- !is.null(list(...)$hypothesis)
  predict_type <- attributes(x)$predict

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

  # define all columns that should be removed
  remove_columns <- c("s.value", "S", "CI", "rowid_dedup", non_focal)

  # reshape and format columns
  params <- .standardize_marginaleffects_columns(
    params,
    remove_columns,
    model_data,
    info,
    estimate_name
  )

  ## TODO: check if we can use modifyList()
  # Store info
  attr(params, "at") <- attr(x, "by")
  attr(params, "by") <- attr(x, "by")
  attr(params, "predict") <- attr(x, "predict")
  attr(params, "contrast") <- attr(x, "contrast")
  attr(params, "trend") <- attr(x, "trend")
  attr(params, "focal_terms") <- attr(x, "focal_terms")
  attr(params, "datagrid") <- attr(x, "datagrid")

  params
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
    model_data,
    info
  )

  ## TODO: check if we can use modifyList()
  # Store info
  attr(params, "at") <- attr(x, "by")
  attr(params, "by") <- attr(x, "by")
  attr(params, "predict") <- attr(x, "predict")
  attr(params, "contrast") <- attr(x, "contrast")
  attr(params, "trend") <- attr(x, "trend")
  attr(params, "focal_terms") <- attr(x, "focal_terms")
  attr(params, "datagrid") <- attr(x, "datagrid")

  params
}


# Helper ----------------------------------------------------------------------
# -----------------------------------------------------------------------------


#' @keywords internal
.standardize_marginaleffects_columns <- function(params,
                                                 remove_columns,
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
