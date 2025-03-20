#' Pool contrasts and comparisons from `estimate_contrasts()`
#'
#' This function "pools" (i.e. combines) multiple `estimate_contrasts` objects,
#' returned by [`estimate_contrasts()`], in a similar fashion as [`mice::pool()`].
#'
#' @param x A list of `estimate_contrasts` objects, as returned by
#' `estimate_contrasts()`.
#' @param ... Currently not used.
#'
#' @details Averaging of parameters follows Rubin's rules (*Rubin, 1987, p. 76*).
#'
#' @references
#' Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys. New York:
#' John Wiley and Sons.
#'
#' @examplesIf require("mice")
#' data("nhanes2", package = "mice")
#' imp <- mice::mice(nhanes2, printFlag = FALSE)
#' comparisons <- lapply(1:5, function(i) {
#'   m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
#'   estimate_contrasts(m, "age")
#' })
#' pool_contrasts(comparisons)
#' @return A data frame with pooled comparisons or contrasts of predictions.
#' @export
pool_contrasts <- function(x, ...) {
  # check input -----

  obj_name <- deparse(substitute(x), width.cutoff = 500)
  original_x <- x

  if (!all(vapply(x, inherits, logical(1), "estimate_contrasts"))) {
    insight::format_error(
      "`x` must be a list of `estimate_contrasts` objects, as returned by `estimate_contrasts()`."
    )
  }

  # name of prediction / contrast column
  estimate_name <- attributes(x[[1]])$coef_name

  # preparation ----

  ci <- attributes(x[[1]])$ci
  dof <- x[[1]]$df

  if (is.null(dof)) {
    dof <- Inf
  }

  # pool contrasts -----
  pooled_comparisons <- .pool_estimates(original_x, estimate_name, original_x[[1]])

  # confidence intervals ----
  pooled_comparisons <- .pool_ci_and_stats(pooled_comparisons, estimate_name, ci, dof)

  attributes(pooled_comparisons) <- utils::modifyList(attributes(original_x[[1]]), attributes(pooled_comparisons))
  pooled_comparisons
}


#' Pool Predictions and Estimated Marginal Means
#'
#' This function "pools" (i.e. combines) multiple `estimate_means` objects, in
#' a similar fashion as [`mice::pool()`].
#'
#' @param x A list of `estimate_means` objects, as returned by
#' [`estimate_means()`], or `estimate_predicted` objects, as returned by
#' [`estimate_relation()`] and related functions.
#' @param ... Currently not used.
#' @inheritParams estimate_means
#'
#' @details Averaging of parameters follows Rubin's rules (*Rubin, 1987, p. 76*).
#' Pooling is applied to the predicted values and based on the standard errors
#' as they are calculated in the `estimate_means` or `estimate_predicted`
#' objects provided in `x`. For objects of class `estimate_means`, the predicted
#' values are on the response scale by default, and standard errors are
#' calculated using the delta method. Then, pooling estimates and calculating
#' standard errors for the pooled estimates based ob Rubin's rule is carried
#' out. There is no back-transformation to the link-scale of predicted values
#' before applying Rubin's rule.
#'
#' @references
#' Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys. New York:
#' John Wiley and Sons.
#'
#' @examplesIf require("mice")
#' # example for multiple imputed datasets
#' data("nhanes2", package = "mice")
#' imp <- mice::mice(nhanes2, printFlag = FALSE)
#' predictions <- lapply(1:5, function(i) {
#'   m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
#'   estimate_means(m, "age")
#' })
#' pool_predictions(predictions)
#' @return A data frame with pooled predictions.
#' @export
pool_predictions <- function(x, transform = NULL, ...) {
  # check input -----

  obj_name <- deparse(substitute(x), width.cutoff = 500)
  original_x <- x

  if (!all(vapply(x, inherits, logical(1), c("estimate_means", "estimate_predicted")))) {
    insight::format_error(
      "`x` must be a list of `estimate_means` objects, as returned by `estimate_means()`, or a list of `estimate_predicted` objects, as returned by functions like `estimate_expectation()`." # nolint
    )
  }

  # name of prediction / contrast column
  estimate_name <- attributes(x[[1]])$coef_name

  # preparation ----

  ci <- attributes(x[[1]])$ci
  model <- attributes(x[[1]])$model
  dof <- x[[1]]$df

  # we don't use the link-inverse because standard errors are calculated using
  # the delta method, hence, these would be incorrect if we apply link-inverse
  # transformation to calculate CIs.

  if (is.null(dof)) {
    dof <- Inf
  }
  if (isTRUE(transform)) {
    transform_fun <- insight::get_transformation(model, verbose = FALSE)$inverse
  } else {
    transform_fun <- transform
  }

  # pool predictions -----
  pooled_predictions <- .pool_estimates(original_x, estimate_name, original_x[[1]])

  # confidence intervals ----
  pooled_predictions <- .pool_ci_and_stats(pooled_predictions, estimate_name, ci, dof)

  # back-transform response and CI?
  if (!is.null(transform_fun)) {
    pooled_predictions[[estimate_name]] <- transform_fun(pooled_predictions[[estimate_name]])
    pooled_predictions$CI_low <- transform_fun(pooled_predictions$CI_low)
    pooled_predictions$CI_high <- transform_fun(pooled_predictions$CI_high)
  }

  pooled_predictions
}


# helper ------


# pool estimate
.pool_estimates <- function(original_x, estimate_name, pooled_predictions) {
  n_rows <- nrow(original_x[[1]])
  len <- length(original_x)

  for (i in 1:n_rows) {
    # pooled estimate
    pooled_pred <- unlist(lapply(original_x, function(j) j[[estimate_name]][i]), use.names = FALSE)
    pooled_predictions[[estimate_name]][i] <- mean(pooled_pred, na.rm = TRUE)

    # pooled standard error
    pooled_se <- unlist(lapply(original_x, function(j) j$SE[i]), use.names = FALSE)
    ubar <- mean(pooled_se^2, na.rm = TRUE)
    tmp <- ubar + (1 + 1 / len) * stats::var(pooled_pred)
    pooled_predictions$SE[i] <- sqrt(tmp)
  }
  pooled_predictions
}


# caluclate confidence intervals for pooled estimates
.pool_ci_and_stats <- function(pooled_estimates, estimate_name, ci, dof) {
  # pooled degrees of freedom for t-statistics
  pooled_df <- .barnad_rubin(
    m = nrow(pooled_estimates),
    b = stats::var(pooled_estimates[[estimate_name]]),
    t = pooled_estimates$SE^2,
    dfcom = dof
  )

  # calculate confidence intervals
  alpha <- (1 + ci) / 2
  fac <- stats::qt(alpha, df = pooled_df)
  pooled_estimates$CI_low <- pooled_estimates[[estimate_name]] - fac * pooled_estimates$SE
  pooled_estimates$CI_high <- pooled_estimates[[estimate_name]] + fac * pooled_estimates$SE

  # update statistic column
  statistic <- pooled_estimates[[estimate_name]] / pooled_estimates$SE
  stat_column <- intersect(colnames(pooled_estimates), .stat_column_names())[1]
  if (!is.na(stat_column)) {
    pooled_estimates[[stat_column]] <- statistic
  }

  # udpate df and p-value
  pooled_estimates$df <- pooled_df
  if ("p" %in% colnames(pooled_estimates)) {
    pooled_estimates$p <- 2 * stats::pt(abs(statistic), df = pooled_df, lower.tail = FALSE)
  }

  pooled_estimates
}


# adjustment for degrees of freedom
.barnad_rubin <- function(m, b, t, dfcom = 999999) {
  # fix for z-statistic
  if (is.null(dfcom) || all(is.na(dfcom)) || all(is.infinite(dfcom))) {
    return(Inf)
  }
  lambda <- (1 + 1 / m) * b / t
  lambda[lambda < 1e-04] <- 1e-04
  dfold <- (m - 1) / lambda^2
  dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
  result <- dfold * dfobs / (dfold + dfobs)
  pmax(round(mean(result, na.rm = TRUE)), 1)
}


.stat_column_names <- function() {
  t_names <- c("t", "t-value", "t value", "t.value", "Pr(>|t|)")
  z_names <- c("z", "z-value", "z value", "z.value", "Pr(>|z|)", "Pr(>|Z|)", "Naive z", "Robust z", "san.z", "Wald Z")
  f_names <- c("F", "F-value", "F value", "F.value", "Pr(>|F|)")
  chi_names <- c("Chisq", "chi-sq", "chi.sq", "Wald", "W", "Pr(>|W|)")

  c(t_names, z_names, f_names, chi_names)
}
