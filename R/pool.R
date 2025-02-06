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

  len <- length(x)
  ci <- attributes(x[[1]])$ci
  dof <- x[[1]]$df

  if (is.null(dof)) {
    dof <- Inf
  }

  # pool predictions -----

  pooled_comparisons <- original_x[[1]]
  pooled_comparisons$SE <- NA
  n_rows <- nrow(original_x[[1]])

  for (i in 1:n_rows) {
    # pooled estimate
    pooled_comp <- unlist(lapply(original_x, function(j) j[[estimate_name]][i]), use.names = FALSE)
    pooled_comparisons[[estimate_name]][i] <- mean(pooled_comp, na.rm = TRUE)

    # pooled standard error
    pooled_se <- unlist(lapply(original_x, function(j) j$SE[i]), use.names = FALSE)
    ubar <- mean(pooled_se^2, na.rm = TRUE)
    tmp <- ubar + (1 + 1 / len) * stats::var(pooled_comp)
    pooled_comparisons$SE[i] <- sqrt(tmp)
  }

  # pooled degrees of freedom for t-statistics
  pooled_df <- .barnad_rubin(
    m = nrow(pooled_comparisons),
    b = stats::var(pooled_comparisons[[estimate_name]]),
    t = pooled_comparisons$SE^2,
    dfcom = dof
  )

  # confidence intervals ----
  alpha <- (1 + ci) / 2
  fac <- stats::qt(alpha, df = dof)
  pooled_comparisons$CI_low <- pooled_comparisons[[estimate_name]] - fac * pooled_comparisons$SE
  pooled_comparisons$CI_high <- pooled_comparisons[[estimate_name]] + fac * pooled_comparisons$SE


  attributes(pooled_comparisons) <- utils::modifyList(attributes(original_x[[1]]), attributes(pooled_comparisons))
  pooled_comparisons
}


#' Pool Estimated Marginal Means
#'
#' This function "pools" (i.e. combines) multiple `estimate_means` objects, in
#' a similar fashion as [`mice::pool()`].
#'
#' @param x A list of `estimate_means` objects, as returned by [`estimate_means()`].
#' @param ... Currently not used.
#' @inheritParams estimate_means
#'
#' @details Averaging of parameters follows Rubin's rules (*Rubin, 1987, p. 76*).
#' Pooling is applied to the predicted values on the scale of the *linear predictor*,
#' not on the response scale, in order to have accurate pooled estimates and
#' standard errors. The final pooled predicted values are then transformed to
#' the response scale, using [`insight::link_inverse()`].
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
#' pool_means(predictions)
#' @return A data frame with pooled predictions.
#' @export
pool_means <- function(x, transform = NULL, ...) {
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

  len <- length(x)
  ci <- attributes(x[[1]])$ci
  model <- attributes(x[[1]])$model
  dof <- x[[1]]$df
  link_inv <- insight::link_inverse(model)
  link_fun <- insight::link_function(model)

  if (is.null(link_inv)) {
    link_inv <- function(x) x
  }
  if (is.null(link_fun)) {
    link_fun <- function(x) x
  }
  if (is.null(dof)) {
    dof <- Inf
  }
  if (isTRUE(transform)) {
    transform_fun <- insight::get_transformation(model, verbose = FALSE)$inverse
  } else {
    transform_fun <- transform
  }

  # pool predictions -----

  pooled_predictions <- original_x[[1]]
  n_rows <- nrow(original_x[[1]])

  for (i in 1:n_rows) {
    # pooled estimate
    pooled_pred <- unlist(lapply(original_x, function(j) link_fun(j[[estimate_name]][i])), use.names = FALSE)
    pooled_predictions[[estimate_name]][i] <- mean(pooled_pred, na.rm = TRUE)

    # pooled standard error
    pooled_se <- unlist(lapply(original_x, function(j) j$SE[i]), use.names = FALSE)
    ubar <- mean(pooled_se^2, na.rm = TRUE)
    tmp <- ubar + (1 + 1 / len) * stats::var(pooled_pred)
    pooled_predictions$SE[i] <- sqrt(tmp)
  }

  # pooled degrees of freedom for t-statistics
  pooled_df <- .barnad_rubin(
    m = nrow(pooled_predictions),
    b = stats::var(pooled_predictions[[estimate_name]]),
    t = pooled_predictions$SE^2,
    dfcom = dof
  )

  # confidence intervals ----
  alpha <- (1 + ci) / 2
  fac <- stats::qt(alpha, df = pooled_df)
  pooled_predictions$CI_low <- link_inv(pooled_predictions[[estimate_name]] - fac * pooled_predictions$SE)
  pooled_predictions$CI_high <- link_inv(pooled_predictions[[estimate_name]] + fac * pooled_predictions$SE)

  # back-transform response and CI?
  if (!is.null(transform_fun)) {
    pooled_predictions[[estimate_name]] <- transform_fun(pooled_predictions[[estimate_name]])
    pooled_predictions$CI_low <- transform_fun(pooled_predictions$CI_low)
    pooled_predictions$CI_high <- transform_fun(pooled_predictions$CI_high)
  }

  # backtransform
  pooled_predictions[[estimate_name]] <- link_inv(pooled_predictions[[estimate_name]])

  pooled_predictions
}


# helper ------

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
