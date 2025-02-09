#' @title Compute partial residuals from a data grid
#' @name residualize_over_grid
#'
#' @description This function computes partial residuals based on a data grid,
#' where the data grid is usually a data frame from all combinations of factor
#' variables or certain values of numeric vectors. This data grid is usually used
#' as `newdata` argument in `predict()`, and can be created with
#' [`insight::get_datagrid()`].
#'
#' @param grid A data frame representing the data grid, or an object of class
#' `estimate_means` or `estimate_predicted`, as returned by the different
#' `estimate_*()` functions.
#' @param model The model for which to compute partial residuals. The data grid
#' `grid` should match to predictors in the model.
#' @param predictor_name The name of the focal predictor, for which partial residuals
#' are computed.
#' @param protect_names Logical, if `TRUE`, preserves column names from the
#' `ggeffects` objects that is used as `grid`.
#' @param ... Currently not used.
#'
#' @section Partial Residuals:
#' For **generalized linear models** (glms), residualized scores are computed as
#' `inv.link(link(Y) + r)` where `Y` are the predicted values on the response
#' scale, and `r` are the *working* residuals.
#'
#' For (generalized) linear **mixed models**, the random effect are also
#' partialled out.
#'
#' @references
#' Fox J, Weisberg S. Visualizing Fit and Lack of Fit in Complex Regression
#' Models with Predictor Effect Plots and Partial Residuals. Journal of
#' Statistical Software 2018;87.
#'
#' @return A data frame with residuals for the focal predictor.
#'
#' @examplesIf requireNamespace("marginaleffects", quietly = TRUE)
#' set.seed(1234)
#' x1 <- rnorm(200)
#' x2 <- rnorm(200)
#' # quadratic relationship
#' y <- 2 * x1 + x1^2 + 4 * x2 + rnorm(200)
#'
#' d <- data.frame(x1, x2, y)
#' model <- lm(y ~ x1 + x2, data = d)
#'
#' pr <- estimate_means(model, c("x1", "x2"))
#' head(residualize_over_grid(pr, model))
#' @export
residualize_over_grid <- function(grid, model, ...) {
  UseMethod("residualize_over_grid")

}


#' @rdname residualize_over_grid
#' @export
residualize_over_grid.data.frame <- function(grid, model, predictor_name, ...) {

  old_d <- insight::get_predictors(model)
  fun_link <- insight::link_function(model)
  inv_fun <- insight::link_inverse(model)
  predicted <- grid[[predictor_name]]
  grid[[predictor_name]] <- NULL

  is_fixed <- sapply(grid, function(x) length(unique(x))) == 1
  grid <- grid[, !is_fixed, drop = FALSE]
  old_d <- old_d[, colnames(grid)[colnames(grid) %in% colnames(old_d)], drop = FALSE]

  if (!.is_grid(grid)) {
    insight::format_error("Grid for partial residuals must be a fully crossed grid.")
  }

  # for each var
  best_match <- NULL

  for (p in colnames(old_d)) {
    if (is.factor(old_d[[p]]) || is.logical(old_d[[p]]) || is.character(old_d[[p]])) {
      grid[[p]] <- as.character(grid[[p]])
      old_d[[p]] <- as.character(old_d[[p]])
    } else {
      grid[[p]] <- .validate_num(grid[[p]])
    }

    # if factor / logical / char in old data, find where it is equal
    # if numeric in old data, find where it is closest
    best_match <- .closest(old_d[[p]], grid[[p]], best_match = best_match)
  }

  idx <- apply(best_match, 2, which)
  idx <- sapply(idx, "[", 1)

  # extract working residuals
  res <- .safe(stats::residuals(model, type = "working"))

  # if failed, and model linear, extract response residuals
  if (is.null(res)) {
    minfo <- insight::model_info(model)
    if (minfo$is_linear) {
      res <- .safe(insight::get_residuals(model, type = "response"))
    }
  }

  if (is.null(res)) {
    insight::format_alert("Could not extract residuals.")
    return(NULL)
  }

  my_points <- grid[idx, , drop = FALSE]
  my_points[[predictor_name]] <- inv_fun(fun_link(predicted[idx]) + res) # add errors

  my_points
}



#' @rdname residualize_over_grid
#' @export
residualize_over_grid.estimate_means <- function(grid, model, ...) {
  new_d <- as.data.frame(grid)

  relevant_columns <- unique(c(
    attributes(grid)$trend,
    attributes(grid)$contrast,
    attributes(grid)$focal_terms,
    attributes(grid)$coef_name
  ))

  new_d <- new_d[colnames(new_d) %in% relevant_columns]

  residualize_over_grid(new_d, model, predictor_name = attributes(grid)$coef_name, ...)
}


# utilities --------------------------------------------------------------------


.is_grid <- function(df) {
  unq <- lapply(df, unique)

  if (prod(lengths(unq)) != nrow(df)) {
    return(FALSE)
  }

  df2 <- do.call(expand.grid, args = unq)
  df2$..1 <- 1

  res <- merge(df, df2, by = colnames(df), all = TRUE)

  sum(res$..1) == sum(df2$..1)
}


.closest <- function(x, target, best_match) {
  if (is.numeric(x)) {
    # AD <- outer(x, target, FUN = function(x, y) abs(x - y))
    AD <- abs(outer(x, target, FUN = `-`))
    idx <- apply(AD, 1, function(x) x == min(x))
  } else {
    idx <- t(outer(x, target, FUN = `==`))
  }

  if (is.matrix(best_match)) {
    idx <- idx & best_match
  }

  idx
}


.validate_num <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(as.character(x))
  }
  x
}
