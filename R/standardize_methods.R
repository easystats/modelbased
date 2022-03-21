# Standardize -------------------------------------------------------------

#' @export
standardize.estimate_predicted <- function(x, include_response = TRUE, ...) {
  # Get data of predictors
  data <- insight::get_data(attributes(x)$model, ...)
  data[[attributes(x)$response]] <- NULL # Remove resp from data

  # Standardize predictors
  x[names(data)] <- datawizard::standardize(as.data.frame(x)[names(data)], reference = data, ...)

  # Standardize response
  if (include_response == TRUE && insight::model_info(attributes(x)$model)$is_linear) {
    resp <- insight::get_response(attributes(x)$model)
    disp <- attributes(datawizard::standardize(resp, ...))$scale

    for (col in c("Predicted", "Mean", "CI_low", "CI_high")) {
      if (col %in% names(x)) {
        x[col] <- datawizard::standardize(x[[col]], reference = resp, ...)
      }
    }

    for (col in c("SE", "MAD")) {
      if (col %in% names(x)) {
        x[col] <- x[[col]] / disp
      }
    }
  }

  attr(x, "table_title") <- c(paste(attributes(x)$table_title[1], " (standardized)"), "blue")
  x
}


#' @export
standardize.estimate_means <- standardize.estimate_predicted


#' @export
standardize.estimate_contrasts <- function(x, robust = FALSE, ...) {
  model <- attributes(x)$model

  if (insight::model_info(model)$is_linear) {
    # Get dispersion scaling factor
    if (robust) {
      disp <- stats::mad(insight::get_response(model), na.rm = TRUE)
    } else {
      disp <- stats::sd(insight::get_response(model), na.rm = TRUE)
    }

    # Standardize relevant cols
    for (col in c("Difference", "Coefficient", "SE", "MAD", "CI_low", "CI_high")) {
      if (col %in% names(x)) {
        x[col] <- x[[col]] / disp
      }
    }
  }

  attr(x, "table_title") <- c(paste(attributes(x)$table_title[1], " (standardized)"), "blue")
  x
}

#' @export
standardize.estimate_slopes <- standardize.estimate_contrasts


# Unstandardize -------------------------------------------------------------

#' @method unstandardize estimate_predicted
#' @export
unstandardize.estimate_predicted <- function(x, include_response = TRUE, ...) {
  # Get data of predictors
  data <- insight::get_data(attributes(x)$model, ...)
  data[[attributes(x)$response]] <- NULL # Remove resp from data

  # Standardize predictors
  x[names(data)] <- datawizard::unstandardize(as.data.frame(x)[names(data)], reference = data, ...)

  # Standardize response
  if (include_response == TRUE && insight::model_info(attributes(x)$model)$is_linear) {
    resp <- insight::get_response(attributes(x)$model)
    disp <- attributes(datawizard::standardize(resp, ...))$scale

    for (col in c("Predicted", "Mean", "CI_low", "CI_high")) {
      if (col %in% names(x)) {
        x[col] <- datawizard::unstandardize(x[[col]], reference = resp, ...)
      }
    }

    for (col in c("SE", "MAD")) {
      if (col %in% names(x)) {
        x[col] <- x[[col]] * disp
      }
    }
  }
  x
}
