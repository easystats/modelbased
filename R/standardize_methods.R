#' @rdname visualisation_matrix
#' @importFrom effectsize standardize
#' @export
standardize.visualisation_matrix <- function(x, ...) {
  effectsize::standardize(as.data.frame(x), reference = attributes(x)$data, ...)
}







#' @rdname estimate_expectation
#' @export
standardize.estimate_predicted <- function(x, include_response = TRUE, ...) {
  # Get data of predictors
  data <- insight::get_data(attributes(x)$model, ...)
  data[[attributes(x)$response]] <- NULL  # Remove resp from data

  # Standardize predictors
  x[names(data)] <- effectsize::standardize(as.data.frame(x)[names(data)], reference = data)

  # Standardize response
  if(include_response == TRUE) {
    resp <- insight::get_response(attributes(x)$model)
    for(col in c("Predicted", "CI_low", "CI_high")) {
      if(col %in% names(x)) {
        x[col] <- effectsize::standardize(x[[col]], reference = resp)
      }
    }
  }
  x
}






#' @importFrom insight get_response model_info
#' @importFrom stats sd mad
#' @keywords internal
.standardize_contrasts <- function(contrasts, model, robust = FALSE) {
  vars <- names(contrasts)[names(contrasts) %in% c("Median", "Mean", "MAP", "Coefficient", "Difference")]
  if (insight::model_info(model)$is_linear) {
    response <- insight::get_response(model)
    if (robust) {
      std <- contrasts[vars] / stats::mad(response, na.rm = TRUE)
    } else {
      std <- contrasts[vars] / stats::sd(response, na.rm = TRUE)
    }
  } else {
    std <- contrasts[vars]
  }
  names(std) <- paste0("Std_", names(std))
  as.data.frame(std)
}


#' @importFrom insight get_response model_info get_predictors
#' @importFrom stats sd mad
#' @keywords internal
.standardize_slopes <- function(slopes, model, trend, robust = FALSE) {
  vars <- names(slopes)[names(slopes) %in% c("Median", "Mean", "MAP", "Coefficient")]
  x <- insight::get_predictors(model)[[trend]]
  if (insight::model_info(model)$is_linear) {
    response <- insight::get_response(model)
    if (robust) {
      std <- slopes[vars] * stats::mad(x, na.rm = TRUE) / stats::mad(response, na.rm = TRUE)
    } else {
      std <- slopes[vars] * stats::sd(x, na.rm = TRUE) / stats::sd(response, na.rm = TRUE)
    }
  } else {
    if (robust) {
      std <- slopes[vars] * stats::mad(x, na.rm = TRUE)
    } else {
      std <- slopes[vars] * stats::sd(x, na.rm = TRUE)
    }
  }
  names(std) <- paste0("Std_", names(std))
  as.data.frame(std)
}