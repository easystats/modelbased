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
