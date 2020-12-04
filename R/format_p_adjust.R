#' Format the name of the p-value adjustment methods
#'
#' Format the name of the p-value adjustment methods.
#'
#' @param method Name of the method.
#'
#' @examples
#' format_p_adjust("holm")
#' format_p_adjust("bonferroni")
#' @return String.
#' @export
format_p_adjust <- function(method) {
  method <- tolower(method)

  if(method == "holm"){
    out <- "Holm (1979)"
  } else if(method == "bonferroni"){
    out <- "Bonferroni"
  } else if(method == "hochberg"){
    out <- "Hochberg (1988)"
  } else if(method == "hommel"){
    out <- "Hommel (1988)"
  } else if(method %in% c("fdr", "bh")){
    out <- "Benjamini & Hochberg (1995)"
  } else if(method == "by"){
    out <- "Benjamini & Yekutieli (2001)"
  } else if(method == "none"){
    out <- "uncorrected"
  } else{
    out <- method
  }
  out
}
