#' Reshape Estimations with Draws to long format
#'
#' @param estimates Estimates with posterior draws.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' estimates <- estimate_response(model, keep_draws = TRUE, draws = 200)
#' reshape_draws(estimates)
#' }
#' @importFrom stats reshape
#' @export
reshape_draws <- function(estimates) {
  estimates$Index <- 1:nrow(estimates)
  long <- reshape(estimates,
                  varying = names(estimates)[grepl("Draw_", names(estimates))],
                  idvar = "Index",
                  v.names = "Draw",
                  timevar = "Draw_Index",
                  direction = "long"
  )
  row.names(long) <- NULL

  return(long)
}