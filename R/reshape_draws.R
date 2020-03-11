#' Reshape estimations with Bayesian posterior draws to long format
#'
#' Reshape data.frame of estimations with Bayesian posterior draws to long format.
#'
#' @param draws data.frame containing posterior draws obtained from \code{estimate_response} or \code{estimate_link}.
#'
#' @examples
#' \donttest{
#' if (require("rstanarm")) {
#'   model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'   estimates <- estimate_response(model, keep_draws = TRUE, draws = 200)
#'   reshape_draws(estimates)
#' }
#' }
#' @return Data frame of reshaped draws in long format.
#' @importFrom stats reshape
#' @export
reshape_draws <- function(draws) {

  # Sanity check
  if(sum(grepl("Draw", names(draws))) == 0){
    stop("Couldn't find columns corresponding to draws ('Draw_*') in your dataframe.")
  }

  draws$Index <- 1:nrow(draws)
  long <- stats::reshape(draws,
    varying = names(draws)[grepl("Draw_", names(draws))],
    idvar = "Index",
    v.names = "Draw",
    timevar = "Draw_Group",
    direction = "long"
  )
  row.names(long) <- NULL

  long
}
