#' @importFrom bayestestR describe_posterior reshape_ci
#' @keywords internal
.summarize_posteriors <- function(posteriors,
                                  centrality = "median",
                                  ci = 0.89,
                                  ci_method = "hdi",
                                  test = c("pd", "rope"),
                                  rope_range = "default",
                                  rope_ci = 1,
                                  bf_prior = NULL) {
  if (inherits(posteriors, "emmGrid") | all(sapply(as.data.frame(posteriors), is.numeric))) {
    x <- bayestestR::describe_posterior(
      posteriors,
      ci = ci,
      ci_method = ci_method,
      centrality = centrality,
      test = test,
      rope_range = rope_range,
      rope_ci = rope_ci,
      bf_prior = bf_prior
    )

    x <- bayestestR::reshape_ci(x)

    if ("CI" %in% names(x) & length(unique(x$CI)) == 1) x$CI <- NULL
    if ("ROPE_CI" %in% names(x) & length(unique(x$ROPE_CI)) == 1) x$ROPE_CI <- NULL

    x$ROPE_low <- x$ROPE_high <- NULL
  } else {
    levels <- unique(unlist(sapply(posteriors, unique)))
    x <- sapply(posteriors, function(x) {
      freq <- as.data.frame(table(x) / nrow(posteriors))
      row.names(freq) <- freq$x
      freq$x <- NULL
      freq <- as.data.frame(t(freq))
      freq[levels[!levels %in% names(freq)]] <- 0
      row.names(freq) <- NULL
      freq
    })
    x <- as.data.frame(t(x))
    x <- as.data.frame(sapply(x, as.numeric))
  }
  x
}
