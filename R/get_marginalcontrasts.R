#' @rdname get_marginalmeans
#'
#' @param method Contrast method.
#' @inheritParams get_emcontrasts
#' @export
get_marginalcontrasts <- function(model,
                                  contrast = NULL,
                                  by = NULL,
                                  predict = "response",
                                  method = "pairwise",
                                  ci = 0.95,
                                  ...) {
  # check if available
  insight::check_if_installed("marginaleffects")

  out <- estimate_means(
    model = model,
    by = c(contrast, by),
    ci = ci,
    hypothesis = method,
    predict = predict,
    backend = "marginaleffects",
    ...
  )

  attr(out, "contrast") <- contrast
  out
}

#' @rdname get_marginalmeans
#' @export
model_marginalcontrasts <- get_marginalcontrasts


# p-value adjustment -------------------

.p_adjust <- function(model, params, p_adjust, ...) {
  # extract information
  datagrid <- attributes(params)$datagrid
  focal <- attributes(params)$focal_terms
  statistic <- insight::get_statistic(model)$Statistic
  dof <- insight::get_df(model)
  verbose <- isTRUE(list(...)$verbose)

  # exit on NULL, or if no p-adjustment requested
  if (is.null(p_adjust) || identical(p_adjust, "none")) {
    return(params)
  }

  all_methods <- c(tolower(stats::p.adjust.methods), "tukey", "sidak")

  # needed for rank adjustment
  focal_terms <- datagrid[focal]
  rank_adjust <- prod(vapply(focal_terms, insight::n_unique, numeric(1)))

  # only proceed if valid argument-value
  if (tolower(p_adjust) %in% all_methods) {
    if (tolower(p_adjust) %in% tolower(stats::p.adjust.methods)) {
      # base R adjustments
      params[["p"]] <- stats::p.adjust(params[["p"]], method = p_adjust)
    } else if (tolower(p_adjust) == "tukey") {
      if (!is.null(statistic)) {
        # tukey adjustment
        params[["p"]] <- suppressWarnings(stats::ptukey(
          sqrt(2) * abs(statistic),
          rank_adjust,
          dof,
          lower.tail = FALSE
        ))
        # for specific contrasts, ptukey might fail, and the tukey-adjustement
        # could just be simple p-value calculation
        if (all(is.na(params[["p"]]))) {
          params[["p"]] <- 2 * stats::pt(abs(statistic), df = dof, lower.tail = FALSE)
        }
      } else if (verbose) {
        insight::format_alert("No test-statistic found. P-values were not adjusted.")
      }
    } else if (tolower(p_adjust) == "sidak") {
      # sidak adjustment
      params[["p"]] <- 1 - (1 - params[["p"]])^rank_adjust
    }
  } else if (verbose) {
    insight::format_alert(paste0("`p_adjust` must be one of ", toString(all_methods)))
  }
  params
}
