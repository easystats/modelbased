#' @rdname get_marginalmeans
#'
#' @param method Contrast method, respectively a formulation of the hypothesis
#' to test. See [this website](https://marginaleffects.com/bonus/hypothesis.html).
#' Will be passed to the `hypothesis` argument in `marginaleffects::avg_predictions()`.
#' @inheritParams get_marginalmeans
#' @inheritParams get_emcontrasts
#' @export
get_marginalcontrasts <- function(model,
                                  contrast = NULL,
                                  by = NULL,
                                  predict = NULL,
                                  method = "pairwise",
                                  ci = 0.95,
                                  p_adjust = "holm",
                                  ...) {
  # check if available
  insight::check_if_installed("marginaleffects")

  # set default, if NULL
  if (is.null(contrast)) {
    contrast <- "auto"
  }

  # Guess arguments
  my_args <- .guess_marginaleffects_arguments(model, by, contrast, ...)

  out <- estimate_means(
    model = model,
    ## TODO: once .format_marginaleffects_contrasts() is working, we have to
    ## pass only "contrast" to the `by` argument, and use `my_args$by` for
    ## filtering...
    by = unique(c(my_args$contrast, my_args$by)),
    ci = ci,
    hypothesis = method,
    predict = predict,
    backend = "marginaleffects",
    ...
  )

  attr(out, "contrast") <- my_args$contrast
  attr(out, "predict") <- predict
  attr(out, "p_adjust") <- p_adjust
  attr(out, "at") <- my_args$by
  attr(out, "by") <- my_args$by

  out
}


# p-value adjustment --------------------------------------

.p_adjust <- function(model, params, p_adjust, ...) {
  # extract information
  datagrid <- attributes(params)$datagrid
  focal <- attributes(params)$contrast
  statistic <- insight::get_statistic(model)$Statistic
  dof <- insight::get_df(model)

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
