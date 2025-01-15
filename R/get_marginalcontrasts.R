#' @rdname get_emmeans
#' @export
get_marginalcontrasts <- function(model,
                                  contrast = NULL,
                                  by = NULL,
                                  predict = NULL,
                                  comparison = "pairwise",
                                  marginalize = "average",
                                  ci = 0.95,
                                  p_adjust = "holm",
                                  verbose = TRUE,
                                  ...) {
  # check if available
  insight::check_if_installed("marginaleffects")


  # First step: prepare arguments ---------------------------------------------
  # ---------------------------------------------------------------------------

  # set default, if NULL
  if (is.null(contrast)) {
    contrast <- "auto"
  }

  # Guess arguments
  my_args <- .guess_marginaleffects_arguments(model, by, contrast, verbose = verbose, ...)

  # check whether contrasts should be made for numerics or categorical
  model_data <- insight::get_data(model, source = "mf", verbose = FALSE)
  on_the_fly_factors <- attributes(model_data)$factors

  # extract first focal term
  first_focal <- my_args$contrast[1]


  # Second step: compute contrasts, for slopes or categorical -----------------
  # ---------------------------------------------------------------------------

  # if first focal term is numeric, we contrast slopes
  if (is.numeric(model_data[[first_focal]]) && !first_focal %in% on_the_fly_factors) {
    # sanity check - contrast for slopes only makes sense when we have a "by" argument
    if (is.null(my_args$by)) {
      insight::format_error("Please specify the `by` argument to calculate contrasts of slopes.") # nolint
    }
    # call slopes with hypothesis argument
    out <- estimate_slopes(
      model = model,
      trend = my_args$contrast,
      by = my_args$by,
      ci = ci,
      hypothesis = comparison,
      backend = "marginaleffects",
      verbose = verbose,
      ...
    )
  } else {
    # for contrasts of categorical predictors, we call avg_predictions
    out <- estimate_means(
      model = model,
      by = unique(c(my_args$contrast, my_args$by)),
      ci = ci,
      hypothesis = comparison,
      predict = predict,
      backend = "marginaleffects",
      marginalize = marginalize,
      verbose = verbose,
      ...
    )
  }

  # adjust p-values
  out <- .p_adjust(model, out, p_adjust, verbose, ...)

  # Last step: Save information in attributes  --------------------------------
  # ---------------------------------------------------------------------------

  out <- .add_attributes(
    out,
    by = my_args$by,
    info = list(
      contrast = my_args$contrast,
      predict = predict,
      comparison = comparison,
      marginalize = marginalize,
      p_adjust = p_adjust
    )
  )

  class(out) <- unique(c("marginaleffects_contrasts", class(out)))
  out
}


# p-value adjustment --------------------------------------

.p_adjust <- function(model, params, p_adjust, verbose = TRUE, ...) {
  # extract information
  datagrid <- attributes(params)$datagrid
  focal <- attributes(params)$contrast
  statistic <- insight::get_statistic(model)$Statistic
  dof <- insight::get_df(model, type = "wald", verbose = FALSE)

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
  } else {
    insight::format_error(paste0("`p_adjust` must be one of ", toString(all_methods)))
  }
  params
}
