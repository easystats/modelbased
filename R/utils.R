#' @keywords internal
#' @noRd
.brms_aux_elements <- function(model = NULL) {
  out <- c(
    "sigma", "mu", "nu", "shape", "beta", "phi", "hu", "ndt", "zoi", "coi",
    "kappa", "bias", "bs", "zi", "alpha", "xi", "delta", "k"
  )
  unique(c(out, insight::find_auxiliary(model, verbose = FALSE)))
}


#' @keywords internal
#' @noRd
.valid_coefficient_names <- function(model = NULL) {
  out <- c(
    "Mean", "Probability", "Difference", "Ratio", "Rate", "ZI-Probability",
    "Proportion", "Median", "MAP", "Coefficient", "Odds_ratio"
  )
  dpars <- insight::find_auxiliary(model, verbose = FALSE)
  if (!is.null(dpars)) {
    out <- unique(c(out, tools::toTitleCase(dpars)))
  }
  out
}


# validate estimate argument and handle aliases
.validate_estimate_arg <- function(estimate) {
  # validate argument
  estimate <- insight::validate_argument(
    estimate,
    c("typical", "population", "specific", "average", "counterfactual")
  )
  # handle aliases
  if (estimate == "counterfactual") {
    estimate <- "population"
  }
  # return resuöt
  estimate
}


#' @keywords internal
#' @noRd
.check_standard_errors <- function(out,
                                   by = NULL,
                                   contrast = NULL,
                                   model = NULL,
                                   model_name = "model",
                                   verbose = TRUE,
                                   ...) {
  if (!verbose || is.null(out$SE)) {
    return(NULL)
  }

  if (all(is.na(out$SE))) {
    # we show an example code how to resolve the problem. this example
    # code only works when we have at least `by` or `contrast`. If both
    # are NULL, we ignore the example code (see below)
    code_snippet <- paste0("\n\nestim <- estimate_relation(\n  ", model_name)
    by_vars <- c(by, contrast)
    if (!is.null(by_vars)) {
      code_snippet <- paste0(
        code_snippet,
        ",\n  by = ",
        ifelse(length(by_vars) > 1, "c(", ""),
        paste0("\"", by_vars, "\"", collapse = ", "),
        ifelse(length(by_vars) > 1, ")", "")
      )
    }
    code_snippet <- paste0(code_snippet, "\n)\nestimate_contrasts(\n  estim")
    if (!is.null(contrast)) {
      code_snippet <- paste0(
        code_snippet,
        ",\n  contrast = ",
        ifelse(length(contrast) > 1, "c(", ""),
        paste0("\"", contrast, "\"", collapse = ", "),
        ifelse(length(contrast) > 1, ")", "")
      )
    }
    code_snippet <- paste0(code_snippet, "\n)")
    # setup message
    msg <- insight::format_message(
      "Could not calculate standard errors for contrasts. This can happen when random effects are involved. You may try following:"
    )
    # add example code, if valid
    if (!is.null(by_vars)) {
      msg <- c(msg, insight::color_text(code_snippet, "green"), "\n")
    }
    message(msg)

    # disable message for now, see
    # https://github.com/easystats/modelbased/issues/526
    # } else if (length(out$SE) > 1 && isTRUE(all(out$SE == out$SE[1])) && insight::is_mixed_model(model)) {
    #   msg <- "Standard errors are probably not reliable. This can happen when random effects are involved. You may try `estimate_relation()` instead." # nolint
    #   if (!inherits(model, "glmmTMB")) {
    #     msg <- paste(msg, "You may also try package {.pkg glmmTMB} to produce valid standard errors.")
    #   }
    #   insight::format_alert(msg)
  }
}


#' @keywords internal
#' @noRd
.safe <- function(code, on_error = NULL) {
  if (isTRUE(getOption("easystats_errors", FALSE)) && is.null(on_error)) {
    code
  } else {
    tryCatch(code, error = function(e) on_error)
  }
}


#' @keywords internal
#' @noRd
.is_integer <- function(x) {
  tryCatch(
    expr = {
      ifelse(is.infinite(x), FALSE, x %% 1 == 0)
    },
    warning = function(w) {
      is.integer(x)
    },
    error = function(e) {
      FALSE
    }
  )
}


#' @keywords internal
#' @noRd
.is_likert <- function(x, integer_as_continuous = 5, verbose = TRUE, ...) {
  # check if argument is missing or not - message only shown when missing
  missing_default <- missing(integer_as_continuous)

  # check for global option
  if (!is.null(getOption("modelbased_integer"))) {
    integer_as_continuous <- getOption("modelbased_integer")
  }

  # no need to check if check is disabled
  if (is.null(integer_as_continuous) || is.na(integer_as_continuous) || isTRUE(integer_as_continuous)) {
    return(FALSE)
  }

  # integer-values, and no more than `integer_as_continuous` unique values?
  is_likert <- all(.is_integer(x)) && insight::n_unique(x) <= integer_as_continuous

  # tell user, this handling might not be desired - but only if we have
  # more than 2 unique values, otherwise it's assumed to be a binary variable
  if (is_likert && verbose && missing_default && insight::n_unique(x) > 2) {
    insight::format_alert(
      "Numeric variable appears to be ordinal or Likert-scale (integer values, no more than 5 unique values) and is treated as discrete variable. Set `integer_as_continuous = TRUE` to disable this check and always treat numeric variables as continuous."
    )
  }
  is_likert
}
