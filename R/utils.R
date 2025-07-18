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
  } else if (length(out$SE) > 1 && isTRUE(all(out$SE == out$SE[1])) && insight::is_mixed_model(model)) {
    msg <- "Standard errors are probably not reliable. This can happen when random effects are involved. You may try `estimate_relation()` instead." # nolint
    if (!inherits(model, "glmmTMB")) {
      msg <- paste(msg, "You may also try package {.pkg glmmTMB} to produce valid standard errors.")
    }
    insight::format_alert(msg)
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
.is_likert <- function(x, integer_as_numeric = 5, ...) {
  if (is.null(integer_as_numeric) || is.na(integer_as_numeric)) {
    return(FALSE)
  }
  all(.is_integer(x)) && insight::n_unique(x) <= integer_as_numeric
}
