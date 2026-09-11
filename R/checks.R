#' @keywords internal
#' @noRd
.check_custom_contrasts_and_filter <- function(
  estimate,
  by,
  original_contrast,
  comparison
) {
  # setup message to tell user that results must be cross-checked
  if (estimate == "average" && !all(.grep_cleaned_by_vars(by) == by)) {
    # first, extract contrast with filtering, which doesn't work
    wrong_contrast <- setdiff(original_contrast, .grep_cleaned_by_vars(original_contrast))
    # clean contrast and by, used to show correct example
    original_contrast <- .grep_cleaned_by_vars(original_contrast)
    original_by <- setdiff(.grep_cleaned_by_vars(by), original_contrast)
    msg1 <- paste0(
      "Selecting specific levels or values in the `contrast` or `by` arguments ",
      if (length(wrong_contrast)) {
        paste0(
          "(e.g., ",
          paste0(
            "`contrast = c(",
            paste0("\"", wrong_contrast, "\"", collapse = ", "),
            ")`) "
          )
        )
      },
      "is error-prone for custom contrasts like ",
      paste0("`comparison = \"", comparison, "\"`"),
      " in combination with `estimate = \"average\"`. This can yield incorrect results,",
      " even if the output suggests the correct comparisons. It is strongly recommended",
      " to use only bare variable names in `contrast` and `by`, e.g.\n\n"
    )
    msg2 <- insight::color_text(
      paste0(
        "  estimate_contrasts(\n",
        "    contrast = c(",
        paste0("\"", original_contrast, "\"", collapse = ", "),
        "),\n",
        if (length(original_by)) {
          paste0("    by = c(", paste0("\"", original_by, "\"", collapse = ", "), "),\n")
        },
        "    estimate = \"average\",\n    comparison = ...\n  )"
      ),
      color = "green"
    )
    msg3 <- "\n\n  and update your `comparison` argument accordingly.\n  Run\n\n"
    msg4 <- insight::color_text(
      paste0(
        "  estimate_means(\n",
        "    c(",
        paste0("\"", c(original_contrast, original_by), "\"", collapse = ", "),
        "),\n    estimate = \"average\"\n  )"
      ),
      color = "green"
    )
    msg5 <- "\n\n  first to find out the correct rows to specify the `b`-coefficients for the `comparison` argument.\n"
    warning(
      insight::format_message(msg1),
      msg2,
      msg3,
      msg4,
      insight::format_message(msg5),
      call. = FALSE
    )
  }
}


#' @keywords internal
#' @noRd
.check_standard_errors <- function(
  out,
  by = NULL,
  contrast = NULL,
  model = NULL,
  model_name = "model",
  verbose = TRUE,
  ...
) {
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
      "Could not calculate standard errors for contrasts. This can happen when random effects are involved."
    )
    # add example code, if valid
    if (!is.null(by_vars)) {
      msg <- c(
        paste(msg, "You may try following:"),
        insight::color_text(code_snippet, "green"),
        "\n"
      )
    }
    message(msg)

    # disable message for now, see
    # https://github.com/easystats/modelbased/issues/526
    # } else if (length(out$SE) > 1 && isTRUE(all(out$SE == out$SE[1])) && insight::is_mixed_model(model)) {
    #   msg <- "Standard errors are probably not reliable. This can happen when random effects are involved. You may try `estimate_relation()` instead."
    #   if (!inherits(model, "glmmTMB")) {
    #     msg <- paste(msg, "You may also try package {.pkg glmmTMB} to produce valid standard errors.")
    #   }
    #   insight::format_alert(msg)
  }
}


#' @keywords internal
#' @noRd
.check_offset <- function(
  model,
  estimate,
  offset = NULL,
  my_args = NULL,
  verbose = TRUE
) {
  model_offset <- insight::find_offset(model)
  # check if model has an offset at all
  if (!is.null(model_offset) && !any(startsWith(my_args$by, model_offset)) && verbose) {
    msg <- NULL
    if (is.null(offset)) {
      # if no offset argument was specified, tell user what this means
      msg <- switch(
        estimate,
        specific = ,
        typical = paste(
          "Model contains an offset-term, which is set to its mean value.",
          "If you want to average predictions over the distribution of the offset",
          "(if appropriate), use `estimate = \"average\"` or `estimate = \"population\"`.",
          "If you want to fix the offset to a specific value, for instance `1`,",
          "use `offset = 1`."
        ),
        average = ,
        population = paste(
          "Model contains an offset-term and you average predictions over the",
          "distribution of that offset. If you want to fix the offset to a",
          "specific value, for instance `1`, use `offset = 1`."
        )
      )
      # if offset term is log-transformed, tell user. offset should be fixed then
      log_offset <- insight::find_transformation(insight::find_offset(
        model,
        as_term = TRUE
      ))
      if (!is.null(log_offset) && startsWith(log_offset, "log")) {
        msg <- c(
          msg,
          paste(
            "We also found that the model has a log-transformed offset term.",
            "If you use the `offset` argument, the log-transformation will",
            "automatically be applied to the provided offset-value. I.e., consider",
            "using, for instance, `offset = 10` and not `offset = log(10)`."
          )
        )
      }
    }
    if (!is.null(msg)) {
      insight::format_alert(msg)
    }
  }
}


#' @keywords internal
#' @noRd
.check_dots_data <- function(dots, verbose) {
  if (!is.null(dots$data)) {
    if (!is.null(dots$newdata) && verbose) {
      insight::format_alert(
        "Both 'data' and 'newdata' were provided. Please specify only one. Ignoring 'newdata' and using 'data' instead."
      )
    }
    dots$newdata <- dots$data
    dots$data <- NULL
  }
  dots
}


#' @keywords internal
#' @noRd
.check_for_inequality_comparison <- function(comparison) {
  # check whether we have a formula definition of inequality comparisons,
  # and convert it to a string
  #
  # the default formulas are converted to a string:
  # ~inequality -> "inequality"
  # inequality ~ pairwise -> "inequality_pairwise"
  # ratio ~ inequality -> "inequality_ratio"
  # ratio ~ inequality + pairwise` -> "inequality_ratio_pairwise"
  #
  # we may have other formulas that control grouping and averaging, like
  # `~ inequality | grp1 + grp2`. In this case, the formula is returned as is
  # and processed later in ".process_inequality_formula()"
  if (inherits(comparison, "formula")) {
    # parse variables into a string
    out <- paste(all.vars(comparison), collapse = "_")
    # handle special cases
    out <- switch(
      out,
      ratio_inequality = "inequality_ratio",
      ratio_inequality_pairwise = "inequality_ratio_pairwise",
      out
    )
    if (.is_inequality_comparison(out)) {
      return(out)
    }
  }
  comparison
}


#' @keywords internal
#' @noRd
.check_format_backend <- function(...) {
  # we allow exporting HTML format based on "gt" or "tinytable"
  dots <- list(...)
  if (identical(dots$backend, "tt")) {
    "tt"
  } else {
    "html"
  }
}


#' @keywords internal
#' @noRd
.check_predict_arg <- function(predict, valid_types, error_arg) {
  if (isTRUE(is.na(predict))) {
    # add modelbased-options to valid types
    valid_types <- unique(c("response", "link", valid_types))
    insight::format_error(paste0(
      "The option provided in the `",
      error_arg,
      "` argument is not recognized.",
      " Valid options are: ",
      datawizard::text_concatenate(valid_types, enclose = "`"),
      "."
    ))
  }
}


# handle errors from marginaleffects -----------------------------------------
#
# This helper function processes errors that occur during calls to the
# {marginaleffects} package. It creates more informative and user-friendly
# error messages by inspecting the original error and suggesting potential
# solutions for common problems, such as using a different `estimate` option
# or switching to the `emmeans` backend.
#
# Arguments:
# - out: The error object returned from the `tryCatch` block.
# - fun_args: A list of arguments that were passed to the failing
#   {marginaleffects} function.
#
# returns: A character vector containing the formatted, user-friendly error
#   message, which is then passed to `insight::format_error()`.
#
#' @keywords internal
#' @noRd
.check_marginaleffects_errors <- function(out, fun_args) {
  # what was requested?
  if (is.null(fun_args$hypothesis)) {
    fun <- "marginal means"
  } else {
    fun <- "marginal contrasts"
  }
  # clean original error message
  out$message <- gsub("\\s+", " ", gsub("\n", "", out$message, fixed = TRUE))
  # setup clear error message
  msg <- c(
    paste0("Sorry, calculating ", fun, " failed with following error:"),
    insight::color_text(gsub("\n", "", out$message, fixed = TRUE), "red")
  )
  # handle exceptions ------------------------------------------------------
  # we get this error when we should use counterfactuals
  if (grepl("not found in column names", out$message, fixed = TRUE)) {
    msg <- c(
      msg,
      "\nIt seems that not all required levels of the focal terms are available in the provided data. If you want predictions extrapolated to a hypothetical target population, try setting `estimate=\"population\"."
    )
  }
  # we get this error for models with complex random effects structures in glmmTMB,
  # or when the data grid is too large
  if (
    grepl("map factor length must equal", out$message, fixed = TRUE) ||
      grepl("cannot allocate", out$message, fixed = TRUE)
  ) {
    msg <- c(
      msg,
      paste0(
        "\nYou may try using the `emmeans` backend, e.g. `estimate_means(model, by = c(",
        toString(paste0("\"", fun_args$by, "\"")),
        "), backend = \"emmeans\")`, or use `estimate_relation(model, by = c(",
        toString(paste0("\"", fun_args$by, "\"")),
        "))` instead. For contrasts or pairwise comparisons, save the output of `estimate_relation()` and pass it to `estimate_contrasts()`, e.g.\n"
      ),
      paste0(
        "out <- estimate_relation(model, by = c(",
        toString(paste0("\"", fun_args$by, "\"")),
        "))"
      ),
      paste0(
        "estimate_contrasts(out, contrast = c(",
        toString(paste0("\"", fun_args$by, "\"")),
        "))"
      )
    )
  }
  msg
}


#' @keywords internal
#' @noRd
.check_filter_args <- function(result, prefix = "") {
  if (nrow(result) == 0) {
    # small helper, because we have the same error message in several places
    insight::format_error(
      prefix,
      "Please check your `by` and `contrast` arguments, or try one of the following options:",
      "1. Use a different option for the `estimate` argument, e.g. `estimate = \"typical\"`.",
      "2. Use the `newdata` argument to provide a data grid of predictor values at which to evaluate predictions."
    )
  }
}


#' @param trend The `trend` argument
#' @return updated `trend` variable
#' @keywords internal
#' @noRd
.check_trend_arg <- function(trend, verbose = TRUE) {
  if (length(trend) > 1) {
    if (verbose) {
      insight::format_alert(paste0(
        "More than one numeric variable was selected for slope estimation. Keeping only `",
        trend[1],
        "`. ",
        "If you want to estimate the slope of `",
        trend[1],
        "` at different values of `",
        trend[2],
        "`, use `by=\"",
        trend[2],
        "\"` instead."
      ))
    }
    trend <- trend[1]
  }
  trend
}
