# special contrasts: context effects ----------------------------------------
# ---------------------------------------------------------------------------

.get_contexteffects <- function(
  model,
  my_args,
  predict = NULL,
  transform = NULL,
  model_info,
  ...
) {
  # if we have stratified by another group, we need the difference between
  # contrasts at each group level
  if (is.null(my_args$by)) {
    comparison <- stats::as.formula("~I(diff(x))")
  } else {
    comparison <- stats::as.formula(paste("~I(diff(x)) |", my_args$by))
  }

  # prepare arguments
  dots <- list(...)
  fun_args <- insight::compact_list(list(
    model,
    variables = my_args$contrast,
    hypothesis = comparison,
    by = my_args$by
  ))

  # for non-Gaussian models, we need to modify arguments "type" and "transform"
  if (!model_info$is_linear) {
    # set default for "type" argument, if not provided
    if (is.null(predict)) {
      fun_args$type <- "link"
      # if "type" was not provided, also change transform argument. we do
      # this only when user did not provide "type", else - if user provided
      # "type" - we keep the default NULL
      if (is.null(transform)) {
        fun_args$transform <- "exp"
      }
    } else {
      fun_args$type <- predict
      fun_args$transform <- transform
    }
  }

  out <- do.call(marginaleffects::avg_comparisons, c(fun_args, dots))

  # pairwise comparison of context effects?
  if (identical(my_args$comparison, "context_pairwise")) {
    # save original levels for formatting
    original_levels <- .safe(out[[my_args$by]])
    # calculate pairwise comparisons
    out <- marginaleffects::hypotheses(out, hypothesis = ~pairwise)
    # format comparison levels. we first split the column with "b" parameter
    # names, like "(b1) - (b2)", into two columns. Then we remove the "b" and
    # just keep the number, which indicates the row.
    if (!is.null(original_levels)) {
      params <- as.data.frame(do.call(
        rbind,
        lapply(out$hypothesis, .split_at_minus_outside_parentheses)
      ))
      # split "b" strings and remove "b"
      params[] <- lapply(params, function(i) as.numeric(gsub("b", "", i, fixed = TRUE)))
      # give proper names
      colnames(params) <- c("Level1", "Level2")
      # replace numbers with original levels
      params$Level1 <- original_levels[params$Level1]
      params$Level2 <- original_levels[params$Level2]
      # save attributes
      att <- attributes(out)
      # remove old "by" column and bind new one back to output
      out[[my_args$by]] <- NULL
      out <- cbind(params, out)
      # add back original attributes
      cn <- colnames(out)
      attributes(out) <- utils::modifyList(attributes(out), att)
      colnames(out) <- cn
    }
  }

  # save some labels for printing
  attr(out, "by") <- attr(out, "hypothesis_by") <- my_args$by
  attr(out, "contrast") <- my_args$contrast
  attr(out, "context_effects") <- TRUE
  class(out) <- unique(c("marginaleffects_means", class(out)))
  out
}
