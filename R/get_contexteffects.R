# special contrasts: context effects ----------------------------------------
# ---------------------------------------------------------------------------

.get_contexteffects <- function(
  model,
  my_args,
  predict = NULL,
  transform = NULL,
  post_process = NULL,
  model_info,
  iterations = NULL,
  verbose = TRUE,
  ...
) {
  # if we have stratified by another group, we need the difference between
  # contrasts at each group level
  if (is.null(my_args$by)) {
    comparison <- stats::as.formula("~revpairwise")
  } else if (length(my_args$by) > 2) {
    # it is not possible to have more than two by-variables for now
    insight::format_error(
      "It is not possible to have more than two variables in `by` when calculating contrasts of slopes."
    )
  } else {
    comparison <- stats::as.formula(paste(
      "~revpairwise |",
      paste(my_args$by, collapse = "+")
    ))
  }

  # prepare arguments
  dots <- .check_dots_data(list(...), verbose)
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

  # bayesian models: number of posterior draws to use, passed to `ndraws`
  if (!is.null(iterations)) {
    fun_args$ndraws <- iterations
  }

  out <- do.call(marginaleffects::avg_comparisons, c(fun_args, dots))

  # pairwise comparison of context effects?
  if (identical(my_args$comparison, "context_pairwise")) {
    # if we have more than one by-variable, use the last one for stratification
    stratify_by <- my_args$by[length(my_args$by)]
    # save original levels from the contrast-variable for formatting
    original_levels <- .safe(out[[my_args$by[1]]])
    if (length(my_args$by) > 1) {
      comparison <- stats::as.formula(paste(
        "~pairwise |",
        paste(stratify_by, collapse = "+")
      ))
    } else {
      comparison <- ~pairwise
    }
    # calculate pairwise comparisons
    out <- marginaleffects::hypotheses(out, hypothesis = comparison)
  }

  # process subsequential comparisons, if any
  out <- .post_process_comparisons(out, post_process = post_process, verbose = verbose)

  # for pairwise comparison of context effects, we want to fix labels
  if (identical(my_args$comparison, "context_pairwise")) {
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
      # replace numbers with original levels
      params[] <- lapply(params, function(i) original_levels[i])
      # if we had post-processing, we may have more than two columns
      if (ncol(params) > 2) {
        params[[1]] <- paste(params$V1, params$V2, sep = " - ")
        params[[2]] <- paste(params$V3, params$V4, sep = " - ")
        # for now, we assume that we have maximum four columns that should be
        # merged into two columns
        params <- params[c(1, 2)]
      }
      # give proper names
      colnames(params) <- c("Level1", "Level2")
      # save attributes
      att <- attributes(out)
      # remove old "by" column and bind new one with the contrast-levels back to output
      out[[my_args$by[1]]] <- NULL
      out <- cbind(params, out)
      # add back original attributes
      cn <- colnames(out)
      attributes(out) <- utils::modifyList(attributes(out), att)
      colnames(out) <- cn
    }
  }

  # not needed
  out$hypothesis <- NULL

  # save some labels for printing
  attr(out, "by") <- attr(out, "hypothesis_by") <- my_args$by
  attr(out, "contrast") <- my_args$contrast
  attr(out, "context_effects") <- TRUE
  class(out) <- unique(c("marginaleffects_means", class(out)))
  out
}
