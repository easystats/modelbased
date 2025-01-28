#' @rdname get_emmeans
#' @export
get_marginalcontrasts <- function(model,
                                  contrast = NULL,
                                  by = NULL,
                                  predict = NULL,
                                  comparison = "pairwise",
                                  marginalize = "average",
                                  ci = 0.95,
                                  p_adjust = "none",
                                  verbose = TRUE,
                                  ...) {
  # check if available
  insight::check_if_installed("marginaleffects")

  # temporarily overwrite settings that error on "too many" rows
  me_option <- getOption("marginaleffects_safe")
  options(marginaleffects_safe = FALSE)
  on.exit(options(marginaleffects_safe = me_option))


  # First step: prepare arguments ---------------------------------------------
  # ---------------------------------------------------------------------------

  # set default, if NULL
  if (is.null(contrast)) {
    contrast <- "auto"
  }

  # check whether contrasts should be made for numerics or categorical
  model_data <- insight::get_data(model, source = "mf", verbose = FALSE)
  on_the_fly_factors <- attributes(model_data)$factors

  # Guess arguments
  my_args <- .guess_marginaleffects_arguments(model, by, contrast, verbose = verbose, ...)

  # sanitize comparison argument, to ensure compatibility between different
  # marginaleffects versions - newer versions don't accept a string argument,
  # only formulas (older versions don't accept formulas)
  my_args <- .get_marginaleffects_hypothesis_argument(comparison, my_args, model_data, ...)

  # extract first focal term
  first_focal <- my_args$contrast[1]

  # sanity check - is it a list? if so, use name
  if (is.list(first_focal)) {
    first_focal <- names(first_focal)
  }


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
      hypothesis = my_args$comparison_slopes,
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
      hypothesis = my_args$comparison,
      predict = predict,
      backend = "marginaleffects",
      marginalize = marginalize,
      verbose = verbose,
      ...
    )
  }

  # adjust p-values
  if (!insight::model_info(model)$is_bayesian) {
    out <- .p_adjust(model, out, p_adjust, verbose, ...)
  }


  # Last step: Save information in attributes  --------------------------------
  # ---------------------------------------------------------------------------

  out <- .add_attributes(
    out,
    by = my_args$by,
    info = list(
      contrast = my_args$contrast,
      predict = predict,
      comparison = my_args$comparison,
      marginalize = marginalize,
      p_adjust = p_adjust
    )
  )

  # remove "estimate_means" class attribute
  class(out) <- setdiff(
    unique(c("marginaleffects_contrasts", class(out))),
    "estimate_means"
  )
  out
}


# make "comparison" argument compatible -----------------------------------

.get_marginaleffects_hypothesis_argument <- function(comparison, my_args, model_data = NULL, ...) {
  # init
  comparison_slopes <- NULL
  original_by <- my_args$by

  # make sure "by" is a valid column name, and no filter-directive, like "Species='setosa'".
  if (!is.null(my_args$by) && any(grepl("[^0-9A-Za-z\\.]", my_args$by))) {

    ## TODO: handle with by-filters
    # for things like estimate_contrasts(model, "gear", by = "am='1'"), we can't
    # use `by` as group in the formula, thus we remove by here - but it's still
    # saved in `original_by`. We could use `original_by` either for creating a
    # data grid, or for filtering. Currently, this is not supported for
    # `estimate_contrasts()` (but for estimate_means()).

    my_args$by <- NULL
  }

  # convert comparison and by into a formula
  if (!is.null(comparison)) {
    # only proceed if we don't have custom comparisons
    if (!.is_custom_comparison(comparison)) {
      # if we have a formula as comparison, we convert it into strings in order to
      # extract the information for "comparison" and "by", as we need for processing
      # in modelbased.
      if (inherits(comparison, "formula")) {
        # check if we have grouping in the formula, indicated via "|". we split
        # the formula into the three single components: lhs ~ rhs | group
        f <- insight::trim_ws(unlist(strsplit(insight::safe_deparse(comparison), "[~|]")))
        # extract formula parts
        formula_lhs <- f[1]
        formula_rhs <- f[2]
        formula_group <- f[3]
        # can be NA when no group
        if (is.na(formula_group) || !nzchar(formula_group)) {
          # no grouping via formula
          formula_group <- NULL
        } else {
          # else, if we have groups, update by-argument
          my_args$by <- formula_group
        }
      } else {
        # sanity check for "comparison" argument
        insight::validate_argument(comparison, .valid_hypothesis_strings())
        formula_lhs <- "difference"
        formula_rhs <- comparison
      }
      # we put "by" into the formula. user either provided "by", or we put the
      # group variable from the formula into "by", hence, "my_args$by" definitely
      # contains the requested groups
      formula_group <- my_args$by
      # compose formula
      f <- paste(formula_lhs, "~", paste(formula_rhs, collapse = "+"))
      # for contrasts of slopes, we don *not* want the group-variable in the formula
      comparison_slopes <- stats::as.formula(f)
      # add group variable and update by
      if (!is.null(formula_group)) {
        f <- paste(f, "|", paste(formula_group, collapse = "+"))
        my_args$by <- formula_group
      }
      comparison <- stats::as.formula(f)
    }
  } else {
    # default to pairwise
    comparison <- comparison_slopes <- ~pairwise
  }
  # remove "by" from "contrast"
  my_args$contrast <- setdiff(my_args$contrast, my_args$by)

  c(
    # the "my_args" argument, containing "by" and "contrast"
    my_args,
    list(
      # the modifed comparison, as formula, which also includes "by" as group
      comparison = comparison,
      # the modifed comparison, as formula, excluding "by" as group
      comparison_slopes = comparison_slopes,
      # the original "by" value, might be required for filtering
      # (e.g. when `by = "Species='setosa'"`)
      original_by = original_by
    )
  )
}


.valid_hypothesis_strings <- function() {
  c(
    "pairwise", "reference", "sequential", "meandev", "meanotherdev",
    "revpairwise", "revreference", "revsequential", "poly", "helmert",
    "trt_vs_ctrl"
  )
}


# check for custom hypothesis  --------------------------------------

.is_custom_comparison <- function(comparison) {
  !is.null(comparison) &&
    length(comparison) == 1 &&
    is.character(comparison) &&
    grepl("=", comparison, fixed = TRUE) &&
    grepl("\\bb\\d+\\b", comparison)
}


.extract_custom_comparison <- function(comparison) {
  # find all "b" strings
  matches <- gregexpr("\\bb\\d+\\b", comparison)[[1]]
  match_lengths <- attr(matches, "match.length")

  # extract all "b" strings, so we have a vector of all "b" used in the comparison
  unlist(lapply(seq_along(matches), function(i) {
    substr(comparison, matches[i], matches[i] + match_lengths[i] - 1)
  }), use.names = FALSE)
}


.reorder_custom_hypothesis <- function(comparison, datagrid) {
  # only proceed if we have a custom hypothesis
  if (.is_custom_comparison(comparison)) {
    # this is the row-order we use in modelbased
    datagrid$.rowid <- 1:nrow(datagrid)
    # this is the row-order in marginaleffects
    datagrid <- datawizard::data_arrange(datagrid, colnames(datagrid)[1:(length(datagrid) - 1)])
    # we need to extract all b's and the former parameter numbers
    b <- .extract_custom_comparison(comparison)
    old_b_numbers <- as.numeric(gsub("b", "", b, fixed = TRUE))
    # these are the new numbers of the b-values
    new_b_numbers <- match(old_b_numbers, datagrid$.rowid)
    new_b <- paste0("b", new_b_numbers)
    # we need to replace all occurences of "b" in comparison with "new_b".
    # however, to avoid overwriting already replaced values with "gsub()", we
    # first replace with a non-existing pattern "new_b_letters", which we will
    # replace with "new_b" in a second step
    new_b_letters <- paste0("b", letters[new_b_numbers])
    # first, numbers to letters
    for (i in seq_along(b)) {
      comparison <- gsub(b[i], new_b_letters[i], comparison, fixed = TRUE)
    }
    # next, letters to new numbers
    for (i in seq_along(b)) {
      comparison <- gsub(new_b_letters[i], new_b[i], comparison, fixed = TRUE)
    }
  }
  comparison
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
