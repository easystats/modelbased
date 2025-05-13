#' @export
estimate_contrasts.estimate_predicted <- function(model,
                                                  contrast = NULL,
                                                  by = NULL,
                                                  predict = "response",
                                                  ci = 0.95,
                                                  p_adjust = "none",
                                                  comparison = "pairwise",
                                                  verbose = TRUE,
                                                  ...) {
  # sanity check
  if (inherits(comparison, "formula")) {
    comparison <- all.vars(comparison)[1]
  }
  comparison <- insight::validate_argument(comparison, c("pairwise", "interaction"))

  # sanity check
  if (is.null(contrast)) {
    contrast <- attributes(model)$focal_terms
  }

  # the "model" object is an object of class "estimate_predicted", we want
  # to copy that into a separate object, for clearer names
  predictions <- object <- model
  model <- attributes(object)$model
  datagrid <- attributes(object)$datagrid

  # vcov matrix, for adjusting se
  vcov_matrix <- .safe(stats::vcov(model, verbose = FALSE, ...))

  minfo <- insight::model_info(model, response = 1)

  # model df
  if (minfo$is_bayesian) {
    dof <- Inf
  } else {
    dof <- insight::get_df(model, type = "wald", verbose = FALSE)
  }
  crit_factor <- (1 + ci) / 2

  ## TODO: For Bayesian models, we always use the returned standard errors
  # need to check whether scale is always correct

  # for non-Gaussian models, we need to adjust the standard errors
  if (!minfo$is_linear && !minfo$is_bayesian) {
    se_from_predictions <- tryCatch(
      {
        # arguments for predict(), to get SE on response scale for non-Gaussian models
        my_args <- list(
          model,
          newdata = datagrid,
          type = predict,
          se.fit = TRUE
        )
        # for mixed models, need to set re.form to NULL or NA
        if (insight::is_mixed_model(model)) {
          my_args$re.form <- NULL
        }
        do.call(stats::predict, my_args)
      },
      error = function(e) {
        e
      }
    )
    # check if everything worked as expected
    if (inherits(se_from_predictions, "error")) {
      insight::format_error(
        "This model (family) is probably not supported. The error that occured was:",
        se_from_predictions$message
      )
    }
    # check if we have standard errors
    if (is.null(se_from_predictions$se.fit)) {
      insight::format_error("Could not extract standard errors from predictions.")
    }
    preds_with_se <- merge(
      predictions,
      cbind(datagrid, se_prob = se_from_predictions$se.fit),
      sort = FALSE,
      all = TRUE
    )
    predictions$SE <- preds_with_se$se_prob
  } else {
    # for linear models, we don't need adjustment of standard errors
    vcov_matrix <- NULL
  }

  # compute contrasts or comparisons
  out <- switch(comparison,
    pairwise = .compute_comparisons(predictions, dof, vcov_matrix, datagrid, contrast, by, crit_factor),
    interaction = .compute_interactions(predictions, dof, vcov_matrix, datagrid, contrast, by, crit_factor)
  )

  # restore attributes, for formatting
  info <- attributes(object)
  attributes(out) <- utils::modifyList(attributes(out), info[.info_elements()])

  # overwrite some of the attributes
  attr(out, "contrast") <- contrast
  attr(out, "focal_terms") <- c(contrast, by)
  attr(out, "by") <- by

  # format output
  out <- format.marginaleffects_contrasts(out, model, p_adjust, comparison, ...)

  # clean levels, remove levels from by-terms, which are already present as columns
  if (!is.null(by) && all(by %in% colnames(out))) {
    for (i in by) {
      to_remove <- unique(out[[i]])
      for (j in to_remove) {
        if (all(c("Level1", "Level2") %in% colnames(out))) {
          levels(out$Level1) <- insight::trim_ws(gsub(j, "", levels(out$Level1), fixed = TRUE))
          levels(out$Level2) <- insight::trim_ws(gsub(j, "", levels(out$Level2), fixed = TRUE))
        } else if ("Parameter" %in% colnames(out)) {
          out$Parameter <- insight::trim_ws(gsub(j, "", out$Parameter, fixed = TRUE))
        }
      }
    }
  }

  # p-value adjustment?
  if (!is.null(p_adjust)) {
    out <- .p_adjust(model, out, p_adjust, verbose, ...)
  }

  # Table formatting
  attr(out, "table_title") <- c("Model-based Contrasts Analysis", "blue")
  attr(out, "table_footer") <- .table_footer(
    out,
    by = contrast,
    type = "contrasts",
    model = model,
    info = info
  )

  # Add attributes
  attr(out, "model") <- model
  attr(out, "response") <- insight::find_response(model)
  attr(out, "ci") <- ci
  attr(out, "p_adjust") <- p_adjust

  # add attributes from workhorse function
  attributes(out) <- utils::modifyList(attributes(out), info[.info_elements()])

  # Output
  class(out) <- unique(c("estimate_contrasts", "see_estimate_contrasts", class(out)))
  out
}


# pairwise comparisons ----------------------------------------------------
.compute_comparisons <- function(predictions, dof, vcov_matrix, datagrid, contrast, by, crit_factor) {
  # we need the focal terms and all unique values from the datagrid
  focal_terms <- unique(c(contrast, by))

  # sanity check - user-defined by-variables may not be in the data
  if (!all(focal_terms %in% colnames(predictions))) {
    not_found <- setdiff(focal_terms, colnames(predictions))
    insight::format_error(paste0(
      "Following variables were not found in the data: ",
      paste0("`", toString(not_found), "`"),
      ". Please check the spelling."
    ))
  }

  # create at-list, i.e. all representative values for the focal terms
  at_list <- lapply(datagrid[focal_terms], unique)

  # pairwise comparisons are a bit more complicated, as we need to create
  # pairwise combinations of the levels of the focal terms.

  # since we split at "." later, we need to replace "." in all levels
  # with a unique character combination
  at_list <- lapply(at_list, function(i) {
    gsub(".", "#_#", as.character(i), fixed = TRUE)
  })
  # create pairwise combinations
  level_pairs <- interaction(expand.grid(at_list))
  # using the matrix and then removing the lower triangle, we get all
  # pairwise combinations, except the ones that are the same
  M <- matrix(
    1,
    nrow = length(level_pairs),
    ncol = length(level_pairs),
    dimnames = list(level_pairs, level_pairs)
  )
  M[!upper.tri(M)] <- NA
  # table() works fine to create variables of this pairwise combinations
  pairs_data <- stats::na.omit(as.data.frame(as.table(M)))
  pairs_data$Freq <- NULL
  pairs_data <- lapply(pairs_data, as.character)
  # the levels are combined by ".", we need to split them and then create
  # a list of data frames, where each data frames contains the levels of
  # the focal terms as variables
  pairs_data <- lapply(pairs_data, function(i) {
    # split at ".", which is the separator char for levels
    pair <- strsplit(i, ".", fixed = TRUE)
    # since we replaced "." with "#_#" in original levels,
    # we need to replace it back here
    pair <- lapply(pair, gsub, pattern = "#_#", replacement = ".", fixed = TRUE)
    pdata <- datawizard::data_rotate(as.data.frame(pair))
    rownames(pdata) <- NULL
    pdata
  })

  # if we want to group by, we reduce the data frames. we can remove all
  # those rows where the by-terms in pairs_data 1 and 2 don't have identical
  # values
  if (!is.null(by)) {
    # check which variables are the by-variables. column names for pairs_data
    # are numbers, not the variable names
    by_pos <- match(by, focal_terms)
    # iterate all rows, check for identical pairs in by, and only keep those
    keep_rows <- vapply(
      seq_len(nrow(pairs_data[[1]])),
      function(i) {
        identical(pairs_data[[1]][i, by_pos], pairs_data[[2]][i, by_pos])
      },
      logical(1),
      USE.NAMES = FALSE
    )
    pairs_data[[1]] <- pairs_data[[1]][keep_rows, ]
    pairs_data[[2]] <- pairs_data[[2]][keep_rows, ]
  }


  # now we iterate over all pairs and try to find the corresponding predictions
  out <- do.call(rbind, lapply(seq_len(nrow(pairs_data[[1]])), function(i) {
    pos1 <- predictions[[focal_terms[1]]] == pairs_data[[1]][i, 1]
    pos2 <- predictions[[focal_terms[1]]] == pairs_data[[2]][i, 1]

    if (length(focal_terms) > 1) {
      pos1 <- pos1 & predictions[[focal_terms[2]]] == pairs_data[[1]][i, 2]
      pos2 <- pos2 & predictions[[focal_terms[2]]] == pairs_data[[2]][i, 2]
    }
    if (length(focal_terms) > 2) {
      pos1 <- pos1 & predictions[[focal_terms[3]]] == pairs_data[[1]][i, 3]
      pos2 <- pos2 & predictions[[focal_terms[3]]] == pairs_data[[2]][i, 3]
    }
    if (length(focal_terms) > 3) {
      pos1 <- pos1 & predictions[[focal_terms[4]]] == pairs_data[[1]][i, 4]
      pos2 <- pos2 & predictions[[focal_terms[4]]] == pairs_data[[2]][i, 4]
    }
    # once we have found the correct rows for the pairs, we can calculate
    # the contrast. We need the predicted values first
    predicted1 <- predictions$Predicted[pos1]
    predicted2 <- predictions$Predicted[pos2]

    # we then create labels for the pairs. "result" is a data frame with
    # the labels (of the pairwise contrasts) as columns.
    result <- data.frame(
      Parameter = paste(
        paste0("(", paste(pairs_data[[1]][i, ], collapse = " "), ")"),
        paste0("(", paste(pairs_data[[2]][i, ], collapse = " "), ")"),
        sep = "-"
      ),
      stringsAsFactors = FALSE
    )
    # we then add the contrast and the standard error. for linear models, the
    # SE is sqrt(se1^2 + se2^2).
    result$Difference <- predicted1 - predicted2
    # sum of squared standard errors
    sum_se_squared <- predictions$SE[pos1]^2 + predictions$SE[pos2]^2
    # for non-Gaussian models, we subtract the covariance of the two predictions
    # but only if the vcov_matrix is not NULL and has the correct dimensions
    correct_row_dims <- nrow(vcov_matrix) > 0 && all(nrow(vcov_matrix) >= which(pos1))
    correct_col_dims <- ncol(vcov_matrix) > 0 && all(ncol(vcov_matrix) >= which(pos2))
    if (is.null(vcov_matrix) || !correct_row_dims || !correct_col_dims) {
      vcov_sub <- 0
    } else {
      vcov_sub <- vcov_matrix[which(pos1), which(pos2)]^2
    }
    # Avoid negative values in sqrt()
    if (vcov_sub >= sum_se_squared) {
      result$SE <- sqrt(sum_se_squared)
    } else {
      result$SE <- sqrt(sum_se_squared - vcov_sub)
    }
    result
  }))
  # add CI and p-values
  out$CI_low <- out$Difference - stats::qt(crit_factor, df = dof) * out$SE
  out$CI_high <- out$Difference + stats::qt(crit_factor, df = dof) * out$SE
  out$Statistic <- out$Difference / out$SE
  out$p <- 2 * stats::pt(abs(out$Statistic), df = dof, lower.tail = FALSE)

  # filter by "by"
  if (!is.null(by)) {
    idx <- rep_len(TRUE, nrow(out))
    for (filter_by in by) {
      # create index with "by" variables for each comparison pair
      filter_data <- do.call(cbind, lapply(pairs_data, function(i) {
        colnames(i) <- focal_terms
        i[filter_by]
      }))
      # check which pairs have identical values - these are the rows we want to keep
      idx <- idx & unname(apply(filter_data, 1, function(r) r[1] == r[2]))
    }
    # prepare filtered dataset
    filter_column <- pairs_data[[1]]
    colnames(filter_column) <- focal_terms
    # bind the filtered data to the output
    out <- cbind(filter_column[idx, by, drop = FALSE], out[idx, , drop = FALSE])
  }

  rownames(out) <- NULL
  out
}


# interaction contrasts  ----------------------------------------------------
.compute_interactions <- function(predictions, dof, vcov_matrix, datagrid, contrast, by, crit_factor) {
  # we need the focal terms and all unique values from the datagrid
  focal_terms <- c(contrast, by)
  at_list <- lapply(datagrid[focal_terms], unique)

  ## TODO: interaction contrasts currently only work for two focal terms
  if (length(focal_terms) != 2) {
    insight::format_error("Interaction contrasts currently only work for two focal terms.")
  }

  # create pairwise combinations of first focal term
  level_pairs <- at_list[[1]]
  M <- matrix(
    1,
    nrow = length(level_pairs),
    ncol = length(level_pairs),
    dimnames = list(level_pairs, level_pairs)
  )
  M[!upper.tri(M)] <- NA
  # table() works fine to create variables of this pairwise combinations
  pairs_focal1 <- stats::na.omit(as.data.frame(as.table(M)))
  pairs_focal1$Freq <- NULL

  # create pairwise combinations of second focal term
  level_pairs <- at_list[[2]]
  M <- matrix(
    1,
    nrow = length(level_pairs),
    ncol = length(level_pairs),
    dimnames = list(level_pairs, level_pairs)
  )
  M[!upper.tri(M)] <- NA
  # table() works fine to create variables of this pairwise combinations
  pairs_focal2 <- stats::na.omit(as.data.frame(as.table(M)))
  pairs_focal2$Freq <- NULL

  # now we iterate over all pairs and try to find the corresponding predictions
  out <- do.call(rbind, lapply(seq_len(nrow(pairs_focal1)), function(i) {
    # differences between levels of first focal term
    pos1 <- predictions[[focal_terms[1]]] == pairs_focal1[i, 1]
    pos2 <- predictions[[focal_terms[1]]] == pairs_focal1[i, 2]

    do.call(rbind, lapply(seq_len(nrow(pairs_focal2)), function(j) {
      # difference between levels of first focal term, *within* first
      # level of second focal term
      pos_1a <- pos1 & predictions[[focal_terms[2]]] == pairs_focal2[j, 1]
      pos_1b <- pos2 & predictions[[focal_terms[2]]] == pairs_focal2[j, 1]
      # difference between levels of first focal term, *within* second
      # level of second focal term
      pos_2a <- pos1 & predictions[[focal_terms[2]]] == pairs_focal2[j, 2]
      pos_2b <- pos2 & predictions[[focal_terms[2]]] == pairs_focal2[j, 2]
      # once we have found the correct rows for the pairs, we can calculate
      # the contrast. We need the predicted values first
      predicted1 <- predictions$Predicted[pos_1a] - predictions$Predicted[pos_1b]
      predicted2 <- predictions$Predicted[pos_2a] - predictions$Predicted[pos_2b]
      # we then create labels for the pairs. "result" is a data frame with
      # the labels (of the pairwise contrasts) as columns.
      result <- data.frame(
        a = paste(pairs_focal1[i, 1], pairs_focal1[i, 2], sep = "-"),
        b = paste(pairs_focal2[j, 1], pairs_focal2[j, 2], sep = " and "),
        stringsAsFactors = FALSE
      )
      colnames(result) <- focal_terms
      # we then add the contrast and the standard error. for linear models, the
      # SE is sqrt(se1^2 + se2^2)
      result$Difference <- predicted1 - predicted2
      sum_se_squared <- sum(
        predictions$SE[pos_1a]^2, predictions$SE[pos_1b]^2,
        predictions$SE[pos_2a]^2, predictions$SE[pos_2b]^2
      )
      # for non-Gaussian models, we subtract the covariance of the two predictions
      # but only if the vcov_matrix is not NULL and has the correct dimensions
      correct_row_dims <- nrow(vcov_matrix) > 0 && all(nrow(vcov_matrix) >= which(pos_1a)) && all(nrow(vcov_matrix) >= which(pos_2a)) # nolint
      correct_col_dims <- ncol(vcov_matrix) > 0 && all(ncol(vcov_matrix) >= which(pos_1b)) && all(ncol(vcov_matrix) >= which(pos_2b)) # nolint
      if (is.null(vcov_matrix) || !correct_row_dims || !correct_col_dims) {
        vcov_sub <- 0
      } else {
        vcov_sub <- sum(
          vcov_matrix[which(pos_1a), which(pos_1b)]^2,
          vcov_matrix[which(pos_2a), which(pos_2b)]^2
        )
      }
      # Avoid negative values in sqrt()
      if (vcov_sub >= sum_se_squared) {
        result$SE <- sqrt(sum_se_squared)
      } else {
        result$SE <- sqrt(sum_se_squared - vcov_sub)
      }
      result
    }))
  }))
  # add CI and p-values
  out$CI_low <- out$Difference - stats::qt(crit_factor, df = dof) * out$SE
  out$CI_high <- out$Difference + stats::qt(crit_factor, df = dof) * out$SE
  out$Statistic <- out$Difference / out$SE
  out$p <- 2 * stats::pt(abs(out$Statistic), df = dof, lower.tail = FALSE)
  out
}
