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
  comparison <- insight::validate_argument(comparison, c("pairwise", "interaction"))

  # the "model" object is an object of class "estimate_predicted", we want
  # to copy that into a separate object, for clearer names
  predictions <- object <- model
  model <- attributes(object)$model
  datagrid <- attributes(object)$datagrid

  # some attributes we need
  focal_terms <- attributes(object)$focal_terms

  # vcov matrix, for adjusting se
  vcov_matrix <- .safe(stats::vcov(model, verbose = FALSE, ...))

  minfo <- insight::model_info(object)

  # model df
  dof <- insight::get_df(model, type = "wald", verbose = FALSE)
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
  at_list <- lapply(datagrid, unique)
  # compute contrasts or comparisons
  out <- switch(comparison,
    pairwise = .compute_comparisons(predictions, dof, vcov_matrix, at_list, focal_terms, crit_factor),
    interaction = .compute_interactions(predictions, dof, vcov_matrix, at_list, focal_terms, crit_factor)
  )

  # p-value adjustment?
  if (!is.null(p_adjust)) {
    out <- .p_adjust(model, predictions, p_adjust, verbose, ...)
  }

  out <- format(estimated, model, p_adjust, comparison, ...)

  # restore attributes later
  info <- attributes(object)

  # Table formatting
  attr(out, "table_title") <- c("Model-based Contrasts Analysis", "blue")
  attr(out, "table_footer") <- .table_footer(
    out,
    by = info$contrast,
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
.compute_comparisons <- function(predictions, dof, vcov_matrix, at_list, focal_terms, crit_factor) {
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
    datawizard::data_rotate(as.data.frame(pair))
  })
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
    # once we have found the correct rows for the pairs, we can calculate
    # the contrast. We need the predicted values first
    predicted1 <- predictions$predicted[pos1]
    predicted2 <- predictions$predicted[pos2]
    # we then create labels for the pairs. "result" is a data frame with
    # the labels (of the pairwise contrasts) as columns.
    result <- as.data.frame(do.call(cbind, lapply(seq_along(focal_terms), function(j) {
      paste(pairs_data[[1]][i, j], pairs_data[[2]][i, j], sep = "-")
    })))
    colnames(result) <- focal_terms
    # we then add the contrast and the standard error. for linear models, the
    # SE is sqrt(se1^2 + se2^2).
    result$Contrast <- predicted1 - predicted2
    # sum of squared standard errors
    sum_se_squared <- predictions$std.error[pos1]^2 + predictions$std.error[pos2]^2
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
      result$std.error <- sqrt(sum_se_squared)
    } else {
      result$std.error <- sqrt(sum_se_squared - vcov_sub)
    }
    result
  }))
  # add CI and p-values
  out$CI_low <- out$Contrast - stats::qt(crit_factor, df = dof) * out$std.error
  out$CI_high <- out$Contrast + stats::qt(crit_factor, df = dof) * out$std.error
  out$Statistic <- out$Contrast / out$std.error
  out$p <- 2 * stats::pt(abs(out$statistic), df = dof, lower.tail = FALSE)
  out
}


# interaction contrasts  ----------------------------------------------------
.compute_interactions <- function(predictions, dof, vcov_matrix, at_list, focal_terms, crit_factor) {
  ## TODO: interaction contrasts currently only work for two focal terms
  if (length(focal_terms) != 2) {
    msg <- "Interaction contrasts currently only work for two focal terms."
    if (length(focal_terms) > 2) {
      cleaned_f3 <- .clean_terms(focal_terms[3])
      s1 <- "pr <- predict_response(\n    model,"
      s2 <- paste0("terms = c(", datawizard::text_concatenate(focal_terms[1:2], enclose = "\"", sep = ", ", last = ", "), "),") # nolint
      s3 <- paste0("condition = c(", cleaned_f3, " = \"", at_list[[cleaned_f3]][1], "\")")
      s4 <- ")"
      msg <- c(
        msg,
        "You can try to fix remaining focal terms to specific values, using the `condition` argument, e.g.:.",
        paste0("\n  ", insight::color_text(s1, "cyan")),
        paste0("  ", insight::color_text(s2, "cyan")),
        paste0("  ", insight::color_text(s3, "cyan")),
        paste0(insight::color_text(s4, "cyan")),
        insight::color_text("test_predictions(pr, engine = \"ggeffects\")", "cyan")
      )
    }
    insight::format_error(msg)
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
      predicted1 <- predictions$predicted[pos_1a] - predictions$predicted[pos_1b]
      predicted2 <- predictions$predicted[pos_2a] - predictions$predicted[pos_2b]
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
      result$Contrast <- predicted1 - predicted2
      sum_se_squared <- sum(
        predictions$std.error[pos_1a]^2, predictions$std.error[pos_1b]^2,
        predictions$std.error[pos_2a]^2, predictions$std.error[pos_2b]^2
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
        result$std.error <- sqrt(sum_se_squared)
      } else {
        result$std.error <- sqrt(sum_se_squared - vcov_sub)
      }
      result
    }))
  }))
  # add CI and p-values
  out$CI_low <- out$Contrast - stats::qt(crit_factor, df = dof) * out$std.error
  out$CI_high <- out$Contrast + stats::qt(crit_factor, df = dof) * out$std.error
  out$Statistic <- out$Contrast / out$std.error
  out$p <- 2 * stats::pt(abs(out$statistic), df = dof, lower.tail = FALSE)
  out
}
