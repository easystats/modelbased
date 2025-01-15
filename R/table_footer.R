# Table footer ===============================================================


.table_footer <- function(x,
                          by = NULL,
                          type = "means",
                          model = NULL,
                          info = NULL) {
  # extract necessary information from attributes
  predict <- info$predict
  comparison <- info$comparison
  datagrid <- info$datagrid
  p_adjust <- info$p_adjust
  adjusted_for <- info$adjusted_for
  model_info <- insight::model_info(model)


  # name of predicted response -----------------------------------------------

  table_footer <- paste0("\nVariable predicted: ", insight::find_response(model))


  # modulated predictors (focal terms) ---------------------------------------

  if (!is.null(by)) {
    modulate_string <- switch(type, contrasts = "contrasted", "modulated")
    table_footer <- paste0(table_footer, "\nPredictors ", modulate_string, ": ", toString(by))
  }


  # predictors controlled (non-focal terms) ----------------------------------

  if (!is.null(adjusted_for) && length(adjusted_for) >= 1 && !all(is.na(adjusted_for))) {
    # if we have values of adjusted terms, add these here
    if (all(adjusted_for %in% colnames(x))) {
      # get values at which non-focal terms are hold constant
      adjusted_values <- sapply(adjusted_for, function(i) {
        x[[i]][1]
      })
      # at values to names of non-focal terms (table_footer)
      if (is.numeric(adjusted_values)) {
        adjusted_for <- sprintf("%s (%.2g)", adjusted_for, adjusted_values)
      } else {
        adjusted_for <- sprintf("%s (%s)", adjusted_for, adjusted_values)
      }
    }
    average_string <- switch(type, predictions = "controlled", "averaged")
    table_footer <- paste0(table_footer, "\nPredictors ", average_string, ": ", toString(adjusted_for))
  }


  # P-value adjustment footer ------------------------------------------------

  if (!is.null(p_adjust) && "p" %in% names(x)) {
    if (p_adjust == "none") {
      table_footer <- paste0(table_footer, "\np-values are uncorrected.")
    } else {
      table_footer <- paste0(table_footer, "\np-value adjustment method: ", parameters::format_p_adjust(p_adjust))
    }
  }


  # tell user about scale of predictions / contrasts -------------------------

  if (!is.null(predict) && isFALSE(model_info$is_linear)) {
    result_type <- switch(type,
      contrasts = "Contrasts",
      "Predictions"
    )
    # exceptions
    predict <- switch(predict,
      none = "link",
      prediction = ,
      expectations = ,
      `invlink(link)` = "response",
      predict
    )
    table_footer <- paste0(table_footer, "\n", result_type, " are on the ", predict, "-scale.")
  }


  # Parameter labels for special hypothesis testing --------------------------

  # for special hypothesis testing, like "(b1 - b2) = (b4 - b3)", we want to
  # add information about the parameter names
  if (!is.null(comparison) && is.character(comparison) && grepl("=", comparison, fixed = TRUE) && grepl("\\bb\\d+\\b", comparison)) { # nolint
    # find all "b" strings
    matches <- gregexpr("\\bb\\d+\\b", comparison)[[1]]
    match_lengths <- attr(matches, "match.length")

    # extract all "b" strings, so we have a vector of all "b" used in the comparison
    parameter_names <- unlist(lapply(seq_along(matches), function(i) {
      substr(comparison, matches[i], matches[i] + match_lengths[i] - 1)
    }), use.names = FALSE)

    # datagrid contains all parameters, so we just need to find out the rows
    # and combine column names with row values
    if (!is.null(datagrid)) {
      # transpose, so we can easier extract information
      transposed_dg <- t(datagrid)
      # interate over all parameters and create labels with proper names
      hypothesis_labels <- unlist(lapply(parameter_names, function(i) {
        rows <- as.numeric(sub(".", "", i))
        paste0(i, " = ", toString(paste0(colnames(datagrid), " [", transposed_dg[, rows], "]")))
      }), use.names = FALSE)
      # add all names to the footer
      table_footer <- paste0(
        table_footer,
        "\n",
        paste0("Parameters:\n", paste(unlist(hypothesis_labels), collapse = "\n"))
      )
    }
  }

  if (all(table_footer == "")) { # nolint
    return(NULL)
  }

  c(paste0(table_footer, "\n"), "blue")
}
