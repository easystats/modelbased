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
  transform <- info$transform
  model_info <- info$model_info
  # make sure we definitely have model information
  if (is.null(model_info) && !is.null(model)) {
    model_info <- insight::model_info(model, response = 1)
  }


  # name of predicted response -----------------------------------------------

  table_footer <- paste0("\nVariable predicted: ", toString(insight::find_response(model)))


  # modulated predictors (focal terms) ---------------------------------------

  if (!is.null(by)) {
    modulate_string <- switch(type,
      contrasts = "contrasted",
      "modulated"
    )
    table_footer <- paste0(table_footer, "\nPredictors ", modulate_string, ": ", toString(by))
  }


  # predictors controlled (non-focal terms) ----------------------------------
  if (!is.null(adjusted_for) && length(adjusted_for) >= 1 && !all(is.na(adjusted_for))) {
    # if we have values of adjusted terms, add these here
    if (all(adjusted_for %in% colnames(x))) {
      ref_cat_data <- x
    } else if (all(adjusted_for %in% colnames(datagrid))) {
      ref_cat_data <- datagrid
    } else {
      ref_cat_data <- NULL
    }
    if (!is.null(ref_cat_data)) {
      # get values at which non-focal terms are hold constant
      adjusted_values <- lapply(adjusted_for, function(i) ref_cat_data[[i]][1])
      # at values to names of non-focal terms (table_footer). we have to iterate
      # over the list, because we may have different types of data
      for (av in seq_along(adjusted_values)) {
        if (is.numeric(adjusted_values[[av]])) {
          adjusted_for[av] <- sprintf("%s (%.2g)", adjusted_for[av], adjusted_values[[av]])
        } else if (identical(type, "predictions")) {
          adjusted_for[av] <- sprintf("%s (%s)", adjusted_for[av], adjusted_values[[av]])
        }
      }
    }
    average_string <- switch(type,
      predictions = "controlled",
      "averaged"
    )
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

  result_type <- switch(type,
    contrasts = "Contrasts",
    "Predictions"
  )

  if (!is.null(predict) && isFALSE(model_info$is_linear)) {
    # exceptions
    predict <- switch(predict,
      none = "link",
      prediction = ,
      expectation = ,
      `invlink(link)` = "response",
      predict
    )
    table_footer <- paste0(
      table_footer,
      "\n",
      result_type,
      " are on the ",
      predict,
      "-scale",
      .contrast_units(type, predict, model_info),
      "."
    )
  } else if (isTRUE(model_info$is_linear) && !isTRUE(transform)) {
    # add information about response transformation
    trans_fun <- .safe(insight::find_transformation(model))
    if (!is.null(trans_fun) && all(trans_fun != "identity")) {
      table_footer <- paste0(
        table_footer,
        "\n",
        result_type,
        " are on the ",
        trans_fun,
        "-scale (consider `transform=TRUE`)."
      )
    }
  }


  # Parameter labels for special hypothesis testing --------------------------

  # for special hypothesis testing, like "(b1 - b2) = (b4 - b3)", we want to
  # add information about the parameter names
  if (.is_custom_comparison(comparison)) {
    # extract all "b" strings, so we have a vector of all "b" used in the comparison
    parameter_names <- .extract_custom_comparison(comparison)

    # datagrid contains all parameters, so we just need to find out the rows
    # and combine column names with row values
    if (!is.null(datagrid)) {
      # extract unique values
      custom_grid <- data.frame(expand.grid(
        lapply(datagrid[info$focal_terms], unique)
      ))
      # transpose, so we can easier extract information
      transposed_dg <- t(custom_grid[info$focal_terms])
      # interate over all parameters and create labels with proper names
      hypothesis_labels <- unlist(lapply(parameter_names, function(i) {
        rows <- as.numeric(sub(".", "", i))
        paste0(i, " = ", toString(paste0(info$focal_terms, " [", transposed_dg[, rows], "]")))
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

  c(paste0(table_footer, "\n"), "yellow")
}


.contrast_units <- function(type, predict, info) {
  # estimate name
  if (is.null(predict) || is.null(info) || type != "contrasts") {
    return(NULL)
  }

  if (
    (!predict %in% c("none", "link") && (info$is_binomial || info$is_bernoulli)) ||
      predict %in% c("zprob", "zero") ||
      (predict %in% c("response", "invlink(link)") && (info$is_beta || info$is_orderedbeta))
  ) {
    " (in %-points)"
  } else {
    NULL
  }
}


# Table footer slopes =========================================================


.table_footer_slopes <- function(x, model = NULL, info = NULL) {
  model_info <- info$model_info
  # make sure we definitely have model information
  if (is.null(model_info) && !is.null(model)) {
    model_info <- insight::model_info(model, response = 1)
  }
  transform <- info$transform

  table_footer <- paste("\nMarginal effects estimated for", info$trend)
  if (!is.null(attributes(x)$slope)) {
    table_footer <- paste0(table_footer, "\nType of slope was ", attributes(x)$slope)
  }
  if (isTRUE(model_info$is_linear) && !isTRUE(transform)) {
    # add information about response transformation
    trans_fun <- .safe(insight::find_transformation(model))
    if (!is.null(trans_fun) && all(trans_fun != "identity")) {
      table_footer <- paste0(table_footer, "\nSlopes are on the ", trans_fun, "-scale (consider `transform=TRUE`).")
    }
  }
  table_footer
}
