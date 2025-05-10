.joint_test <- function(means, ...) {
  UseMethod(".joint_test")
}


# marginaleffects

.joint_test.predictions <- function(means, my_args, test = "f", ...) {
  cnames <- colnames(means)
  # we need to separate the "by" argument, to find out which variables
  # were used as contrasts, and which for grouping
  by_vars <- intersect(cnames, my_args$by)
  contrast_vars <- setdiff(my_args$by, by_vars)

  # if we have no grouping variable, joint test simplifies to an anova-table
  # tell user to use `anova()` then.
  if (!length(by_vars)) {
    insight::format_error("Joint tests using `comparison = \"joint\"` only work when `by` is specified. If this stratification is not desired, please use `anova()` on your model object instead.") # nolint
  }

  # get column names. We need to have the column "hypothesis", else,
  # no test can be performed
  if (!"hypothesis" %in% cnames) {
    insight::format_error("Can't perform joint test. Data frame needs a column \"hypothesis\".")
  }

  # check out how many comparisons we have. If only one,
  # # we jointly test all rows at once
  n_hypothesis <- prod(insight::n_unique(means[by_vars]))

  # determine number of rows to test, and which rows
  if (n_hypothesis == 1) {
    test_rows <- as.list(1:nrow(means))
  } else {
    test_rows <- split(
      seq_len(nrow(means)),
      cut(seq_len(nrow(means)), n_hypothesis, labels = FALSE)
    )
  }

  # sanity check
  if (is.null(test)) {
    test <- "f"
  } else {
    test <- tolower(insight::compact_character(test)[1])
  }

  # handle aliases
  test <- switch(tolower(test),
    chi2 = "chisq",
    test
  )

  # joint test for all test rows
  out <- lapply(test_rows, function(x) {
    marginaleffects::hypotheses(means, joint = x, joint_test = test)
  })

  # bind results
  result <- do.call(rbind, out)

  # add variable names and levels
  result <- cbind(
    contrast_vars,
    unique(means[by_vars]),
    estimate = NA,
    result
  )

  # proper column names
  if (test == "f") {
    colnames(result) <- c("Contrast", by_vars, "estimate", "F", "p", "df1", "df2")
    # these are special columns, not yet covered by "insight::format_table()"
    result$df1 <- insight::format_value(result$df1, protect_integers = TRUE)
    result$df2 <- insight::format_value(result$df2, protect_integers = TRUE)
  } else {
    colnames(result) <- c("Contrast", by_vars, "estimate", "Chi2", "p", "df")
  }
  class(result) <- unique(c(class(means), "marginal_jointtest", "data.frame"))

  result
}


# emmeans

.joint_test.emmGrid <- function(means, my_args, ...) {
  by_arg <- attributes(means)$misc$by.vars
  result <- try(as.data.frame(emmeans::joint_tests(means, by = by_arg)), silent = TRUE)

  # check if everything was ok
  if (inherits(result, "try-error")) {
    insight::format_error(
      "Could not compute joint test. This error occured:",
      attributes(result)$condition$message,
      "\nYou may try to set `backend = \"marginaleffects\" in your call to `estimate_contrasts()`."
    )
  }

  # these are special columns, not yet covered by "insight::format_table()"
  result$df1 <- insight::format_value(result$df1, protect_integers = TRUE)
  result$df2 <- insight::format_value(result$df2, protect_integers = TRUE)

  # rename statistic column
  colnames(result)[colnames(result) == "F.ratio"] <- "F"
  colnames(result)[1] <- "Contrast"
  result[[1]] <- my_args$contrast

  result
}
