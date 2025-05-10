.joint_test <- function(means, my_args) {
  cnames <- colnames(means)
  # we need to separate the "by" argument, to find out which variables
  # were used as contrasts, and which for grouping
  by_vars <- intersect(cnames, my_args$by)
  contrast_vars <- setdiff(my_args$by, by_vars)
  # get column names. We need to have the column "hypothesis", else,
  # no test can be performed
  if (!"hypothesis" %in% cnames) {
    insight::format_error("Can't perform joint test. Data frame needs a column \"hypothesis\".")
  }
  # check out how many comparisons we have. If only one,
  # # we jointly test all rows at once
  n_hypothesis <- insight::n_unique(means$hypothesis) * length(by_vars)

  # determine number of rows to test, and which rows
  if (n_hypothesis == 1) {
    test_rows <- as.list(1:nrow(means))
  } else {
    test_rows <- split(
      seq_len(nrow(means)),
      cut(seq_len(nrow(means)), n_hypothesis, labels = FALSE)
    )
  }

  # joint test for all test rows
  out <- lapply(test_rows, function(x) {
    marginaleffects::hypotheses(means, joint = x)
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
  colnames(result) <- c("Contrast", by_vars, "estimate", "F", "p", "df1", "df2")
  class(result) <- unique(c(class(means), "marginal_jointtest", "data.frame"))

  # these are special columns, not yet covered by "insight::format_table()"
  result$df1 <- insight::format_value(result$df1, protect_integers = TRUE)
  result$df2 <- insight::format_value(result$df2, protect_integers = TRUE)

  result
}
