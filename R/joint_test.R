# marginaleffects::hypotheses(means, joint = 1:2)
# resulting data frame in "means" is:
# facetype              Hypothesis Estimate Std. Error      t Pr(>|t|)    S
# Unattractive (Low dose) - (Placebo)     1.375      0.585  2.350   0.0237  5.4
# Unattractive (High dose) - (Placebo)    3.125      0.585  5.342   <0.001 18.0
# Attractive   (Low dose) - (Placebo)     0.125      0.585  0.214   0.8319  0.3
# Attractive   (High dose) - (Placebo)   -0.250      0.585 -0.427   0.6714  0.6

# my_args$by contains both arguments `contrast` and `by`, so we must find
# names of contrast-variables by removing colname "facetype" from my_args$by,
# and remaining values are the names for the "Hypothesis" column

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
  n_hypothesis <- insight::n_unique(means$hypothesis)

  # determine number of rows to test, and which rows
  if (n_hypothesis == 1) {
    test_rows <- list(1:nrow(means))
  } else {
    test_rows <- split(
      seq_len(nrow(means)),
      cut(seq_len(nrow(means)), n_hypothesis, labels = FALSE)
    )
  }

  out <- lapply(test_rows, function(x) {
    marginaleffects::hypotheses(means, joint = x)
  })

  res <- do.call(rbind, out)
  res
}
