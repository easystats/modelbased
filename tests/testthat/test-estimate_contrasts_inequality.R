skip_on_cran()
skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects")
skip_on_os("mac")
skip_if(getRversion() < "4.5.0")
skip_if_not_installed("datawizard")


test_that("estimate_contrast, marginal effects inequalities", {
  data(penguins)
  penguins$long_bill <- factor(
    datawizard::categorize(penguins$bill_len),
    labels = c("short", "long")
  )

  m <- glm(long_bill ~ species + island + bill_dep, data = penguins, family = "binomial")

  out <- estimate_contrasts(m, "species", comparison = "inequality", estimate = "average")
  expect_equal(out$Mean_Difference, 0.6380998, tolerance = 1e-4)
  expect_identical(attributes(out)$table_title, c("Averaged Inequality Analysis", "blue"))
  out <- estimate_contrasts(m, "species", comparison = "inequality")
  expect_equal(out$Mean_Difference, 0.5580707, tolerance = 1e-4)
  expect_identical(attributes(out)$table_title, c("Marginal Inequality Analysis", "blue"))

  expect_error(
    estimate_contrasts(m, "species", comparison = "inequality_pairwise"),
    regex = "Pairwise comparisons require"
  )

  out <- estimate_contrasts(m, c("species", "island"), comparison = "inequality", estimate = "average")
  expect_equal(out$Mean_Difference, c(0.23043, 0.6381), tolerance = 1e-4)
  expect_identical(out$Parameter, c("island", "species"))
  out <- estimate_contrasts(m, c("species", "island"), comparison = "inequality")
  expect_equal(out$Mean_Difference, c(0.299254, 0.558071), tolerance = 1e-4)

  out <- estimate_contrasts(m, "species", by = "island", comparison = "inequality", estimate = "average")
  expect_equal(out$Mean_Difference, c(0.66259, 0.60411, 0.64052), tolerance = 1e-4)
  expect_named(out, c("island", "Mean_Difference", "SE", "CI_low", "CI_high", "z", "p"))
  out <- estimate_contrasts(m, "species", by = "island", comparison = "inequality")
  expect_equal(out$Mean_Difference, c(0.665814, 0.538415, 0.665679), tolerance = 1e-4)

  out <- estimate_contrasts(m, c("species", "island"), comparison = "inequality_pairwise", estimate = "average")
  expect_equal(out$Mean_Difference, -0.4076682, tolerance = 1e-4, ignore_attr = TRUE)
  expect_identical(out$Parameter, "island - species")
  out <- estimate_contrasts(m, c("species", "island"), comparison = "inequality_pairwise")
  expect_equal(out$Mean_Difference, -0.2588168, tolerance = 1e-4, ignore_attr = TRUE)

  out <- estimate_contrasts(m, "species", by = "island", comparison = "inequality_pairwise", estimate = "average")
  expect_equal(
    out$Mean_Difference,
    c(0.05848, 0.02207, -0.03641),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_named(out, c("Parameter", "Mean_Difference", "SE", "CI_low", "CI_high", "z", "p"))
  expect_identical(out$Parameter, c("Biscoe - Dream", "Biscoe - Torgersen", "Dream - Torgersen"))
  out <- estimate_contrasts(m, "species", by = "island", comparison = "inequality_pairwise")
  expect_equal(
    out$Mean_Difference,
    c(0.127399, 0.000135, -0.127264),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  expect_error(
    estimate_contrasts(m, c("species", "bill_dep"), comparison = "inequality"),
    regex = "All variables specified"
  )

  m <- glm(long_bill ~ species * sex * island + bill_dep, data = penguins, family = "binomial")
  out <- suppressWarnings(estimate_contrasts(
    m, "species", by = c("island", "sex"), comparison = ~inequality | sex
  ))
  expect_equal(
    out$Mean_Difference,
    c(0.6537358, 0.6634438),
    tolerance = 1e-4
  )
  out <- suppressWarnings(estimate_contrasts(
    m, "species", by = c("island", "sex"), comparison = "inequality"
  ))
  expect_equal(
    out$Mean_Difference,
    c(0.65074, 0.66246, 0.655004, 0.666667, 0.655463, 0.661205),
    tolerance = 1e-4
  )
  expect_error(
    estimate_contrasts(m, "species", by = c("island", "sex", "bill_dep"), comparison = "inequality"),
    regex = "`by` can only contain",
    fixed = TRUE
  )

  m <- glm(long_bill ~ sex + species + island * bill_dep, data = penguins, family = "binomial")
  out <- estimate_contrasts(
    m,
    "bill_dep",
    by = "island",
    estimate = "average",
    comparison = "inequality"
  )
  expect_equal(out$Mean_Difference, 0.02291537, tolerance = 1e-4)
  expect_identical(out$Parameter, "island")

  # same as:
  # out <- avg_slopes(m, variables = "bill_dep", by = "island", hypothesis = ~pairwise)
  # hypotheses(out, hypothesis = ~I(mean(abs(x))))

  out <- estimate_contrasts(
    m,
    "bill_dep",
    by = "island",
    comparison = "inequality"
  )
  expect_equal(out$Mean_Difference, 0.02443619, tolerance = 1e-4)
  expect_identical(out$Parameter, "island")

  expect_error(
    estimate_contrasts(m, "bill_dep", comparison = "inequality"),
    regex = "`by` argument must be specified",
    fixed = TRUE
  )
})


test_that("estimate_contrast, inequality ratios", {
  data(penguins)
  m <- lm(bill_len ~ species * bill_dep + island, data = penguins)

  out <- estimate_contrasts(m, "bill_dep", by = "species", comparison = ratio ~ pairwise)
  expect_equal(out$Ratio, c(2.262453, 2.378612, 1.051342), tolerance = 1e-4, ignore_attr = TRUE)

  out <- estimate_contrasts(m, "bill_dep", by = "species", comparison = ratio ~ inequality)
  expect_equal(out$Mean_Ratio, 1.897469, tolerance = 1e-4, ignore_attr = TRUE)
  expect_identical(out$Parameter, "species")

  m <- lm(bill_len ~ island * sex + bill_dep + species, data = penguins)

  out <- estimate_contrasts(m, "island", by = "sex", comparison = ratio ~ pairwise)
  expect_equal(
    out$Ratio,
    c(0.988576, 1.011352, 1.023039, 0.992977, 0.991165, 0.998175),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  out <- estimate_contrasts(m, "island", by = "sex", comparison = ratio ~ inequality)
  expect_equal(
    out$Mean_Ratio,
    c(1.007656, 0.994106),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_identical(as.character(out$sex), c("female", "male"))

  out <- estimate_contrasts(m, "island", by = "sex", comparison = ratio ~ inequality + pairwise)
  expect_equal(out$Mean_Ratio_Difference, 0.01355006, tolerance = 1e-4, ignore_attr = TRUE)
})


test_that("estimate_contrast, slopes, inequality pairwise", {
  skip_if_not_installed("parameters")

  data(qol_cancer, package = "parameters")
  qol_cancer$ID <- as.numeric(qol_cancer$ID)
  qol_cancer$grp <- as.factor(ifelse(qol_cancer$ID < 100, "Group 1", "Group 2"))

  m <- lm(QoL ~ time * education * grp, data = qol_cancer)

  # test integer handling
  expect_silent(
    estimate_contrasts(m, "time", by = "education", integer_as_continuous = TRUE)
  )
  expect_silent(
    estimate_contrasts(m, "time", by = "education", integer_as_continuous = 2)
  )
  # we also show no warning when user explicitly sets integer_as_continuous
  expect_silent(
    estimate_contrasts(m, "time", by = "education", integer_as_continuous = 10)
  )
  expect_message(
    estimate_contrasts(m, "time", by = "education"),
    regex = "Numeric variable appears to be ordinal"
  )

  out <- estimate_contrasts(
    m,
    "time",
    by = "education",
    comparison = ~inequality,
    integer_as_continuous = TRUE
  )
  expect_equal(out$Mean_Difference, 3.171296, tolerance = 1e-4, ignore_attr = TRUE)
  expect_identical(out$Parameter, "education")

  out1 <- estimate_contrasts(
    m,
    "time",
    by = "education",
    comparison = ~inequality,
    integer_as_continuous = TRUE,
    estimate = "average"
  )
  out2 <- marginaleffects::hypotheses(
    marginaleffects::avg_slopes(m, variables = "time", by = "education", hypothesis = ~pairwise),
    hypothesis = ~I(mean(abs(x)))
  )
  expect_equal(
    out1$Mean_Difference,
    out2$estimate,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  out <- estimate_contrasts(
    m,
    "time",
    by = c("education", "grp"),
    comparison = ~inequality,
    integer_as_continuous = TRUE
  )
  expect_equal(out$Mean_Difference, c(4.742403, 2.883987), tolerance = 1e-4, ignore_attr = TRUE)
  expect_identical(out$Parameter, c("education: Group 1", "education: Group 2"))

  out <- estimate_contrasts(
    m,
    "time",
    by = c("education", "grp"),
    comparison = ~inequality | grp,
    integer_as_continuous = TRUE
  )
  expect_equal(out$Mean_Difference, c(4.742403, 2.883987), tolerance = 1e-4, ignore_attr = TRUE)
  expect_identical(out$Parameter, c("education: Group 1", "education: Group 2"))

  out <- estimate_contrasts(
    m,
    "time",
    by = c("education", "grp"),
    comparison = inequality ~ pairwise| grp,
    integer_as_continuous = TRUE
  )
  expect_equal(out$Mean_Difference,  1.858416, tolerance = 1e-4, ignore_attr = TRUE)
  expect_identical(out$Parameter, "Group 1 - Group 2")

  out1 <- estimate_contrasts(
    m,
    "time",
    by = c("education", "grp"),
    comparison = ~inequality,
    estimate = "average",
    integer_as_continuous = TRUE
  )
  out2 <- marginaleffects::hypotheses(
    marginaleffects::avg_slopes(m, variables = "time", by = c("education", "grp"), hypothesis = ~pairwise | grp),
    hypothesis = ~I(mean(abs(x))) | grp
  )
  expect_equal(
    out1$Mean_Difference,
    out2$estimate,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  out <- estimate_contrasts(
    m,
    "time",
    by = c("education", "grp"),
    comparison = "inequality_pairwise",
    integer_as_continuous = TRUE
  )
  expect_equal(out$Mean_Difference, 1.858416, tolerance = 1e-4, ignore_attr = TRUE)
  expect_identical(out$Parameter, "Group 1 - Group 2")

  out <- estimate_contrasts(
    m,
    "time",
    by = "education",
    comparison = ratio ~ inequality,
    integer_as_continuous = TRUE
  )
  expect_equal(out$Mean_Ratio, 1.734764, tolerance = 1e-4, ignore_attr = TRUE)
  expect_identical(out$Parameter, "education")

  out <- estimate_contrasts(
    m,
    "time",
    by = c("education", "grp"),
    comparison = ratio ~ inequality,
    integer_as_continuous = TRUE
  )
  expect_equal(out$Mean_Ratio, c(0.0198939, 1.9717087), tolerance = 1e-4, ignore_attr = TRUE)
  expect_identical(out$Parameter, c("education: Group 1", "education: Group 2"))

  out <- estimate_contrasts(
    m,
    "time",
    by = c("education", "grp"),
    comparison = "inequality_ratio_pairwise",
    integer_as_continuous = TRUE
  )
  expect_equal(out$Mean_Ratio_Difference, -1.951815, tolerance = 1e-4, ignore_attr = TRUE)

  out <- estimate_contrasts(
    m,
    "time",
    by = c("education", "grp"),
    comparison = ~pairwise | grp,
    integer_as_continuous = TRUE
  )
  expect_identical(dim(out), c(6L, 9L))
  expect_named(
    out,
    c("Parameter", "grp", "Difference", "SE", "CI_low", "CI_high",  "t", "df", "p")
  )
})
