skip_on_cran()
skip_if_not_installed("curl")
skip_if_offline()
skip_if_not_installed("brms")
skip_if_not_installed("BH")
skip_if_not_installed("RcppEigen")
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
skip_if_not_installed("httr2")
skip_if_not_installed("MASS")

test_that("estimate_relation prints ordinal models correctly", {
  m <- suppressWarnings(insight::download_model("brms_categorical_2_num"))
  skip_if(is.null(m))
  out <- suppressWarnings(estimate_relation(m))
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")
  out <- suppressWarnings(estimate_means(m, by = "Sepal.Width"))
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")

  m <- MASS::polr(Species ~ Sepal.Width, data = iris)
  out <- estimate_relation(m, verbose = FALSE)
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")
  out <- estimate_means(m, by = "Sepal.Width")
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")

  # keep row column
  out <- suppressWarnings(estimate_relation(m, data = iris[1:3, ], verbose = FALSE))
  expect_named(
    out,
    c("Row", "Response", "Sepal.Width", "Predicted", "CI_low", "CI_high", "Residuals")
  )
  expect_identical(dim(out), c(9L, 7L))
})


# compares probabilities from the emmeans and the marginaleffects backend,
# matching rows by focal terms and response category
.compare_ordinal_backends <- function(out_emmeans, out_marginaleffects, by) {
  merge_by <- c(by, "Response")
  out_emmeans <- as.data.frame(out_emmeans)[c(merge_by, "Probability")]
  out_marginaleffects <- as.data.frame(out_marginaleffects)[c(merge_by, "Probability")]
  for (i in merge_by) {
    out_emmeans[[i]] <- as.character(out_emmeans[[i]])
    out_marginaleffects[[i]] <- as.character(out_marginaleffects[[i]])
  }
  merge(out_emmeans, out_marginaleffects, by = merge_by)
}


test_that("estimate_means, backend emmeans, ordinal, predict = 'prob'", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  # one row per focal level and response category
  out <- estimate_means(m, "Type", predict = "prob", backend = "emmeans")
  expect_identical(nrow(out), 12L)
  expect_setequal(as.character(out$Response), levels(housing$Sat))
  compared <- .compare_ordinal_backends(out, estimate_means(m, "Type"), "Type")
  expect_identical(nrow(compared), 12L)
  expect_equal(compared$Probability.x, compared$Probability.y, tolerance = 1e-6)

  # two focal terms
  out <- estimate_means(m, c("Type", "Infl"), predict = "prob", backend = "emmeans")
  expect_identical(nrow(out), 36L)
  compared <- .compare_ordinal_backends(
    out,
    estimate_means(m, c("Type", "Infl")),
    c("Type", "Infl")
  )
  expect_identical(nrow(compared), 36L)
  expect_equal(compared$Probability.x, compared$Probability.y, tolerance = 1e-6)
})


test_that("estimate_means, backend emmeans, ordinal, default predict", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  # default returns probabilities, same as predict = "prob"
  out_prob <- estimate_means(m, "Type", predict = "prob", backend = "emmeans")
  out_default <- estimate_means(m, "Type", backend = "emmeans")
  expect_identical(nrow(out_default), 12L)
  expect_identical(
    paste(out_default$Type, out_default$Response),
    paste(out_prob$Type, out_prob$Response)
  )
  expect_equal(out_default$Probability, out_prob$Probability, tolerance = 1e-6)

  # contrasts are not affected by the new default for means
  out <- estimate_contrasts(m, "Type", backend = "emmeans")
  expect_identical(
    paste(out$Level1, out$Level2),
    c(
      "Apartment Tower",
      "Atrium Apartment",
      "Atrium Tower",
      "Terrace Apartment",
      "Terrace Atrium",
      "Terrace Tower"
    )
  )
  expect_equal(
    out$Difference,
    c(-0.5723501, 0.2061636, -0.3661866, -0.5186648, -0.7248283, -1.0910149),
    tolerance = 1e-6
  )
})


test_that("estimate_means, backend emmeans, ordinal, mean.class and latent", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  modes <- c(mean.class = "Mean_class", latent = "Latent")
  for (mode in names(modes)) {
    out <- estimate_means(m, "Type", predict = mode, backend = "emmeans")
    expect_identical(nrow(out), 4L)
    expect_true(modes[[mode]] %in% colnames(out))
    expect_identical(attributes(out)$coef_name, modes[[mode]])
    expected <- as.data.frame(suppressMessages(emmeans::emmeans(m, "Type", mode = mode)))
    estimate_column <- setdiff(
      colnames(expected),
      c("Type", "SE", "df", "asymp.LCL", "asymp.UCL")
    )
    compared <- merge(
      data.frame(
        Type = as.character(out$Type),
        x = out[[modes[[mode]]]],
        stringsAsFactors = FALSE
      ),
      data.frame(
        Type = as.character(expected$Type),
        y = expected[[estimate_column]],
        stringsAsFactors = FALSE
      ),
      by = "Type"
    )
    expect_identical(nrow(compared), 4L)
    expect_equal(compared$x, compared$y, tolerance = 1e-6)
  }
})


test_that("estimate_means, backend emmeans, ordinal, threshold modes", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  modes <- c(
    cum.prob = "Probability",
    exc.prob = "Probability",
    linear.predictor = "Linear_predictor"
  )
  for (mode in names(modes)) {
    out <- estimate_means(m, "Type", predict = mode, backend = "emmeans")
    expect_identical(nrow(out), 8L)
    expect_setequal(as.character(out$Threshold), c("Low|Medium", "Medium|High"))
    expect_true(modes[[mode]] %in% colnames(out))
    expect_identical(attributes(out)$coef_name, modes[[mode]])
    expected <- as.data.frame(suppressMessages(emmeans::emmeans(
      m,
      c("Type", "cut"),
      mode = mode
    )))
    estimate_column <- setdiff(
      colnames(expected),
      c("Type", "cut", "SE", "df", "asymp.LCL", "asymp.UCL")
    )
    compared <- merge(
      data.frame(
        Type = as.character(out$Type),
        Threshold = as.character(out$Threshold),
        x = out[[modes[[mode]]]],
        stringsAsFactors = FALSE
      ),
      data.frame(
        Type = as.character(expected$Type),
        Threshold = as.character(expected$cut),
        y = expected[[estimate_column]],
        stringsAsFactors = FALSE
      ),
      by = c("Type", "Threshold")
    )
    expect_identical(nrow(compared), 8L)
    expect_equal(compared$x, compared$y, tolerance = 1e-6)
  }
})


test_that("estimate_means, backend emmeans, ordinal, clm and glmmTMB", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ordinal")
  data(housing, package = "MASS")
  m <- ordinal::clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  out <- estimate_means(m, "Type", predict = "prob", backend = "emmeans")
  expect_identical(nrow(out), 12L)
  compared <- .compare_ordinal_backends(out, estimate_means(m, "Type"), "Type")
  expect_identical(nrow(compared), 12L)
  expect_equal(compared$Probability.x, compared$Probability.y, tolerance = 1e-6)

  # ordinal glmmTMB not supported in marginaleffects <= 1.0.0
  skip_if_not_installed("glmmTMB", minimum_version = "1.1.15.2")
  skip_if_not_installed("marginaleffects", minimum_version = "1.0.1")

  m <- glmmTMB::glmmTMB(
    Sat ~ Infl + Type + Cont,
    data = housing,
    family = glmmTMB::ordinal()
  )
  out <- estimate_means(m, "Type", predict = "prob", backend = "emmeans")
  expect_identical(nrow(out), 12L)
  compared <- .compare_ordinal_backends(out, estimate_means(m, "Type"), "Type")
  expect_identical(nrow(compared), 12L)
  expect_equal(compared$Probability.x, compared$Probability.y, tolerance = 1e-6)
})


test_that("estimate_means, backend emmeans, ordinal, transformed response", {
  skip_if_not_installed("emmeans")
  data(housing, package = "MASS")
  housing$SatNum <- as.integer(housing$Sat)
  # `Hess = TRUE`, else `vcov()` re-fits the model in an environment where
  # `SatNum` does not exist
  m <- MASS::polr(
    factor(SatNum) ~ Infl + Type + Cont,
    weights = Freq,
    data = housing,
    Hess = TRUE
  )
  out <- estimate_means(m, "Type", backend = "emmeans")
  expect_identical(nrow(out), 12L)
  expect_setequal(as.character(out$Response), c("1", "2", "3"))
  compared <- .compare_ordinal_backends(out, estimate_means(m, "Type"), "Type")
  expect_identical(nrow(compared), 12L)
  expect_equal(compared$Probability.x, compared$Probability.y, tolerance = 1e-6)
})


test_that("estimate_means, print bracl", {
  skip_if_not_installed("brglm2")
  # required for the penguins dataset, which was added in R 4.5.0
  skip_if(getRversion() < "4.5.0")

  data(penguins, package = "datasets")

  m <- brglm2::bracl(species ~ island + sex, data = penguins)
  out <- estimate_means(m, by = "island")
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")

  m <- nnet::multinom(species ~ island + sex, data = penguins)
  out <- estimate_means(m, by = "island")
  expect_snapshot(print(out, zap_small = TRUE), variant = "windows")
})
