skip_if_not_installed("emmeans")
skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

mt <- mtcars
mt$g <- factor(mt$cyl)
mt$a <- factor(mt$am)
m2 <- lm(mpg ~ wt * g, data = mt)
m3 <- lm(mpg ~ wt * g * a, data = mt)
m6 <- lm(mpg ~ wt * g * a + hp, data = mt)

# filters for `by`, the matching `at` for emmeans, and the levels that each
# filter allows
filters <- list(
  F1 = list(
    by = c("g='4'", "a"),
    at = list(g = "4"),
    allowed = list(g = "4", a = c("0", "1"))
  ),
  F2 = list(
    by = c("g=c('4','6')", "a='1'"),
    at = list(g = c("4", "6"), a = "1"),
    allowed = list(g = c("4", "6"), a = "1")
  ),
  F3 = list(
    by = c("g", "a='1'"),
    at = list(a = "1"),
    allowed = list(g = c("4", "6", "8"), a = "1")
  ),
  F4 = list(
    by = "g=c('8','4')",
    at = list(g = c("8", "4")),
    allowed = list(g = c("8", "4"))
  )
)

# identify the slope in a label like "4, 1", "6" or "g4 a1"; variables that
# the label leaves out take the one value their filter allows
.slope_key <- function(label, allowed) {
  vars <- names(allowed)
  tokens <- strsplit(trimws(label), ",\\s*|\\s+")[[1]]
  prefixed <- vapply(
    tokens,
    function(tk) {
      any(vapply(vars, function(v) startsWith(tk, v) && substring(tk, nchar(v) + 1) %in% allowed[[v]], logical(1)))
    },
    logical(1)
  )
  values <- list()
  if (all(prefixed)) {
    for (tk in tokens) {
      v <- vars[vapply(vars, function(v) startsWith(tk, v), logical(1))][1]
      values[[v]] <- substring(tk, nchar(v) + 1)
    }
  } else if (length(tokens) == length(vars)) {
    values <- stats::setNames(as.list(tokens), vars)
  } else {
    values <- stats::setNames(as.list(tokens), vars[lengths(allowed) > 1])
  }
  for (v in vars) {
    if (is.null(values[[v]])) {
      values[[v]] <- allowed[[v]]
    }
  }
  paste(vars, unlist(values[vars]), sep = "=", collapse = ";")
}

# check that `out` and the emmeans contrasts in `oracle` pair one to one,
# with the same two slopes, the same difference and the same p-value
expect_matches_oracle <- function(out, oracle, allowed) {
  oracle <- as.data.frame(oracle)
  expect_identical(nrow(out), nrow(oracle))
  oracle_pairs <- strsplit(as.character(oracle$contrast), " - ", fixed = TRUE)
  oracle_1 <- vapply(oracle_pairs, function(x) .slope_key(x[1], allowed), character(1))
  oracle_2 <- vapply(oracle_pairs, function(x) .slope_key(x[2], allowed), character(1))
  used <- logical(nrow(oracle))
  for (i in seq_len(nrow(out))) {
    key_1 <- .slope_key(as.character(out$Level1[i]), allowed)
    key_2 <- .slope_key(as.character(out$Level2[i]), allowed)
    same <- which(oracle_1 == key_1 & oracle_2 == key_2)
    flipped <- which(oracle_1 == key_2 & oracle_2 == key_1)
    expect_identical(length(same) + length(flipped), 1L)
    if (length(same) + length(flipped) != 1L) {
      next
    }
    j <- c(same, flipped)
    sign <- if (length(same)) 1 else -1
    expect_false(used[j])
    used[j] <- TRUE
    expect_equal(out$Difference[i], sign * oracle$estimate[j], tolerance = 1e-5, ignore_attr = TRUE)
    expect_equal(out$p[i], oracle$p.value[j], tolerance = 1e-5, ignore_attr = TRUE)
  }
  expect_true(all(used))
}

.oracle <- function(model, filter) {
  specs <- if (length(filter$allowed) == 1) ~g else ~ g * a
  pairs(emmeans::emtrends(model, specs, "wt", at = filter$at), adjust = "none")
}

probes <- list(
  list(model = m3, filter = filters$F1),
  list(model = m3, filter = filters$F2),
  list(model = m3, filter = filters$F3),
  list(model = m6, filter = filters$F1),
  list(model = m6, filter = filters$F2),
  list(model = m6, filter = filters$F3),
  list(model = m2, filter = filters$F4)
)


test_that("estimate_contrasts, slopes, by-filters, marginaleffects", {
  for (probe in probes) {
    oracle <- .oracle(probe$model, probe$filter)
    for (estimate in c("typical", "average")) {
      out <- estimate_contrasts(probe$model, "wt", by = probe$filter$by, estimate = estimate)
      expect_matches_oracle(out, oracle, probe$filter$allowed)
    }
  }
})


test_that("estimate_contrasts, slopes, by-filters, emmeans", {
  for (probe in probes) {
    oracle <- .oracle(probe$model, probe$filter)
    out <- estimate_contrasts(probe$model, "wt", by = probe$filter$by, backend = "emmeans")
    expect_matches_oracle(out, oracle, probe$filter$allowed)
  }
})


test_that("estimate_contrasts, slopes, by-filters leave fewer than two slopes", {
  calls <- list(
    list(model = m2, by = "g='4'"),
    list(model = m3, by = c("g='4'", "a='1'"))
  )
  for (cl in calls) {
    expect_error(
      estimate_contrasts(cl$model, "wt", by = cl$by, estimate = "typical"),
      regexp = "at least two"
    )
    expect_error(
      estimate_contrasts(cl$model, "wt", by = cl$by, estimate = "average"),
      regexp = "at least two"
    )
    expect_error(
      estimate_contrasts(cl$model, "wt", by = cl$by, backend = "emmeans"),
      regexp = "at least two"
    )
  }
  # spaces around "=", and `estimate = "specific"`
  expect_error(
    estimate_contrasts(m3, "wt", by = c("g = '4'", "a = '1'"), estimate = "specific"),
    regexp = "at least two"
  )
  out <- estimate_contrasts(m3, "wt", by = c("g = '4'", "a"), estimate = "average")
  expect_matches_oracle(out, .oracle(m3, filters$F1), filters$F1$allowed)
  # with `estimate = "average"`, only slopes with data count: for 5 gears,
  # all cars have am = 1
  mt$gear <- factor(mt$gear)
  m7 <- lm(mpg ~ wt * a + gear, data = mt)
  expect_error(
    estimate_contrasts(m7, "wt", by = c("gear='5'", "a"), estimate = "average"),
    regexp = "at least two"
  )
  expect_identical(nrow(estimate_contrasts(m7, "wt", by = c("gear='5'", "a"))), 1L)
})


test_that("estimate_contrasts, slopes, no by-filter", {
  oracle <- pairs(emmeans::emtrends(m3, ~ g * a, "wt"), adjust = "none")
  allowed <- list(g = c("4", "6", "8"), a = c("0", "1"))
  for (estimate in c("typical", "average")) {
    out <- estimate_contrasts(m3, "wt", by = c("g", "a"), estimate = estimate)
    expect_matches_oracle(out, oracle, allowed)
  }
  out <- estimate_contrasts(m3, "wt", by = c("g", "a"), backend = "emmeans")
  expect_matches_oracle(out, oracle, allowed)
})
