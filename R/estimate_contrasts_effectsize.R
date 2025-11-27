.estimate_contrasts_effectsize <- function(
  model,
  estimated,
  contrasts_results,
  effectsize,
  bootstraps,
  bootES_type,
  backend
) {
  # Add standardized effect size
  insight::validate_argument(effectsize, c("none", "emmeans", "marginal", "boot"))

  if (effectsize == "emmeans" && backend != "emmeans") {
    insight::format_error(
      "`effectsize = \"emmeans\"` only possible with `backend = \"emmeans\"`"
    )
  }

  if (!is.null(bootES_type) && effectsize != "boot") {
    insight::format_error("`es_type` can only be used when `effectsize = \"boot\"`.")
  }

  # Check if the model includes any random effects. Effect size calculations in
  # the current implementation are not designed for, or may not be appropriate
  # for, models with random effects. Random effects complicate the calculation
  # of standardized effect sizes, as it's not straightforward how to account for
  # the variance explained by the random effects in the denominator of the
  # effect size calculation.
  if (length(insight::find_random(model)) > 0) {
    insight::format_error(paste0(
      "We strongly recommend not using the `effectsize` ",
      "argument with models containing random effects."
    ))
  }

  # Check if the model's response variable follows a Gaussian (normal)
  # distribution. Effect size calculations implemented in this function are
  # designed for Gaussian models and may not be appropriate or meaningful for
  # other types of distributions.
  if (insight::get_family(model)$family != "gaussian") {
    insight::format_error(paste0(
      "We strongly recommend not using the `effectsize` ",
      "argument with non-Gaussian models."
    ))
  }

  switch(
    effectsize,
    emmeans = {
      eff <- emmeans::eff_size(
        estimated,
        sigma = insight::get_sigma(model, verbose = FALSE),
        edf = insight::get_df(model),
        method = "identity"
      )
      eff <- as.data.frame(eff)
      eff <- eff[c(2, 5:6)]
      names(eff) <- c("partial_d", "es_CI_low", "es_CI_high")
      contrasts_results <- cbind(contrasts_results, eff)
    },
    marginal = {
      # Original: d_adj <- t * se_b / sigma * sqrt(1 - R2_cov)
      # d_adj <- contrasts$t * contrasts$SE / sigma(model) * sqrt(1 - R2)
      # New: d_adj <- difference * (1- R2)/ sigma
      R2 <- summary(model)$r.squared
      d_adj <- contrasts_results$Difference *
        (1 - R2) /
        insight::get_sigma(model, verbose = FALSE)
      contrasts_results <- cbind(contrasts_results, marginal_d = d_adj)
    },
    boot = {
      # set default
      if (is.null(bootES_type)) {
        bootES_type <- "cohens.d"
      }
      insight::check_if_installed("bootES")
      dat <- insight::get_data(model)
      resp <- insight::find_response(model)
      group <- .get_group_variable(estimated, backend)
      contrast <- .get_contrasts(estimated, backend)

      contrast <- lapply(seq_len(nrow(contrast)), function(x) {
        z <- contrast[x, ]
        names(z) <- levels(as.factor(dat[[group]]))
        z
      })

      es.lists <- lapply(contrast, function(x) {
        y <- bootES::bootES(
          data = stats::na.omit(dat),
          R = bootstraps,
          data.col = resp,
          group.col = group,
          contrast = x,
          effect.type = bootES_type
        )
        y <- as.data.frame(summary(y))
      })

      eff <- do.call(rbind, es.lists)
      eff <- eff[1:3]
      names(eff) <- c(
        bootES_type,
        paste0(bootES_type, "_CI_low"),
        paste0(bootES_type, "_CI_high")
      )

      contrasts_results <- cbind(contrasts_results, eff)
    }
  )
  .rename_es_columns(contrasts_results)
}


.get_group_variable <- function(estimated, backend) {
  if (backend == "emmeans") {
    names(estimated@model.info$xlev)
  } else if (backend == "marginaleffects") {
    attributes(estimated)$contrast
  }
}


.get_contrasts <- function(estimated, backend) {
  if (backend == "emmeans") {
    estimated@misc$con.coef
  } else if (backend == "marginaleffects") {
    attributes(estimated)$linfct # This is not correct
  }
}


.rename_es_columns <- function(x) {
  colnames(x) <- gsub("cohens.d", "Cohens_d", colnames(x), fixed = TRUE)
  colnames(x) <- gsub("hedges.g", "Hedges_g", colnames(x), fixed = TRUE)
  colnames(x) <- gsub("partial.d", "d_partial", colnames(x), fixed = TRUE)
  colnames(x) <- gsub("akp.robust.d", "Cohens_d_robust", colnames(x), fixed = TRUE)
  x
}
