.estimate_contrasts_effectsize <- function(
  model,
  estimated,
  contrasts_results,
  effectsize,
  bootstraps,
  bootES_type,
  backend,
  ci = 0.95,
  rescale = NULL
) {
  # Add standardized effect size
  insight::validate_argument(effectsize, c("none", "emmeans", "marginal", "boot"))

  if (effectsize != "none" && isTRUE(attributes(estimated)$joint_test)) {
    insight::format_error("Effect sizes are not possible for joint or omnibus tests.")
  }

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

  # ordinal models are standardized by the SD of the latent error
  latent_ordinal <- effectsize == "emmeans" && inherits(model, c("polr", "clm"))

  # Check if the model's response variable follows a Gaussian (normal)
  # distribution. Effect size calculations implemented in this function are
  # designed for Gaussian models and may not be appropriate or meaningful for
  # other types of distributions. Ordinal models on the latent scale are an
  # exception for `effectsize = "emmeans"`.
  if (!latent_ordinal && insight::get_family(model)$family != "gaussian") {
    insight::format_error(paste0(
      "We strongly recommend not using the `effectsize` ",
      "argument with non-Gaussian models."
    ))
  }

  switch(
    effectsize,
    emmeans = {
      if (latent_ordinal) {
        sigma <- .ordinal_latent_sigma(model, attributes(estimated)$predict)
        # `rescale` also rescales the latent error SD
        if (!is.null(rescale)) {
          sigma <- sigma * abs(rescale[2])
        }
        edf <- Inf
      } else {
        sigma <- insight::get_sigma(model, verbose = FALSE)
        edf <- insight::get_df(model)
      }
      eff <- emmeans::eff_size(
        estimated,
        sigma = sigma,
        edf = edf,
        method = "identity"
      )
      eff <- as.data.frame(stats::confint(eff, level = ci, adjust = "none"))
      # select columns by name, because `by` variables add columns
      es_col <- intersect(c("effect.size", "estimate"), colnames(eff))[1]
      ci_low <- intersect(c("lower.CL", "asymp.LCL"), colnames(eff))[1]
      ci_high <- intersect(c("upper.CL", "asymp.UCL"), colnames(eff))[1]
      eff <- eff[c(es_col, ci_low, ci_high)]
      names(eff) <- c("partial_d", "es_CI_low", "es_CI_high")
      # match effect sizes to the contrasts, which may be sorted differently
      emmeans_rows <- attributes(contrasts_results)$emmeans_rows
      if (!is.null(emmeans_rows)) {
        eff <- eff[emmeans_rows, , drop = FALSE]
        rownames(eff) <- NULL
      }
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


# SD of the latent error of ordinal models: pi / sqrt(3) for logit, 1 for probit
.ordinal_latent_sigma <- function(model, predict) {
  if (!identical(predict, "latent")) {
    insight::format_error(paste0(
      "Effect sizes for ordinal models are only possible on the latent scale. ",
      "Please use `predict = \"latent\"`."
    ))
  }
  if (inherits(model, "clm") && !is.null(model$formulas$scale)) {
    insight::format_error(
      "Effect sizes are not possible for ordinal models with a `scale` formula."
    )
  }
  # nominal effects make the thresholds differ between groups
  if (inherits(model, "clm") && !is.null(model$formulas$nominal)) {
    insight::format_error(
      "Effect sizes are not possible for ordinal models with a `nominal` formula."
    )
  }
  link <- if (inherits(model, "polr")) model$method else model$link
  switch(
    link,
    logistic = ,
    logit = pi / sqrt(3),
    probit = 1,
    insight::format_error(paste0(
      "Effect sizes for ordinal models are only possible for the `logit` and ",
      "`probit` links, but the model uses the `",
      link,
      "` link."
    ))
  )
}


# `rescale` argument for emmeans from `...`, also when partly matched
.rescale_argument <- function(...) {
  dots <- list(...)
  dot_names <- names(dots)
  if (is.null(dot_names)) {
    return(NULL)
  }
  i <- which(nzchar(dot_names) & startsWith("rescale", dot_names))
  if (length(i) == 0) {
    return(NULL)
  }
  dots[[i[1]]]
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
