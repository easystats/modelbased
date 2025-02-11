.estimate_contrasts_effecsize <- function() {
  # Add standardized effect size
  if (!effectsize %in% c("none", "emmeans", "marginal", "bootES")) {
    message("Unsupported effect size '", effectsize, "', returning none.")
  }

  if (effectsize == "emmeans") {
    eff <- emmeans::eff_size(
      estimated, sigma = stats::sigma(model),
      edf = stats::df.residual(model), method = "identity")
    eff <- as.data.frame(eff)
    eff <- eff[c(2, 5:6)]
    names(eff) <- c("partial_d", "es_CI_low", "es_CI_high")
    contrasts <- cbind(contrasts, eff)

  } else if (effectsize == "marginal") {
    # Original: d_adj <- t * se_b / sigma * sqrt(1 - R2_cov)
    # d_adj <- contrasts$t * contrasts$SE / sigma(model) * sqrt(1 - R2)
    # New: d_adj <- difference * (1- R2)/ sigma
    R2 <- summary(model)$r.squared
    d_adj <- contrasts$Difference * (1 - R2) / sigma(model)
    contrasts <- cbind(contrasts, marginal_d = d_adj)

    } else if (effectsize == "bootES") {
      if (bootstraps < 500) {
        message("Number of bootstraps probably too low. Consider increasing it.")
      }

      insight::check_if_installed("bootES")
      dat <- insight::get_data(model)
      resp <- insight::find_response(model)
      group <- names(estimated@model.info$xlev)
      contrast <- estimated@misc$con.coef

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
        y <- as.data.frame(summary(y))})

      eff <- do.call(rbind, es.lists)
      eff <- eff[1:3]
      names(eff) <- c(bootES_type, paste0(bootES_type, "_CI_low"),
                      paste0(bootES_type, "es_CI_high"))

      contrasts <- cbind(contrasts, eff)
        }
}
