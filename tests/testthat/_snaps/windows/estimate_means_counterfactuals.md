# estimate_means() - counterfactuals

    Code
      print(estimate_means(m, "spp", backend = "marginaleffects"), zap_small = TRUE,
      table_width = Inf)
    Output
      Estimated Marginal Means
      
      spp   | Mean |   SE |       95% CI |    z
      -----------------------------------------
      GP    | 1.24 | 0.22 | [0.80, 1.67] | 5.55
      PR    | 0.31 | 0.08 | [0.16, 0.46] | 4.08
      DM    | 1.56 | 0.27 | [1.02, 2.09] | 5.72
      EC-A  | 0.57 | 0.12 | [0.34, 0.81] | 4.81
      EC-L  | 2.30 | 0.39 | [1.54, 3.06] | 5.96
      DES-L | 2.44 | 0.41 | [1.64, 3.24] | 5.99
      DF    | 1.34 | 0.24 | [0.87, 1.81] | 5.62
      
      Variable predicted: count
      Predictors modulated: spp
      Predictors averaged: mined, site
      Predictions are on the response-scale.

---

    Code
      print(estimate_means(m, "spp", backend = "marginaleffects", estimate = "population"),
      zap_small = TRUE, table_width = Inf)
    Output
      Average Counterfactual Predictions
      
      spp   | Mean |   SE |       95% CI |    z
      -----------------------------------------
      GP    | 1.17 | 0.21 | [0.76, 1.58] | 5.60
      PR    | 0.29 | 0.07 | [0.15, 0.43] | 4.09
      DM    | 1.48 | 0.26 | [0.97, 1.98] | 5.77
      EC-A  | 0.54 | 0.11 | [0.32, 0.76] | 4.84
      EC-L  | 2.18 | 0.36 | [1.47, 2.89] | 6.01
      DES-L | 2.31 | 0.38 | [1.56, 3.06] | 6.04
      DF    | 1.27 | 0.22 | [0.83, 1.71] | 5.66
      
      Variable predicted: count
      Predictors modulated: spp
      Predictors averaged: mined, site
      Predictions are on the response-scale.

---

    Code
      print(estimate_means(m, "spp", backend = "marginaleffects", predict = "inverse_link"),
      zap_small = TRUE, table_width = Inf)
    Output
      Estimated Marginal Means
      
      spp   | Mean |   SE |       95% CI |     z
      ------------------------------------------
      GP    | 0.62 | 0.10 | [0.45, 0.87] | -2.83
      PR    | 0.16 | 0.04 | [0.10, 0.25] | -7.88
      DM    | 0.79 | 0.13 | [0.57, 1.08] | -1.50
      EC-A  | 0.29 | 0.06 | [0.20, 0.42] | -6.32
      EC-L  | 1.16 | 0.18 | [0.86, 1.57] |  0.97
      DES-L | 1.23 | 0.19 | [0.91, 1.66] |  1.36
      DF    | 0.68 | 0.11 | [0.49, 0.93] | -2.38
      
      Variable predicted: count
      Predictors modulated: spp
      Predictors averaged: mined, site
      Predictions are on the response-scale.

---

    Code
      print(estimate_means(m, "spp", backend = "marginaleffects", estimate = "population",
        predict = "inverse_link"), zap_small = TRUE, table_width = Inf)
    Output
      Average Counterfactual Predictions
      
      spp   | Mean |   SE |       95% CI |     z
      ------------------------------------------
      GP    | 0.66 | 0.11 | [0.47, 0.91] | -2.55
      PR    | 0.16 | 0.04 | [0.10, 0.26] | -7.70
      DM    | 0.82 | 0.13 | [0.60, 1.13] | -1.20
      EC-A  | 0.30 | 0.06 | [0.21, 0.44] | -6.10
      EC-L  | 1.22 | 0.19 | [0.90, 1.64] |  1.30
      DES-L | 1.29 | 0.20 | [0.96, 1.74] |  1.69
      DF    | 0.71 | 0.12 | [0.52, 0.98] | -2.10
      
      Variable predicted: count
      Predictors modulated: spp
      Predictors averaged: mined, site
      Predictions are on the response-scale.

---

    Code
      print(estimate_means(m, "Days", backend = "marginaleffects"), zap_small = TRUE,
      table_width = Inf)
    Output
      Estimated Marginal Means
      
      Days |   Mean |    SE |           95% CI | t(174)
      -------------------------------------------------
      0    | 251.32 |  7.59 | [236.34, 266.31] |  33.11
      1    | 261.49 |  7.55 | [246.57, 276.40] |  34.61
      2    | 271.65 |  7.89 | [256.08, 287.21] |  34.45
      3    | 281.81 |  8.54 | [264.95, 298.67] |  33.00
      4    | 291.97 |  9.45 | [273.31, 310.63] |  30.89
      5    | 302.13 | 10.56 | [281.30, 322.97] |  28.62
      6    | 312.29 | 11.80 | [289.01, 335.58] |  26.47
      7    | 322.46 | 13.13 | [296.53, 348.38] |  24.55
      8    | 332.62 | 14.54 | [303.91, 361.33] |  22.87
      9    | 342.78 | 16.01 | [311.19, 374.37] |  21.41
      
      Variable predicted: Reaction
      Predictors modulated: Days
      Predictors averaged: Subject

---

    Code
      print(estimate_means(m, "Days", backend = "marginaleffects", estimate = "population"),
      zap_small = TRUE, table_width = Inf)
    Output
      Average Counterfactual Predictions
      
      Days |   Mean |    SE |           95% CI | t(174)
      -------------------------------------------------
      0    | 248.80 |  7.59 | [233.82, 263.79] |  32.78
      1    | 259.04 |  7.55 | [244.13, 273.95] |  34.29
      2    | 269.28 |  7.89 | [253.72, 284.85] |  34.15
      3    | 279.52 |  8.54 | [262.67, 296.38] |  32.73
      4    | 289.76 |  9.45 | [271.11, 308.42] |  30.65
      5    | 300.01 | 10.56 | [279.17, 320.84] |  28.42
      6    | 310.25 | 11.80 | [286.96, 333.53] |  26.30
      7    | 320.49 | 13.13 | [294.56, 346.41] |  24.40
      8    | 330.73 | 14.54 | [302.02, 359.43] |  22.74
      9    | 340.97 | 16.01 | [309.37, 372.56] |  21.30
      
      Variable predicted: Reaction
      Predictors modulated: Days
      Predictors averaged: Subject

