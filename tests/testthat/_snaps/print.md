# estimate_contrasts - by with special character

    Code
      print(estimate_contrasts(fit, "c172code", "barthtot = [sd]", backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      c172code   | barthtot | Difference |   SE |         95% CI | t(810) |      p
      ----------------------------------------------------------------------------
      low - high | 35.18764 |      -0.22 | 0.56 | [-1.31,  0.87] |  -0.40 | > .999
      low - mid  | 35.18764 |       0.22 | 0.42 | [-0.60,  1.05] |   0.53 | > .999
      mid - high | 35.18764 |      -0.45 | 0.49 | [-1.40,  0.51] |  -0.92 | > .999
      low - high | 64.79167 |      -0.72 | 0.40 | [-1.49,  0.06] |  -1.81 | 0.636 
      low - mid  | 64.79167 |      -0.17 | 0.31 | [-0.79,  0.45] |  -0.54 | > .999
      mid - high | 64.79167 |      -0.55 | 0.33 | [-1.19,  0.10] |  -1.65 | 0.789 
      low - high | 94.39569 |      -1.21 | 0.59 | [-2.37, -0.05] |  -2.05 | 0.450 
      low - mid  | 94.39569 |      -0.56 | 0.46 | [-1.47,  0.34] |  -1.23 | > .999
      mid - high | 94.39569 |      -0.65 | 0.48 | [-1.60,  0.30] |  -1.34 | > .999
      
      Variable predicted: neg_c_7
      Predictors modulated: c172code
      p-value adjustment method: Holm (1979)

---

    Code
      print(estimate_contrasts(fit, c("c172code", "barthtot = [sd]"), backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      c172code    |            barthtot | Difference |   SE |         95% CI | t(810) |      p
      ----------------------------------------------------------------------------------------
      low - high  | 35.18764 - 35.18764 |      -0.22 | 0.56 | [-1.31,  0.87] |  -0.40 | > .999
      low - mid   | 35.18764 - 35.18764 |       0.22 | 0.42 | [-0.60,  1.05] |   0.53 | > .999
      mid - high  | 35.18764 - 35.18764 |      -0.45 | 0.49 | [-1.40,  0.51] |  -0.92 | > .999
      high - high | 35.18764 - 64.79167 |       1.43 | 0.32 | [ 0.81,  2.06] |   4.50 | < .001
      low - high  | 35.18764 - 64.79167 |       1.21 | 0.46 | [ 0.31,  2.11] |   2.65 | 0.099 
      low - low   | 35.18764 - 64.79167 |       1.93 | 0.27 | [ 1.40,  2.45] |   7.20 | < .001
      low - mid   | 35.18764 - 64.79167 |       1.76 | 0.39 | [ 0.99,  2.52] |   4.52 | < .001
      mid - high  | 35.18764 - 64.79167 |       0.99 | 0.37 | [ 0.26,  1.71] |   2.67 | 0.099 
      mid - mid   | 35.18764 - 64.79167 |       1.53 | 0.16 | [ 1.23,  1.84] |   9.80 | < .001
      high - high | 35.18764 - 94.39569 |       2.86 | 0.64 | [ 1.61,  4.11] |   4.50 | < .001
      low - high  | 35.18764 - 94.39569 |       2.64 | 0.56 | [ 1.55,  3.74] |   4.73 | < .001
      low - low   | 35.18764 - 94.39569 |       3.85 | 0.54 | [ 2.80,  4.90] |   7.20 | < .001
      low - mid   | 35.18764 - 94.39569 |       3.29 | 0.42 | [ 2.47,  4.11] |   7.90 | < .001
      mid - high  | 35.18764 - 94.39569 |       2.42 | 0.49 | [ 1.46,  3.38] |   4.95 | < .001
      mid - mid   | 35.18764 - 94.39569 |       3.07 | 0.31 | [ 2.45,  3.68] |   9.80 | < .001
      low - high  | 64.79167 - 35.18764 |      -2.15 | 0.51 | [-3.14, -1.15] |  -4.24 | < .001
      low - mid   | 64.79167 - 35.18764 |      -1.70 | 0.35 | [-2.40, -1.01] |  -4.81 | < .001
      mid - high  | 64.79167 - 35.18764 |      -1.98 | 0.46 | [-2.88, -1.08] |  -4.32 | < .001
      low - high  | 64.79167 - 64.79167 |      -0.72 | 0.40 | [-1.49,  0.06] |  -1.81 | 0.636 
      low - mid   | 64.79167 - 64.79167 |      -0.17 | 0.31 | [-0.79,  0.45] |  -0.54 | > .999
      mid - high  | 64.79167 - 64.79167 |      -0.55 | 0.33 | [-1.19,  0.10] |  -1.65 | 0.789 
      high - high | 64.79167 - 94.39569 |       1.43 | 0.32 | [ 0.81,  2.06] |   4.50 | < .001
      low - high  | 64.79167 - 94.39569 |       0.72 | 0.51 | [-0.28,  1.71] |   1.41 | > .999
      low - low   | 64.79167 - 94.39569 |       1.93 | 0.27 | [ 1.40,  2.45] |   7.20 | < .001
      low - mid   | 64.79167 - 94.39569 |       1.36 | 0.35 | [ 0.68,  2.04] |   3.93 | 0.001 
      mid - high  | 64.79167 - 94.39569 |       0.89 | 0.46 | [-0.02,  1.79] |   1.93 | 0.545 
      mid - mid   | 64.79167 - 94.39569 |       1.53 | 0.16 | [ 1.23,  1.84] |   9.80 | < .001
      low - high  | 94.39569 - 35.18764 |      -4.07 | 0.59 | [-5.23, -2.92] |  -6.91 | < .001
      low - mid   | 94.39569 - 35.18764 |      -3.63 | 0.46 | [-4.54, -2.72] |  -7.81 | < .001
      mid - high  | 94.39569 - 35.18764 |      -3.51 | 0.48 | [-4.46, -2.57] |  -7.30 | < .001
      low - high  | 94.39569 - 64.79167 |      -2.64 | 0.50 | [-3.62, -1.67] |  -5.32 | < .001
      low - mid   | 94.39569 - 64.79167 |      -2.10 | 0.43 | [-2.95, -1.24] |  -4.82 | < .001
      mid - high  | 94.39569 - 64.79167 |      -2.08 | 0.36 | [-2.79, -1.37] |  -5.74 | < .001
      low - high  | 94.39569 - 94.39569 |      -1.21 | 0.59 | [-2.37, -0.05] |  -2.05 | 0.450 
      low - mid   | 94.39569 - 94.39569 |      -0.56 | 0.46 | [-1.47,  0.34] |  -1.23 | > .999
      mid - high  | 94.39569 - 94.39569 |      -0.65 | 0.48 | [-1.60,  0.30] |  -1.34 | > .999
      
      Variable predicted: neg_c_7
      Predictors modulated: c172code, barthtot = [sd]
      p-value adjustment method: Holm (1979)

# estimate_means - by is list

    Code
      print(estimate_means(fit, list(c172code = c("low", "high"), c161sex = c(
        "Female", "Male")), backend = "marginaleffects"), table_width = Inf,
      zap_small = TRUE)
    Output
      Estimated Marginal Means
      
      c172code | c161sex |  Mean |   SE |         95% CI | t(825)
      -----------------------------------------------------------
      low      |  Female | 12.13 | 0.33 | [11.48, 12.78] |  36.44
      high     |  Female | 12.73 | 0.38 | [11.98, 13.48] |  33.31
      low      |    Male | 11.09 | 0.61 | [ 9.89, 12.28] |  18.25
      high     |    Male | 11.77 | 0.58 | [10.64, 12.90] |  20.45
      
      Variable predicted: neg_c_7
      Predictors modulated: c172code = c('low', 'high'), c161sex = c('Female', 'Male')
      Predictors averaged: e16sex

---

    Code
      print(estimate_means(fit, c("c172code = c('low', 'high')",
        "c161sex = c('Female', 'Male')"), backend = "marginaleffects"), table_width = Inf,
      zap_small = TRUE)
    Output
      Estimated Marginal Means
      
      c172code | c161sex |  Mean |   SE |         95% CI | t(825)
      -----------------------------------------------------------
      low      |  Female | 12.13 | 0.33 | [11.48, 12.78] |  36.44
      high     |  Female | 12.73 | 0.38 | [11.98, 13.48] |  33.31
      low      |    Male | 11.09 | 0.61 | [ 9.89, 12.28] |  18.25
      high     |    Male | 11.77 | 0.58 | [10.64, 12.90] |  20.45
      
      Variable predicted: neg_c_7
      Predictors modulated: c172code = c('low', 'high'), c161sex = c('Female', 'Male')
      Predictors averaged: e16sex

# estimate_epectation - don't print empty RE columns

    Code
      print(estimate_expectation(m, by = "spp", predict = "conditional"), zap_small = TRUE)
    Output
      spp   | Predicted |   SE |           CI
      ---------------------------------------
      GP    |      0.73 | 0.21 | [0.32, 1.14]
      PR    |      0.42 | 0.16 | [0.11, 0.72]
      DM    |      0.94 | 0.25 | [0.45, 1.43]
      EC-A  |      0.60 | 0.19 | [0.24, 0.96]
      EC-L  |      1.42 | 0.37 | [0.69, 2.14]
      DES-L |      1.34 | 0.36 | [0.63, 2.04]
      DF    |      0.78 | 0.21 | [0.37, 1.19]

