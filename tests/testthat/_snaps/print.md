# estimate_contrasts - by with special character

    Code
      print(estimate_contrasts(fit, "c172code", "barthtot = [sd]", backend = "marginaleffects",
        p_adjust = "holm"), table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      Level1 | Level2 | barthtot | Difference |   SE |        95% CI | t(810) |      p
      --------------------------------------------------------------------------------
      mid    | low    |    35.19 |      -0.22 | 0.42 | [-1.05, 0.60] |  -0.53 | > .999
      high   | low    |    35.19 |       0.22 | 0.56 | [-0.87, 1.31] |   0.40 | > .999
      high   | mid    |    35.19 |       0.45 | 0.49 | [-0.51, 1.40] |   0.92 | > .999
      mid    | low    |    64.79 |       0.17 | 0.31 | [-0.45, 0.79] |   0.54 | > .999
      high   | low    |    64.79 |       0.72 | 0.40 | [-0.06, 1.49] |   1.81 |  0.565
      high   | mid    |    64.79 |       0.55 | 0.33 | [-0.10, 1.19] |   1.65 |  0.691
      mid    | low    |    94.40 |       0.56 | 0.46 | [-0.34, 1.47] |   1.23 | > .999
      high   | low    |    94.40 |       1.21 | 0.59 | [ 0.05, 2.37] |   2.05 |  0.368
      high   | mid    |    94.40 |       0.65 | 0.48 | [-0.30, 1.60] |   1.34 | > .999
      
      Variable predicted: neg_c_7
      Predictors contrasted: c172code
      p-value adjustment method: Holm (1979)

---

    Code
      print(estimate_contrasts(fit, c("c172code", "barthtot = [sd]"), backend = "marginaleffects",
      p_adjust = "holm"), table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      Level1         | Level2         | Difference |   SE |         95% CI | t(810) |      p
      --------------------------------------------------------------------------------------
      low, 64.79167  | low, 35.18764  |      -1.93 | 0.27 | [-2.45, -1.40] |  -7.20 | < .001
      low, 94.39569  | low, 35.18764  |      -3.85 | 0.54 | [-4.90, -2.80] |  -7.20 | < .001
      mid, 35.18764  | low, 35.18764  |      -0.22 | 0.42 | [-1.05,  0.60] |  -0.53 | > .999
      mid, 64.79167  | low, 35.18764  |      -1.76 | 0.39 | [-2.52, -0.99] |  -4.52 | < .001
      mid, 94.39569  | low, 35.18764  |      -3.29 | 0.42 | [-4.11, -2.47] |  -7.90 | < .001
      high, 35.18764 | low, 35.18764  |       0.22 | 0.56 | [-0.87,  1.31] |   0.40 | > .999
      high, 64.79167 | low, 35.18764  |      -1.21 | 0.46 | [-2.11, -0.31] |  -2.65 |  0.099
      high, 94.39569 | low, 35.18764  |      -2.64 | 0.56 | [-3.74, -1.55] |  -4.73 | < .001
      low, 94.39569  | low, 64.79167  |      -1.93 | 0.27 | [-2.45, -1.40] |  -7.20 | < .001
      mid, 35.18764  | low, 64.79167  |       1.70 | 0.35 | [ 1.01,  2.40] |   4.81 | < .001
      mid, 64.79167  | low, 64.79167  |       0.17 | 0.31 | [-0.45,  0.79] |   0.54 | > .999
      mid, 94.39569  | low, 64.79167  |      -1.36 | 0.35 | [-2.04, -0.68] |  -3.93 |  0.001
      high, 35.18764 | low, 64.79167  |       2.15 | 0.51 | [ 1.15,  3.14] |   4.24 | < .001
      high, 64.79167 | low, 64.79167  |       0.72 | 0.40 | [-0.06,  1.49] |   1.81 |  0.636
      high, 94.39569 | low, 64.79167  |      -0.72 | 0.51 | [-1.71,  0.28] |  -1.41 | > .999
      mid, 35.18764  | low, 94.39569  |       3.63 | 0.46 | [ 2.72,  4.54] |   7.81 | < .001
      mid, 64.79167  | low, 94.39569  |       2.10 | 0.43 | [ 1.24,  2.95] |   4.82 | < .001
      mid, 94.39569  | low, 94.39569  |       0.56 | 0.46 | [-0.34,  1.47] |   1.23 | > .999
      high, 35.18764 | low, 94.39569  |       4.07 | 0.59 | [ 2.92,  5.23] |   6.91 | < .001
      high, 64.79167 | low, 94.39569  |       2.64 | 0.50 | [ 1.67,  3.62] |   5.32 | < .001
      high, 94.39569 | low, 94.39569  |       1.21 | 0.59 | [ 0.05,  2.37] |   2.05 |  0.450
      mid, 64.79167  | mid, 35.18764  |      -1.53 | 0.16 | [-1.84, -1.23] |  -9.80 | < .001
      mid, 94.39569  | mid, 35.18764  |      -3.07 | 0.31 | [-3.68, -2.45] |  -9.80 | < .001
      high, 35.18764 | mid, 35.18764  |       0.45 | 0.49 | [-0.51,  1.40] |   0.92 | > .999
      high, 64.79167 | mid, 35.18764  |      -0.99 | 0.37 | [-1.71, -0.26] |  -2.67 |  0.099
      high, 94.39569 | mid, 35.18764  |      -2.42 | 0.49 | [-3.38, -1.46] |  -4.95 | < .001
      mid, 94.39569  | mid, 64.79167  |      -1.53 | 0.16 | [-1.84, -1.23] |  -9.80 | < .001
      high, 35.18764 | mid, 64.79167  |       1.98 | 0.46 | [ 1.08,  2.88] |   4.32 | < .001
      high, 64.79167 | mid, 64.79167  |       0.55 | 0.33 | [-0.10,  1.19] |   1.65 |  0.789
      high, 94.39569 | mid, 64.79167  |      -0.89 | 0.46 | [-1.79,  0.02] |  -1.93 |  0.545
      high, 35.18764 | mid, 94.39569  |       3.51 | 0.48 | [ 2.57,  4.46] |   7.30 | < .001
      high, 64.79167 | mid, 94.39569  |       2.08 | 0.36 | [ 1.37,  2.79] |   5.74 | < .001
      high, 94.39569 | mid, 94.39569  |       0.65 | 0.48 | [-0.30,  1.60] |   1.34 | > .999
      high, 64.79167 | high, 35.18764 |      -1.43 | 0.32 | [-2.06, -0.81] |  -4.50 | < .001
      high, 94.39569 | high, 35.18764 |      -2.86 | 0.64 | [-4.11, -1.61] |  -4.50 | < .001
      high, 94.39569 | high, 64.79167 |      -1.43 | 0.32 | [-2.06, -0.81] |  -4.50 | < .001
      
      Variable predicted: neg_c_7
      Predictors contrasted: c172code, barthtot = [sd]
      p-value adjustment method: Holm (1979)

---

    Code
      print(estimate_contrasts(fit, "c172code", "barthtot = [sd]", backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      Level1 | Level2 | barthtot | Difference |   SE |        95% CI | t(810) |     p
      -------------------------------------------------------------------------------
      mid    | low    |    35.19 |      -0.22 | 0.42 | [-1.05, 0.60] |  -0.53 | 0.596
      high   | low    |    35.19 |       0.22 | 0.56 | [-0.87, 1.31] |   0.40 | 0.691
      high   | mid    |    35.19 |       0.45 | 0.49 | [-0.51, 1.40] |   0.92 | 0.360
      mid    | low    |    64.79 |       0.17 | 0.31 | [-0.45, 0.79] |   0.54 | 0.589
      high   | low    |    64.79 |       0.72 | 0.40 | [-0.06, 1.49] |   1.81 | 0.071
      high   | mid    |    64.79 |       0.55 | 0.33 | [-0.10, 1.19] |   1.65 | 0.099
      mid    | low    |    94.40 |       0.56 | 0.46 | [-0.34, 1.47] |   1.23 | 0.221
      high   | low    |    94.40 |       1.21 | 0.59 | [ 0.05, 2.37] |   2.05 | 0.041
      high   | mid    |    94.40 |       0.65 | 0.48 | [-0.30, 1.60] |   1.34 | 0.181
      
      Variable predicted: neg_c_7
      Predictors contrasted: c172code
      p-values are uncorrected.

---

    Code
      print(estimate_contrasts(fit, c("c172code", "barthtot = [sd]"), backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      Level1         | Level2         | Difference |   SE |         95% CI | t(810) |      p
      --------------------------------------------------------------------------------------
      low, 64.79167  | low, 35.18764  |      -1.93 | 0.27 | [-2.45, -1.40] |  -7.20 | < .001
      low, 94.39569  | low, 35.18764  |      -3.85 | 0.54 | [-4.90, -2.80] |  -7.20 | < .001
      mid, 35.18764  | low, 35.18764  |      -0.22 | 0.42 | [-1.05,  0.60] |  -0.53 |  0.596
      mid, 64.79167  | low, 35.18764  |      -1.76 | 0.39 | [-2.52, -0.99] |  -4.52 | < .001
      mid, 94.39569  | low, 35.18764  |      -3.29 | 0.42 | [-4.11, -2.47] |  -7.90 | < .001
      high, 35.18764 | low, 35.18764  |       0.22 | 0.56 | [-0.87,  1.31] |   0.40 |  0.691
      high, 64.79167 | low, 35.18764  |      -1.21 | 0.46 | [-2.11, -0.31] |  -2.65 |  0.008
      high, 94.39569 | low, 35.18764  |      -2.64 | 0.56 | [-3.74, -1.55] |  -4.73 | < .001
      low, 94.39569  | low, 64.79167  |      -1.93 | 0.27 | [-2.45, -1.40] |  -7.20 | < .001
      mid, 35.18764  | low, 64.79167  |       1.70 | 0.35 | [ 1.01,  2.40] |   4.81 | < .001
      mid, 64.79167  | low, 64.79167  |       0.17 | 0.31 | [-0.45,  0.79] |   0.54 |  0.589
      mid, 94.39569  | low, 64.79167  |      -1.36 | 0.35 | [-2.04, -0.68] |  -3.93 | < .001
      high, 35.18764 | low, 64.79167  |       2.15 | 0.51 | [ 1.15,  3.14] |   4.24 | < .001
      high, 64.79167 | low, 64.79167  |       0.72 | 0.40 | [-0.06,  1.49] |   1.81 |  0.071
      high, 94.39569 | low, 64.79167  |      -0.72 | 0.51 | [-1.71,  0.28] |  -1.41 |  0.160
      mid, 35.18764  | low, 94.39569  |       3.63 | 0.46 | [ 2.72,  4.54] |   7.81 | < .001
      mid, 64.79167  | low, 94.39569  |       2.10 | 0.43 | [ 1.24,  2.95] |   4.82 | < .001
      mid, 94.39569  | low, 94.39569  |       0.56 | 0.46 | [-0.34,  1.47] |   1.23 |  0.221
      high, 35.18764 | low, 94.39569  |       4.07 | 0.59 | [ 2.92,  5.23] |   6.91 | < .001
      high, 64.79167 | low, 94.39569  |       2.64 | 0.50 | [ 1.67,  3.62] |   5.32 | < .001
      high, 94.39569 | low, 94.39569  |       1.21 | 0.59 | [ 0.05,  2.37] |   2.05 |  0.041
      mid, 64.79167  | mid, 35.18764  |      -1.53 | 0.16 | [-1.84, -1.23] |  -9.80 | < .001
      mid, 94.39569  | mid, 35.18764  |      -3.07 | 0.31 | [-3.68, -2.45] |  -9.80 | < .001
      high, 35.18764 | mid, 35.18764  |       0.45 | 0.49 | [-0.51,  1.40] |   0.92 |  0.360
      high, 64.79167 | mid, 35.18764  |      -0.99 | 0.37 | [-1.71, -0.26] |  -2.67 |  0.008
      high, 94.39569 | mid, 35.18764  |      -2.42 | 0.49 | [-3.38, -1.46] |  -4.95 | < .001
      mid, 94.39569  | mid, 64.79167  |      -1.53 | 0.16 | [-1.84, -1.23] |  -9.80 | < .001
      high, 35.18764 | mid, 64.79167  |       1.98 | 0.46 | [ 1.08,  2.88] |   4.32 | < .001
      high, 64.79167 | mid, 64.79167  |       0.55 | 0.33 | [-0.10,  1.19] |   1.65 |  0.099
      high, 94.39569 | mid, 64.79167  |      -0.89 | 0.46 | [-1.79,  0.02] |  -1.93 |  0.055
      high, 35.18764 | mid, 94.39569  |       3.51 | 0.48 | [ 2.57,  4.46] |   7.30 | < .001
      high, 64.79167 | mid, 94.39569  |       2.08 | 0.36 | [ 1.37,  2.79] |   5.74 | < .001
      high, 94.39569 | mid, 94.39569  |       0.65 | 0.48 | [-0.30,  1.60] |   1.34 |  0.181
      high, 64.79167 | high, 35.18764 |      -1.43 | 0.32 | [-2.06, -0.81] |  -4.50 | < .001
      high, 94.39569 | high, 35.18764 |      -2.86 | 0.64 | [-4.11, -1.61] |  -4.50 | < .001
      high, 94.39569 | high, 64.79167 |      -1.43 | 0.32 | [-2.06, -0.81] |  -4.50 | < .001
      
      Variable predicted: neg_c_7
      Predictors contrasted: c172code, barthtot = [sd]
      p-values are uncorrected.

# estimate_means - by is list

    Code
      print(estimate_means(fit, list(c172code = c("low", "high"), c161sex = c(
        "Female", "Male")), backend = "marginaleffects"), table_width = Inf,
      zap_small = TRUE)
    Output
      Estimated Marginal Means
      
      c172code | c161sex |  Mean |   SE |         95% CI | t(825)
      -----------------------------------------------------------
      low      | Female  | 12.13 | 0.33 | [11.48, 12.78] |  36.44
      high     | Female  | 12.73 | 0.38 | [11.98, 13.48] |  33.31
      low      | Male    | 11.09 | 0.61 | [ 9.89, 12.28] |  18.25
      high     | Male    | 11.77 | 0.58 | [10.64, 12.90] |  20.45
      
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
      low      | Female  | 12.13 | 0.33 | [11.48, 12.78] |  36.44
      high     | Female  | 12.73 | 0.38 | [11.98, 13.48] |  33.31
      low      | Male    | 11.09 | 0.61 | [ 9.89, 12.28] |  18.25
      high     | Male    | 11.77 | 0.58 | [10.64, 12.90] |  20.45
      
      Variable predicted: neg_c_7
      Predictors modulated: c172code = c('low', 'high'), c161sex = c('Female', 'Male')
      Predictors averaged: e16sex

# estimate_epectation - don't print empty RE columns

    Code
      print(estimate_expectation(m, by = "spp", predict = "conditional"), zap_small = TRUE)
    Output
      Model-based Predictions
      
      spp   | Predicted |   SE |           CI
      ---------------------------------------
      GP    |      0.73 | 0.21 | [0.32, 1.14]
      PR    |      0.42 | 0.16 | [0.11, 0.72]
      DM    |      0.94 | 0.25 | [0.45, 1.43]
      EC-A  |      0.60 | 0.19 | [0.24, 0.96]
      EC-L  |      1.42 | 0.37 | [0.69, 2.14]
      DES-L |      1.34 | 0.36 | [0.63, 2.04]
      DF    |      0.78 | 0.21 | [0.37, 1.19]
      
      Variable predicted: count
      Predictors modulated: spp
      Predictors controlled: mined (yes)
      Predictions are on the conditional-scale.

# print - layouts and include data grid

    Code
      print(out)
    Output
      Estimated Marginal Means
      
      Species    | Mean |   SE |       95% CI | t(147)
      ------------------------------------------------
      setosa     | 1.46 | 0.06 | [1.34, 1.58] |  24.02
      versicolor | 4.26 | 0.06 | [4.14, 4.38] |  70.00
      virginica  | 5.55 | 0.06 | [5.43, 5.67] |  91.23
      
      Variable predicted: Petal.Length
      Predictors modulated: Species

---

    Code
      print(out, select = "minimal")
    Output
      Estimated Marginal Means
      
      Species    |         Mean (CI)
      ------------------------------
      setosa     | 1.46 (1.34, 1.58)
      versicolor | 4.26 (4.14, 4.38)
      virginica  | 5.55 (5.43, 5.67)
      
      Variable predicted: Petal.Length
      Predictors modulated: Species

---

    Code
      print(out, select = "minimal")
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     |   Difference (CI) |      p
      ----------------------------------------------------
      versicolor | setosa     | 2.80 (2.63, 2.97) | <0.001
      virginica  | setosa     | 4.09 (3.92, 4.26) | <0.001
      virginica  | versicolor | 1.29 (1.12, 1.46) | <0.001
      
      Variable predicted: Petal.Length
      Predictors contrasted: Species
      p-values are uncorrected.

---

    Code
      print(out, select = "{estimate}{stars}|{ci}")
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |         CI
      -------------------------------------------------
      versicolor | setosa     |    2.80*** | 2.63, 2.97
      virginica  | setosa     |    4.09*** | 3.92, 4.26
      virginica  | versicolor |    1.29*** | 1.12, 1.46
      
      Variable predicted: Petal.Length
      Predictors contrasted: Species
      p-values are uncorrected.

---

    Code
      print(estimate_relation(m, by = "qsec"))
    Output
      Model-based Predictions
      
      qsec  | Predicted |   SE |       95% CI
      ---------------------------------------
      14.50 |      2.80 | 0.19 | [2.41, 3.18]
      15.43 |      2.91 | 0.15 | [2.62, 3.21]
      16.37 |      3.03 | 0.11 | [2.81, 3.26]
      17.30 |      3.15 | 0.09 | [2.97, 3.32]
      18.23 |      3.27 | 0.08 | [3.10, 3.44]
      19.17 |      3.38 | 0.10 | [3.17, 3.60]
      20.10 |      3.50 | 0.14 | [3.21, 3.78]
      21.03 |      3.62 | 0.18 | [3.25, 3.98]
      21.97 |      3.73 | 0.22 | [3.28, 4.19]
      22.90 |      3.85 | 0.27 | [3.30, 4.40]
      
      Variable predicted: wt
      Predictors modulated: qsec
      Predictors controlled: mpg (20)

---

    Code
      print(estimate_relation(m, by = "qsec"), include_grid = TRUE)
    Output
      Model-based Predictions
      
      qsec  |   mpg | Predicted |   SE |       95% CI
      -----------------------------------------------
      14.50 | 20.09 |      2.80 | 0.19 | [2.41, 3.18]
      15.43 | 20.09 |      2.91 | 0.15 | [2.62, 3.21]
      16.37 | 20.09 |      3.03 | 0.11 | [2.81, 3.26]
      17.30 | 20.09 |      3.15 | 0.09 | [2.97, 3.32]
      18.23 | 20.09 |      3.27 | 0.08 | [3.10, 3.44]
      19.17 | 20.09 |      3.38 | 0.10 | [3.17, 3.60]
      20.10 | 20.09 |      3.50 | 0.14 | [3.21, 3.78]
      21.03 | 20.09 |      3.62 | 0.18 | [3.25, 3.98]
      21.97 | 20.09 |      3.73 | 0.22 | [3.28, 4.19]
      22.90 | 20.09 |      3.85 | 0.27 | [3.30, 4.40]
      
      Variable predicted: wt
      Predictors modulated: qsec
      Predictors controlled: mpg (20)

