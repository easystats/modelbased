# estimate_contrasts - Frequentist, Three factors

    Code
      print(estimate_contrasts(model, contrast = c("vs", "am"), by = "gear", backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | gear | Difference |   SE |         95% CI | t(25) |     p
      -----------------------------------------------------------------------------------
      vs 0, am 1 | vs 0, am 0 | 3    |       6.98 | 2.33 | [ 2.17, 11.79] |  2.99 | 0.006
      vs 1, am 0 | vs 0, am 1 | 4    |       0.05 | 3.13 | [-6.40,  6.50] |  0.02 | 0.987
      vs 1, am 1 | vs 1, am 0 | 5    |       6.98 | 2.33 | [ 2.17, 11.79] |  2.99 | 0.006
      
      Variable predicted: mpg
      Predictors contrasted: vs, am
      p-values are uncorrected.

# estimate_contrasts - marginaleffects, comparisons, validate against predict

    Code
      print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects",
      p_adjust = "none"), zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1             | Level2            | Difference |   SE |         95% CI | t(113) |      p
      ---------------------------------------------------------------------------------------------
      morning, control   | morning, coffee   |      -5.78 | 1.99 | [-9.73, -1.83] |  -2.90 |  0.004
      noon, coffee       | morning, coffee   |      -1.93 | 1.99 | [-5.88,  2.02] |  -0.97 |  0.336
      noon, control      | morning, coffee   |       0.00 | 1.99 | [-3.95,  3.95] |   0.00 | > .999
      afternoon, coffee  | morning, coffee   |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
      afternoon, control | morning, coffee   |       0.00 | 1.99 | [-3.95,  3.95] |   0.00 | > .999
      noon, coffee       | morning, control  |       3.86 | 1.99 | [-0.09,  7.81] |   1.93 |  0.056
      noon, control      | morning, control  |       5.78 | 1.99 | [ 1.83,  9.73] |   2.90 |  0.004
      afternoon, coffee  | morning, control  |       7.71 | 1.99 | [ 3.76, 11.66] |   3.87 | < .001
      afternoon, control | morning, control  |       5.78 | 1.99 | [ 1.83,  9.73] |   2.90 |  0.004
      noon, control      | noon, coffee      |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
      afternoon, coffee  | noon, coffee      |       3.86 | 1.99 | [-0.09,  7.81] |   1.93 |  0.056
      afternoon, control | noon, coffee      |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
      afternoon, coffee  | noon, control     |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
      afternoon, control | noon, control     |       0.00 | 1.99 | [-3.95,  3.95] |   0.00 | > .999
      afternoon, control | afternoon, coffee |      -1.93 | 1.99 | [-5.88,  2.02] |  -0.97 |  0.336
      
      Variable predicted: alertness
      Predictors contrasted: time, coffee
      Predictors averaged: sex
      p-values are uncorrected.

---

    Code
      print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects",
      p_adjust = "none", comparison = ratio ~ reference | coffee), zap_small = TRUE,
      table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1    | Level2  | coffee  | Ratio |   SE |       95% CI | t(113) |      p
      -----------------------------------------------------------------------------
      noon      | morning | coffee  |  0.89 | 0.11 | [0.67, 1.11] |   8.08 | < .001
      afternoon | morning | coffee  |  1.11 | 0.12 | [0.87, 1.36] |   9.05 | < .001
      noon      | morning | control |  1.51 | 0.22 | [1.06, 1.95] |   6.73 | < .001
      afternoon | morning | control |  1.51 | 0.22 | [1.06, 1.95] |   6.73 | < .001
      
      Variable predicted: alertness
      Predictors contrasted: time
      Predictors averaged: sex
      p-values are uncorrected.

---

    Code
      print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects",
      p_adjust = "none", comparison = "(b2-b1)=(b4-b3)"), zap_small = TRUE,
      table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Parameter   | Difference |   SE |        95% CI | t(113) |     p
      ----------------------------------------------------------------
      b2-b1=b4-b3 |       5.78 | 2.82 | [0.20, 11.37] |   2.05 | 0.043
      
      Variable predicted: alertness
      Predictors contrasted: time, coffee
      Predictors averaged: sex
      p-values are uncorrected.
      Parameters:
      b2 = time [noon], coffee [coffee]
      b1 = time [morning], coffee [coffee]
      b4 = time [morning], coffee [control]
      b3 = time [afternoon], coffee [coffee]

---

    Code
      print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects",
      p_adjust = "none", comparison = "b5=b3"), zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Parameter | Difference |   SE |        95% CI | t(113) |     p
      --------------------------------------------------------------
      b5=b3     |      -1.93 | 1.99 | [-5.88, 2.02] |  -0.97 | 0.336
      
      Variable predicted: alertness
      Predictors contrasted: time, coffee
      Predictors averaged: sex
      p-values are uncorrected.
      Parameters:
      b5 = time [noon], coffee [control]
      b3 = time [afternoon], coffee [coffee]

---

    Code
      estimate_contrasts(fit, c("e42dep", "c172code"), comparison = "b6-b3=0",
      backend = "marginaleffects")
    Output
      Marginal Contrasts Analysis
      
      Parameter | Difference |   SE |        95% CI | t(800) |     p
      --------------------------------------------------------------
      b6-b3=0   |      -0.34 | 0.56 | [-1.44, 0.77] |  -0.60 | 0.552
      
      Variable predicted: neg_c_7
      Predictors contrasted: e42dep, c172code
      Predictors averaged: c12hour (42), barthtot (65), c161sex
      p-values are uncorrected.
      Parameters:
      b6 = e42dep [slightly dependent], c172code [intermediate level of education]
      b3 = e42dep [moderately dependent], c172code [low level of education]

# estimate_contrasts - marginaleffects vs emmeans

    Code
      estimate_contrasts(model, backend = "emmeans")
    Message
      No variable was specified for contrast estimation. Selecting `contrast =
        "Species"`.
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |         95% CI |   SE |  df |     z |      p
      -----------------------------------------------------------------------------------
      setosa     | versicolor |      -0.68 | [-0.82, -0.54] | 0.07 | Inf | -9.27 | < .001
      setosa     | virginica  |      -0.50 | [-0.67, -0.33] | 0.08 | Inf | -5.90 | < .001
      versicolor | virginica  |       0.18 | [ 0.01,  0.35] | 0.08 | Inf |  2.12 |  0.034
      
      Variable predicted: y
      Predictors contrasted: Species
      p-values are uncorrected.
      Contrasts are on the response-scale.

---

    Code
      estimate_contrasts(model, backend = "marginaleffects")
    Message
      We selected `contrast=c("Species")`.
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |   SE |         95% CI |     z |      p
      -----------------------------------------------------------------------------
      versicolor | setosa     |       0.68 | 0.07 | [ 0.54,  0.82] |  9.27 | < .001
      virginica  | setosa     |       0.50 | 0.08 | [ 0.33,  0.67] |  5.90 | < .001
      virginica  | versicolor |      -0.18 | 0.08 | [-0.35, -0.01] | -2.12 |  0.034
      
      Variable predicted: y
      Predictors contrasted: Species
      p-values are uncorrected.
      Contrasts are on the response-scale.

---

    Code
      estimate_contrasts(model, backend = "emmeans", p_adjust = "holm")
    Message
      No variable was specified for contrast estimation. Selecting `contrast =
        "Species"`.
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |         95% CI |   SE |  df |     z |      p
      -----------------------------------------------------------------------------------
      setosa     | versicolor |      -0.68 | [-0.86, -0.50] | 0.07 | Inf | -9.27 | < .001
      setosa     | virginica  |      -0.50 | [-0.70, -0.30] | 0.08 | Inf | -5.90 | < .001
      versicolor | virginica  |       0.18 | [-0.02,  0.38] | 0.08 | Inf |  2.12 |  0.034
      
      Variable predicted: y
      Predictors contrasted: Species
      p-value adjustment method: Holm (1979)
      Contrasts are on the response-scale.

---

    Code
      estimate_contrasts(model, backend = "marginaleffects", p_adjust = "holm")
    Message
      We selected `contrast=c("Species")`.
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |   SE |         95% CI |     z |      p
      -----------------------------------------------------------------------------
      versicolor | setosa     |       0.68 | 0.07 | [ 0.54,  0.82] |  9.27 | < .001
      virginica  | setosa     |       0.50 | 0.08 | [ 0.33,  0.67] |  5.90 | < .001
      virginica  | versicolor |      -0.18 | 0.08 | [-0.35, -0.01] | -2.12 |  0.034
      
      Variable predicted: y
      Predictors contrasted: Species
      p-value adjustment method: Holm (1979)
      Contrasts are on the response-scale.

# estimate_contrasts - filtering works

    Code
      print(estimate_contrasts(fit, "c172code", backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      Level1 | Level2 | Difference |   SE |        95% CI | t(827) |     p
      --------------------------------------------------------------------
      mid    | low    |      -0.09 | 0.34 | [-0.76, 0.58] |  -0.25 | 0.802
      high   | low    |       0.61 | 0.43 | [-0.24, 1.45] |   1.40 | 0.162
      high   | mid    |       0.69 | 0.36 | [-0.02, 1.40] |   1.92 | 0.055
      
      Variable predicted: neg_c_7
      Predictors contrasted: c172code
      Predictors averaged: e16sex, c161sex
      p-values are uncorrected.

---

    Code
      print(estimate_contrasts(fit, c("c161sex", "c172code"), backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      Level1       | Level2      | Difference |   SE |        95% CI | t(825) |     p
      -------------------------------------------------------------------------------
      Male, mid    | Male, low   |       0.29 | 0.71 | [-1.10, 1.68] |   0.41 | 0.684
      Male, high   | Male, low   |       0.69 | 0.83 | [-0.95, 2.32] |   0.82 | 0.410
      Female, low  | Male, low   |       1.05 | 0.69 | [-0.31, 2.40] |   1.51 | 0.131
      Female, mid  | Male, low   |       0.85 | 0.64 | [-0.40, 2.10] |   1.33 | 0.183
      Female, high | Male, low   |       1.65 | 0.71 | [ 0.24, 3.05] |   2.30 | 0.022
      Male, high   | Male, mid   |       0.40 | 0.68 | [-0.94, 1.73] |   0.59 | 0.558
      Female, low  | Male, mid   |       0.76 | 0.50 | [-0.22, 1.73] |   1.52 | 0.129
      Female, mid  | Male, mid   |       0.56 | 0.42 | [-0.26, 1.38] |   1.35 | 0.178
      Female, high | Male, mid   |       1.36 | 0.53 | [ 0.32, 2.39] |   2.58 | 0.010
      Female, low  | Male, high  |       0.36 | 0.66 | [-0.95, 1.66] |   0.54 | 0.589
      Female, mid  | Male, high  |       0.16 | 0.61 | [-1.03, 1.35] |   0.27 | 0.789
      Female, high | Male, high  |       0.96 | 0.69 | [-0.39, 2.30] |   1.40 | 0.163
      Female, mid  | Female, low |      -0.20 | 0.39 | [-0.96, 0.57] |  -0.51 | 0.613
      Female, high | Female, low |       0.60 | 0.51 | [-0.39, 1.59] |   1.18 | 0.236
      Female, high | Female, mid |       0.80 | 0.43 | [-0.04, 1.63] |   1.87 | 0.061
      
      Variable predicted: neg_c_7
      Predictors contrasted: c161sex, c172code
      Predictors averaged: e16sex
      p-values are uncorrected.

---

    Code
      print(estimate_contrasts(fit, "c161sex", "c172code", backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      Level1 | Level2 | c172code | Difference |   SE |        95% CI | t(825) |     p
      -------------------------------------------------------------------------------
      Female | Male   | low      |       1.05 | 0.69 | [-0.31, 2.40] |   1.51 | 0.131
      Female | Male   | mid      |       0.56 | 0.42 | [-0.26, 1.38] |   1.35 | 0.178
      Female | Male   | high     |       0.96 | 0.69 | [-0.39, 2.30] |   1.40 | 0.163
      
      Variable predicted: neg_c_7
      Predictors contrasted: c161sex
      Predictors averaged: e16sex
      p-values are uncorrected.

---

    Code
      print(estimate_slopes(fit, "barthtot", backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Estimated Marginal Effects
      
      Slope |   SE |         95% CI |      t |      p
      -----------------------------------------------
      -0.05 | 0.00 | [-0.06, -0.05] | -12.77 | < .001
      
      Marginal effects estimated for barthtot
      Type of slope was dY/dX

---

    Code
      print(estimate_slopes(fit, "barthtot", by = "c172code", backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Estimated Marginal Effects
      
      c172code | Slope |   SE |         95% CI |     t |      p
      ---------------------------------------------------------
      low      | -0.06 | 0.01 | [-0.08, -0.05] | -7.08 | < .001
      mid      | -0.05 | 0.01 | [-0.06, -0.04] | -9.82 | < .001
      high     | -0.05 | 0.01 | [-0.07, -0.03] | -4.51 | < .001
      
      Marginal effects estimated for barthtot
      Type of slope was dY/dX

---

    Code
      print(estimate_contrasts(fit, "barthtot", "c172code", backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      Level1 | Level2 | Difference |   SE |        95% CI |    t |     p
      ------------------------------------------------------------------
      mid    | low    |       0.01 | 0.01 | [-0.01, 0.03] | 1.17 | 0.243
      high   | low    |       0.02 | 0.01 | [-0.01, 0.04] | 1.10 | 0.271
      high   | mid    |       0.00 | 0.01 | [-0.02, 0.03] | 0.27 | 0.786
      
      Variable predicted: neg_c_7
      Predictors contrasted: barthtot
      Predictors averaged: e16sex, barthtot (65)
      p-values are uncorrected.

---

    Code
      print(estimate_contrasts(fit, "barthtot", c("c172code", "e16sex"), backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      Level1       | Level2      | Difference |   SE |        95% CI |     t |     p
      ------------------------------------------------------------------------------
      low, female  | low, male   |      -0.01 | 0.02 | [-0.04, 0.03] | -0.35 | 0.729
      mid, male    | low, male   |      -0.01 | 0.02 | [-0.04, 0.02] | -0.46 | 0.648
      mid, female  | low, male   |       0.02 | 0.01 | [-0.01, 0.05] |  1.24 | 0.216
      high, male   | low, male   |       0.00 | 0.02 | [-0.05, 0.04] | -0.16 | 0.876
      high, female | low, male   |       0.02 | 0.02 | [-0.01, 0.06] |  1.19 | 0.236
      mid, male    | low, female |       0.00 | 0.02 | [-0.03, 0.03] | -0.08 | 0.940
      mid, female  | low, female |       0.02 | 0.01 | [ 0.00, 0.05] |  1.76 | 0.079
      high, male   | low, female |       0.00 | 0.02 | [-0.04, 0.05] |  0.12 | 0.908
      high, female | low, female |       0.03 | 0.02 | [-0.01, 0.06] |  1.58 | 0.115
      mid, female  | mid, male   |       0.03 | 0.01 | [ 0.00, 0.05] |  2.24 | 0.025
      high, male   | mid, male   |       0.00 | 0.02 | [-0.04, 0.05] |  0.18 | 0.859
      high, female | mid, male   |       0.03 | 0.02 | [ 0.00, 0.06] |  1.83 | 0.067
      high, male   | mid, female |      -0.02 | 0.02 | [-0.06, 0.02] | -1.08 | 0.280
      high, female | mid, female |       0.00 | 0.01 | [-0.02, 0.03] |  0.26 | 0.792
      high, female | high, male  |       0.03 | 0.02 | [-0.02, 0.07] |  1.11 | 0.268
      
      Variable predicted: neg_c_7
      Predictors contrasted: barthtot
      Predictors averaged: barthtot (65)
      p-values are uncorrected.

# estimate_contrasts - simple contrasts and with - in levels works

    Code
      print(estimate_contrasts(model, "Species", backend = "marginaleffects"),
      table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |   SE |       95% CI | t(146) |      p
      ----------------------------------------------------------------------------
      versicolor | setosa     |       1.46 | 0.11 | [1.24, 1.68] |  13.01 | < .001
      virginica  | setosa     |       1.95 | 0.10 | [1.75, 2.14] |  19.47 | < .001
      virginica  | versicolor |       0.49 | 0.09 | [0.31, 0.67] |   5.41 | < .001
      
      Variable predicted: Sepal.Length
      Predictors contrasted: Species
      Predictors averaged: Sepal.Width (3.1)
      p-values are uncorrected.

---

    Code
      print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1             | Level2            | Difference |   SE |         95% CI | t(113) |      p
      ---------------------------------------------------------------------------------------------
      morning, control   | morning, coffee   |      -5.78 | 1.99 | [-9.73, -1.83] |  -2.90 |  0.004
      noon, coffee       | morning, coffee   |      -1.93 | 1.99 | [-5.88,  2.02] |  -0.97 |  0.336
      noon, control      | morning, coffee   |       0.00 | 1.99 | [-3.95,  3.95] |   0.00 | > .999
      afternoon, coffee  | morning, coffee   |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
      afternoon, control | morning, coffee   |       0.00 | 1.99 | [-3.95,  3.95] |   0.00 | > .999
      noon, coffee       | morning, control  |       3.86 | 1.99 | [-0.09,  7.81] |   1.93 |  0.056
      noon, control      | morning, control  |       5.78 | 1.99 | [ 1.83,  9.73] |   2.90 |  0.004
      afternoon, coffee  | morning, control  |       7.71 | 1.99 | [ 3.76, 11.66] |   3.87 | < .001
      afternoon, control | morning, control  |       5.78 | 1.99 | [ 1.83,  9.73] |   2.90 |  0.004
      noon, control      | noon, coffee      |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
      afternoon, coffee  | noon, coffee      |       3.86 | 1.99 | [-0.09,  7.81] |   1.93 |  0.056
      afternoon, control | noon, coffee      |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
      afternoon, coffee  | noon, control     |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
      afternoon, control | noon, control     |       0.00 | 1.99 | [-3.95,  3.95] |   0.00 | > .999
      afternoon, control | afternoon, coffee |      -1.93 | 1.99 | [-5.88,  2.02] |  -0.97 |  0.336
      
      Variable predicted: alertness
      Predictors contrasted: time, coffee
      Predictors averaged: sex
      p-values are uncorrected.

---

    Code
      print(estimate_contrasts(m, contrast = "time", by = "coffee", backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1    | Level2  | coffee  | Difference |   SE |        95% CI | t(113) |      p
      -----------------------------------------------------------------------------------
      noon      | morning | coffee  |      -1.93 | 1.99 | [-5.88, 2.02] |  -0.97 |  0.336
      afternoon | morning | coffee  |       1.93 | 1.99 | [-2.02, 5.88] |   0.97 |  0.336
      afternoon | noon    | coffee  |       3.86 | 1.99 | [-0.09, 7.81] |   1.93 |  0.056
      noon      | morning | control |       5.78 | 1.99 | [ 1.83, 9.73] |   2.90 |  0.004
      afternoon | morning | control |       5.78 | 1.99 | [ 1.83, 9.73] |   2.90 |  0.004
      afternoon | noon    | control |       0.00 | 1.99 | [-3.95, 3.95] |   0.00 | > .999
      
      Variable predicted: alertness
      Predictors contrasted: time
      Predictors averaged: sex
      p-values are uncorrected.

---

    Code
      print(estimate_contrasts(model, contrast = c("mined", "spp"), backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |   SE |         95% CI |     z |      p
      -----------------------------------------------------------------------------
      yes, PR    | yes, GP    |       0.06 | 0.05 | [-0.04,  0.16] |  1.10 |  0.271
      yes, DM    | yes, GP    |       0.32 | 0.11 | [ 0.10,  0.54] |  2.89 |  0.004
      yes, EC-A  | yes, GP    |       0.04 | 0.05 | [-0.05,  0.13] |  0.80 |  0.421
      yes, EC-L  | yes, GP    |       0.17 | 0.08 | [ 0.02,  0.32] |  2.19 |  0.028
      yes, DES-L | yes, GP    |       0.43 | 0.14 | [ 0.17,  0.70] |  3.19 |  0.001
      yes, DF    | yes, GP    |       0.43 | 0.14 | [ 0.17,  0.70] |  3.19 |  0.001
      no, GP     | yes, GP    |       2.59 | 0.55 | [ 1.50,  3.67] |  4.67 | < .001
      no, PR     | yes, GP    |       0.51 | 0.16 | [ 0.20,  0.82] |  3.21 |  0.001
      no, DM     | yes, GP    |       2.86 | 0.61 | [ 1.68,  4.05] |  4.73 | < .001
      no, EC-A   | yes, GP    |       1.10 | 0.27 | [ 0.57,  1.64] |  4.03 | < .001
      no, EC-L   | yes, GP    |       4.67 | 0.94 | [ 2.82,  6.52] |  4.95 | < .001
      no, DES-L  | yes, GP    |       4.62 | 0.93 | [ 2.79,  6.45] |  4.95 | < .001
      no, DF     | yes, GP    |       2.24 | 0.49 | [ 1.28,  3.20] |  4.58 | < .001
      yes, DM    | yes, PR    |       0.26 | 0.11 | [ 0.05,  0.48] |  2.43 |  0.015
      yes, EC-A  | yes, PR    |      -0.02 | 0.06 | [-0.13,  0.09] | -0.33 |  0.740
      yes, EC-L  | yes, PR    |       0.11 | 0.08 | [-0.04,  0.27] |  1.43 |  0.154
      yes, DES-L | yes, PR    |       0.38 | 0.13 | [ 0.12,  0.64] |  2.86 |  0.004
      yes, DF    | yes, PR    |       0.38 | 0.13 | [ 0.12,  0.64] |  2.86 |  0.004
      no, GP     | yes, PR    |       2.53 | 0.56 | [ 1.44,  3.62] |  4.54 | < .001
      no, PR     | yes, PR    |       0.45 | 0.16 | [ 0.13,  0.77] |  2.75 |  0.006
      no, DM     | yes, PR    |       2.80 | 0.61 | [ 1.61,  4.00] |  4.61 | < .001
      no, EC-A   | yes, PR    |       1.05 | 0.28 | [ 0.50,  1.59] |  3.76 | < .001
      no, EC-L   | yes, PR    |       4.61 | 0.95 | [ 2.76,  6.47] |  4.87 | < .001
      no, DES-L  | yes, PR    |       4.56 | 0.94 | [ 2.73,  6.40] |  4.87 | < .001
      no, DF     | yes, PR    |       2.18 | 0.49 | [ 1.22,  3.15] |  4.44 | < .001
      yes, EC-A  | yes, DM    |      -0.28 | 0.11 | [-0.50, -0.07] | -2.59 |  0.010
      yes, EC-L  | yes, DM    |      -0.15 | 0.11 | [-0.36,  0.06] | -1.39 |  0.164
      yes, DES-L | yes, DM    |       0.11 | 0.13 | [-0.14,  0.36] |  0.89 |  0.375
      yes, DF    | yes, DM    |       0.11 | 0.13 | [-0.14,  0.36] |  0.89 |  0.375
      no, GP     | yes, DM    |       2.27 | 0.58 | [ 1.14,  3.40] |  3.93 | < .001
      no, PR     | yes, DM    |       0.19 | 0.20 | [-0.21,  0.58] |  0.93 |  0.352
      no, DM     | yes, DM    |       2.54 | 0.63 | [ 1.31,  3.77] |  4.04 | < .001
      no, EC-A   | yes, DM    |       0.78 | 0.31 | [ 0.18,  1.38] |  2.56 |  0.011
      no, EC-L   | yes, DM    |       4.35 | 0.96 | [ 2.46,  6.24] |  4.51 | < .001
      no, DES-L  | yes, DM    |       4.30 | 0.95 | [ 2.43,  6.17] |  4.51 | < .001
      no, DF     | yes, DM    |       1.92 | 0.51 | [ 0.91,  2.93] |  3.74 | < .001
      yes, EC-L  | yes, EC-A  |       0.13 | 0.08 | [-0.02,  0.29] |  1.68 |  0.093
      yes, DES-L | yes, EC-A  |       0.40 | 0.13 | [ 0.14,  0.66] |  2.97 |  0.003
      yes, DF    | yes, EC-A  |       0.40 | 0.13 | [ 0.14,  0.66] |  2.97 |  0.003
      no, GP     | yes, EC-A  |       2.55 | 0.56 | [ 1.46,  3.64] |  4.58 | < .001
      no, PR     | yes, EC-A  |       0.47 | 0.16 | [ 0.15,  0.79] |  2.90 |  0.004
      no, DM     | yes, EC-A  |       2.82 | 0.61 | [ 1.63,  4.01] |  4.65 | < .001
      no, EC-A   | yes, EC-A  |       1.06 | 0.28 | [ 0.52,  1.61] |  3.85 | < .001
      no, EC-L   | yes, EC-A  |       4.63 | 0.95 | [ 2.78,  6.48] |  4.90 | < .001
      no, DES-L  | yes, EC-A  |       4.58 | 0.94 | [ 2.75,  6.42] |  4.90 | < .001
      no, DF     | yes, EC-A  |       2.20 | 0.49 | [ 1.24,  3.17] |  4.49 | < .001
      yes, DES-L | yes, EC-L  |       0.26 | 0.13 | [ 0.02,  0.51] |  2.08 |  0.037
      yes, DF    | yes, EC-L  |       0.26 | 0.13 | [ 0.02,  0.51] |  2.08 |  0.037
      no, GP     | yes, EC-L  |       2.42 | 0.57 | [ 1.31,  3.53] |  4.28 | < .001
      no, PR     | yes, EC-L  |       0.34 | 0.18 | [-0.01,  0.69] |  1.89 |  0.058
      no, DM     | yes, EC-L  |       2.69 | 0.62 | [ 1.48,  3.90] |  4.37 | < .001
      no, EC-A   | yes, EC-L  |       0.93 | 0.29 | [ 0.37,  1.50] |  3.23 |  0.001
      no, EC-L   | yes, EC-L  |       4.50 | 0.95 | [ 2.63,  6.37] |  4.72 | < .001
      no, DES-L  | yes, EC-L  |       4.45 | 0.94 | [ 2.60,  6.30] |  4.71 | < .001
      no, DF     | yes, EC-L  |       2.07 | 0.50 | [ 1.09,  3.05] |  4.14 | < .001
      yes, DF    | yes, DES-L |       0.00 | 0.13 | [-0.26,  0.26] |  0.00 | > .999
      no, GP     | yes, DES-L |       2.15 | 0.59 | [ 1.00,  3.31] |  3.67 | < .001
      no, PR     | yes, DES-L |       0.07 | 0.22 | [-0.36,  0.50] |  0.33 |  0.738
      no, DM     | yes, DES-L |       2.43 | 0.64 | [ 1.18,  3.68] |  3.81 | < .001
      no, EC-A   | yes, DES-L |       0.67 | 0.32 | [ 0.04,  1.29] |  2.09 |  0.037
      no, EC-L   | yes, DES-L |       4.24 | 0.97 | [ 2.33,  6.14] |  4.36 | < .001
      no, DES-L  | yes, DES-L |       4.19 | 0.96 | [ 2.30,  6.07] |  4.35 | < .001
      no, DF     | yes, DES-L |       1.81 | 0.52 | [ 0.78,  2.83] |  3.45 | < .001
      no, GP     | yes, DF    |       2.15 | 0.59 | [ 1.00,  3.31] |  3.67 | < .001
      no, PR     | yes, DF    |       0.07 | 0.22 | [-0.36,  0.50] |  0.33 |  0.738
      no, DM     | yes, DF    |       2.43 | 0.64 | [ 1.18,  3.68] |  3.81 | < .001
      no, EC-A   | yes, DF    |       0.67 | 0.32 | [ 0.04,  1.29] |  2.09 |  0.037
      no, EC-L   | yes, DF    |       4.24 | 0.97 | [ 2.33,  6.14] |  4.36 | < .001
      no, DES-L  | yes, DF    |       4.19 | 0.96 | [ 2.30,  6.07] |  4.35 | < .001
      no, DF     | yes, DF    |       1.81 | 0.52 | [ 0.78,  2.83] |  3.45 | < .001
      no, PR     | no, GP     |      -2.08 | 0.48 | [-3.02, -1.14] | -4.35 | < .001
      no, DM     | no, GP     |       0.27 | 0.37 | [-0.46,  1.00] |  0.73 |  0.466
      no, EC-A   | no, GP     |      -1.49 | 0.41 | [-2.29, -0.68] | -3.61 | < .001
      no, EC-L   | no, GP     |       2.08 | 0.58 | [ 0.95,  3.21] |  3.61 | < .001
      no, DES-L  | no, GP     |       2.03 | 0.57 | [ 0.92,  3.15] |  3.57 | < .001
      no, DF     | no, GP     |      -0.35 | 0.35 | [-1.04,  0.35] | -0.98 |  0.328
      no, DM     | no, PR     |       2.35 | 0.53 | [ 1.32,  3.39] |  4.47 | < .001
      no, EC-A   | no, PR     |       0.59 | 0.23 | [ 0.14,  1.05] |  2.56 |  0.011
      no, EC-L   | no, PR     |       4.16 | 0.86 | [ 2.49,  5.84] |  4.87 | < .001
      no, DES-L  | no, PR     |       4.11 | 0.85 | [ 2.45,  5.77] |  4.86 | < .001
      no, DF     | no, PR     |       1.73 | 0.42 | [ 0.92,  2.55] |  4.15 | < .001
      no, EC-A   | no, DM     |      -1.76 | 0.46 | [-2.65, -0.87] | -3.86 | < .001
      no, EC-L   | no, DM     |       1.81 | 0.55 | [ 0.73,  2.89] |  3.29 | < .001
      no, DES-L  | no, DM     |       1.76 | 0.54 | [ 0.70,  2.82] |  3.24 |  0.001
      no, DF     | no, DM     |      -0.62 | 0.38 | [-1.36,  0.12] | -1.65 |  0.100
      no, EC-L   | no, EC-A   |       3.57 | 0.77 | [ 2.07,  5.07] |  4.66 | < .001
      no, DES-L  | no, EC-A   |       3.52 | 0.76 | [ 2.03,  5.00] |  4.65 | < .001
      no, DF     | no, EC-A   |       1.14 | 0.36 | [ 0.43,  1.85] |  3.16 |  0.002
      no, DES-L  | no, EC-L   |      -0.05 | 0.48 | [-0.99,  0.89] | -0.10 |  0.918
      no, DF     | no, EC-L   |      -2.43 | 0.61 | [-3.63, -1.22] | -3.95 | < .001
      no, DF     | no, DES-L  |      -2.38 | 0.61 | [-3.57, -1.19] | -3.92 | < .001
      
      Variable predicted: count
      Predictors contrasted: mined, spp
      Predictors averaged: cover (8.7e-11), site
      p-values are uncorrected.
      Contrasts are on the response-scale.

---

    Code
      print(estimate_contrasts(model, contrast = "mined", by = "spp", backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1 | Level2 | spp   | Difference |   SE |       95% CI |    z |      p
      --------------------------------------------------------------------------
      no     | yes    | GP    |       2.59 | 0.55 | [1.50, 3.67] | 4.67 | < .001
      no     | yes    | PR    |       0.45 | 0.16 | [0.13, 0.77] | 2.75 |  0.006
      no     | yes    | DM    |       2.54 | 0.63 | [1.31, 3.77] | 4.04 | < .001
      no     | yes    | EC-A  |       1.06 | 0.28 | [0.52, 1.61] | 3.85 | < .001
      no     | yes    | EC-L  |       4.50 | 0.95 | [2.63, 6.37] | 4.72 | < .001
      no     | yes    | DES-L |       4.19 | 0.96 | [2.30, 6.07] | 4.35 | < .001
      no     | yes    | DF    |       1.81 | 0.52 | [0.78, 2.83] | 3.45 | < .001
      
      Variable predicted: count
      Predictors contrasted: mined
      Predictors averaged: cover (8.7e-11), site
      p-values are uncorrected.
      Contrasts are on the response-scale.

