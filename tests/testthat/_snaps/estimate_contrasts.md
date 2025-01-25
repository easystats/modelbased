# estimate_contrasts - Frequentist

    Code
      print(estimate_contrasts(model, contrast = c("vs", "am"), by = "gear='5'",
      backend = "marginaleffects"), zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |   SE |         95% CI | t(25) |      p
      -----------------------------------------------------------------------------
      vs 0, am 1 | vs 0, am 0 |       6.98 | 2.33 | [ 2.17, 11.79] |  2.99 |  0.006
      vs 1, am 0 | vs 0, am 0 |      11.27 | 4.04 | [ 2.95, 19.60] |  2.79 |  0.010
      vs 1, am 0 | vs 0, am 1 |       4.29 | 4.67 | [-5.33, 13.91] |  0.92 |  0.367
      vs 1, am 1 | vs 0, am 0 |      18.26 | 4.67 | [ 8.64, 27.88] |  3.91 | < .001
      vs 1, am 1 | vs 0, am 1 |      11.27 | 4.04 | [ 2.95, 19.60] |  2.79 |  0.010
      vs 1, am 1 | vs 1, am 0 |       6.98 | 2.33 | [ 2.17, 11.79] |  2.99 |  0.006
      
      Variable predicted: mpg
      Predictors contrasted: vs, am
      p-values are uncorrected.

# estimate_contrasts - marginaleffects

    Code
      print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects",
      p_adjust = "none"), zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1             | Level2            | Difference |   SE |         95% CI | t(113) |      p
      ---------------------------------------------------------------------------------------------
      morning, control   | morning, coffee   |      -5.78 | 1.99 | [-9.73, -1.83] |  -2.90 |  0.004
      noon, coffee       | morning, coffee   |      -1.93 | 1.99 | [-5.88,  2.02] |  -0.97 |  0.336
      noon, coffee       | morning, control  |       3.86 | 1.99 | [-0.09,  7.81] |   1.93 |  0.056
      noon, control      | morning, coffee   |       0.00 | 1.99 | [-3.95,  3.95] |   0.00 | > .999
      noon, control      | morning, control  |       5.78 | 1.99 | [ 1.83,  9.73] |   2.90 |  0.004
      noon, control      | noon, coffee      |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
      afternoon, coffee  | morning, coffee   |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
      afternoon, coffee  | morning, control  |       7.71 | 1.99 | [ 3.76, 11.66] |   3.87 | < .001
      afternoon, coffee  | noon, coffee      |       3.86 | 1.99 | [-0.09,  7.81] |   1.93 |  0.056
      afternoon, coffee  | noon, control     |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
      afternoon, control | morning, coffee   |       0.00 | 1.99 | [-3.95,  3.95] |   0.00 | > .999
      afternoon, control | morning, control  |       5.78 | 1.99 | [ 1.83,  9.73] |   2.90 |  0.004
      afternoon, control | noon, coffee      |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
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
      
      Parameter                               | coffee  | Difference |   SE |       95% CI | t(113) |      p
      ------------------------------------------------------------------------------------------------------
      (noon coffee) / (morning coffee)        | coffee  |       0.89 | 0.11 | [0.67, 1.11] |   8.08 | < .001
      (afternoon coffee) / (morning coffee)   | coffee  |       1.11 | 0.12 | [0.87, 1.36] |   9.05 | < .001
      (noon control) / (morning control)      | control |       1.51 | 0.22 | [1.06, 1.95] |   6.73 | < .001
      (afternoon control) / (morning control) | control |       1.51 | 0.22 | [1.06, 1.95] |   6.73 | < .001
      
      Variable predicted: alertness
      Predictors contrasted: time, coffee
      Predictors averaged: sex
      p-values are uncorrected.

---

    Code
      print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects",
      p_adjust = "none", comparison = "(b2-b1)=(b4-b3)"), zap_small = TRUE,
      table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Parameter       | Difference |   SE |        95% CI | t(113) |     p
      --------------------------------------------------------------------
      (b2-b1)=(b4-b3) |       5.78 | 2.82 | [0.20, 11.37] |   2.05 | 0.043
      
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

---

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
      Male, high   | Male, mid   |       0.40 | 0.68 | [-0.94, 1.73] |   0.59 | 0.558
      Female, low  | Male, low   |       1.05 | 0.69 | [-0.31, 2.40] |   1.51 | 0.131
      Female, low  | Male, mid   |       0.76 | 0.50 | [-0.22, 1.73] |   1.52 | 0.129
      Female, low  | Male, high  |       0.36 | 0.66 | [-0.95, 1.66] |   0.54 | 0.589
      Female, mid  | Male, low   |       0.85 | 0.64 | [-0.40, 2.10] |   1.33 | 0.183
      Female, mid  | Male, mid   |       0.56 | 0.42 | [-0.26, 1.38] |   1.35 | 0.178
      Female, mid  | Male, high  |       0.16 | 0.61 | [-1.03, 1.35] |   0.27 | 0.789
      Female, mid  | Female, low |      -0.20 | 0.39 | [-0.96, 0.57] |  -0.51 | 0.613
      Female, high | Male, low   |       1.65 | 0.71 | [ 0.24, 3.05] |   2.30 | 0.022
      Female, high | Male, mid   |       1.36 | 0.53 | [ 0.32, 2.39] |   2.58 | 0.010
      Female, high | Male, high  |       0.96 | 0.69 | [-0.39, 2.30] |   1.40 | 0.163
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
      mid, male    | low, female |       0.00 | 0.02 | [-0.03, 0.03] | -0.08 | 0.940
      mid, female  | low, male   |       0.02 | 0.01 | [-0.01, 0.05] |  1.24 | 0.216
      mid, female  | low, female |       0.02 | 0.01 | [ 0.00, 0.05] |  1.76 | 0.079
      mid, female  | mid, male   |       0.03 | 0.01 | [ 0.00, 0.05] |  2.24 | 0.025
      high, male   | low, male   |       0.00 | 0.02 | [-0.05, 0.04] | -0.16 | 0.876
      high, male   | low, female |       0.00 | 0.02 | [-0.04, 0.05] |  0.12 | 0.908
      high, male   | mid, male   |       0.00 | 0.02 | [-0.04, 0.05] |  0.18 | 0.859
      high, male   | mid, female |      -0.02 | 0.02 | [-0.06, 0.02] | -1.08 | 0.280
      high, female | low, male   |       0.02 | 0.02 | [-0.01, 0.06] |  1.19 | 0.236
      high, female | low, female |       0.03 | 0.02 | [-0.01, 0.06] |  1.58 | 0.115
      high, female | mid, male   |       0.03 | 0.02 | [ 0.00, 0.06] |  1.83 | 0.067
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
      noon, coffee       | morning, control  |       3.86 | 1.99 | [-0.09,  7.81] |   1.93 |  0.056
      noon, control      | morning, coffee   |       0.00 | 1.99 | [-3.95,  3.95] |   0.00 | > .999
      noon, control      | morning, control  |       5.78 | 1.99 | [ 1.83,  9.73] |   2.90 |  0.004
      noon, control      | noon, coffee      |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
      afternoon, coffee  | morning, coffee   |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
      afternoon, coffee  | morning, control  |       7.71 | 1.99 | [ 3.76, 11.66] |   3.87 | < .001
      afternoon, coffee  | noon, coffee      |       3.86 | 1.99 | [-0.09,  7.81] |   1.93 |  0.056
      afternoon, coffee  | noon, control     |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
      afternoon, control | morning, coffee   |       0.00 | 1.99 | [-3.95,  3.95] |   0.00 | > .999
      afternoon, control | morning, control  |       5.78 | 1.99 | [ 1.83,  9.73] |   2.90 |  0.004
      afternoon, control | noon, coffee      |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
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
      noon      | morning | control |       5.78 | 1.99 | [ 1.83, 9.73] |   2.90 |  0.004
      afternoon | morning | coffee  |       1.93 | 1.99 | [-2.02, 5.88] |   0.97 |  0.336
      afternoon | morning | control |       5.78 | 1.99 | [ 1.83, 9.73] |   2.90 |  0.004
      afternoon | noon    | coffee  |       3.86 | 1.99 | [-0.09, 7.81] |   1.93 |  0.056
      afternoon | noon    | control |       0.00 | 1.99 | [-3.95, 3.95] |   0.00 | > .999
      
      Variable predicted: alertness
      Predictors contrasted: time
      Predictors averaged: sex
      p-values are uncorrected.

