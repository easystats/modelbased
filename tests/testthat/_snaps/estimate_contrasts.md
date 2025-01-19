# estimate_contrasts - Frequentist

    Code
      print(estimate_contrasts(model, contrast = c("vs", "am"), by = "gear='5'",
      backend = "marginaleffects"), zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |   SE |          95% CI | t(25) |      p
      ------------------------------------------------------------------------------
      vs 0, am 0 | vs 0, am 1 |      -6.98 | 2.33 | [-11.79, -2.17] | -2.99 |  0.006
      vs 0, am 0 | vs 1, am 0 |     -11.27 | 4.04 | [-19.60, -2.95] | -2.79 |  0.010
      vs 0, am 0 | vs 1, am 1 |     -18.26 | 4.67 | [-27.88, -8.64] | -3.91 | < .001
      vs 0, am 1 | vs 1, am 0 |      -4.29 | 4.67 | [-13.91,  5.33] | -0.92 |  0.367
      vs 0, am 1 | vs 1, am 1 |     -11.27 | 4.04 | [-19.60, -2.95] | -2.79 |  0.010
      vs 1, am 0 | vs 1, am 1 |      -6.98 | 2.33 | [-11.79, -2.17] | -2.99 |  0.006
      
      Variable predicted: mpg
      Predictors contrasted: vs, am
      p-values are uncorrected.

---

    Code
      print(estimate_contrasts(model, contrast = c("three", "vs", "am"), backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1              | Level2              | Difference |   SE |          95% CI | t(24) |      p
      ------------------------------------------------------------------------------------------------
      three 0, vs 0, am 0 | three 0, vs 0, am 1 |      -2.87 | 2.24 | [ -7.49,  1.76] | -1.28 |  0.213
      three 0, vs 0, am 0 | three 0, vs 1, am 0 |      -5.83 | 2.46 | [-10.90, -0.76] | -2.37 |  0.026
      three 0, vs 0, am 0 | three 0, vs 1, am 1 |     -14.03 | 2.10 | [-18.37, -9.69] | -6.67 | < .001
      three 0, vs 0, am 0 | three 1, vs 0, am 0 |       0.57 | 2.01 | [ -3.57,  4.71] |  0.28 |  0.780
      three 0, vs 0, am 0 | three 1, vs 0, am 1 |      -7.52 | 2.84 | [-13.37, -1.66] | -2.65 |  0.014
      three 0, vs 0, am 0 | three 1, vs 1, am 0 |      -5.09 | 2.24 | [ -9.72, -0.46] | -2.27 |  0.032
      three 0, vs 0, am 0 | three 1, vs 1, am 1 |     -10.57 | 2.84 | [-16.42, -4.71] | -3.73 |  0.001
      three 0, vs 0, am 1 | three 0, vs 1, am 0 |      -2.97 | 2.65 | [ -8.44,  2.51] | -1.12 |  0.275
      three 0, vs 0, am 1 | three 0, vs 1, am 1 |     -11.16 | 2.33 | [-15.97, -6.35] | -4.79 | < .001
      three 0, vs 0, am 1 | three 1, vs 0, am 0 |       3.43 | 2.24 | [ -1.19,  8.06] |  1.53 |  0.139
      three 0, vs 0, am 1 | three 1, vs 0, am 1 |      -4.65 | 3.01 | [-10.86,  1.56] | -1.55 |  0.135
      three 0, vs 0, am 1 | three 1, vs 1, am 0 |      -2.23 | 2.46 | [ -7.29,  2.84] | -0.91 |  0.374
      three 0, vs 0, am 1 | three 1, vs 1, am 1 |      -7.70 | 3.01 | [-13.91, -1.49] | -2.56 |  0.017
      three 0, vs 1, am 0 | three 0, vs 1, am 1 |      -8.19 | 2.54 | [-13.43, -2.96] | -3.23 |  0.004
      three 0, vs 1, am 0 | three 1, vs 0, am 0 |       6.40 | 2.46 | [  1.33, 11.47] |  2.61 |  0.016
      three 0, vs 1, am 0 | three 1, vs 0, am 1 |      -1.68 | 3.17 | [ -8.23,  4.86] | -0.53 |  0.600
      three 0, vs 1, am 0 | three 1, vs 1, am 0 |       0.74 | 2.65 | [ -4.73,  6.22] |  0.28 |  0.782
      three 0, vs 1, am 0 | three 1, vs 1, am 1 |      -4.73 | 3.17 | [-11.28,  1.81] | -1.49 |  0.149
      three 0, vs 1, am 1 | three 1, vs 0, am 0 |      14.59 | 2.10 | [ 10.25, 18.93] |  6.94 | < .001
      three 0, vs 1, am 1 | three 1, vs 0, am 1 |       6.51 | 2.91 | [  0.51, 12.51] |  2.24 |  0.035
      three 0, vs 1, am 1 | three 1, vs 1, am 0 |       8.94 | 2.33 | [  4.13, 13.74] |  3.83 | < .001
      three 0, vs 1, am 1 | three 1, vs 1, am 1 |       3.46 | 2.91 | [ -2.54,  9.46] |  1.19 |  0.246
      three 1, vs 0, am 0 | three 1, vs 0, am 1 |      -8.08 | 2.84 | [-13.94, -2.23] | -2.85 |  0.009
      three 1, vs 0, am 0 | three 1, vs 1, am 0 |      -5.66 | 2.24 | [-10.29, -1.03] | -2.52 |  0.019
      three 1, vs 0, am 0 | three 1, vs 1, am 1 |     -11.13 | 2.84 | [-16.99, -5.28] | -3.93 | < .001
      three 1, vs 0, am 1 | three 1, vs 1, am 0 |       2.42 | 3.01 | [ -3.78,  8.63] |  0.81 |  0.428
      three 1, vs 0, am 1 | three 1, vs 1, am 1 |      -3.05 | 3.47 | [-10.22,  4.12] | -0.88 |  0.389
      three 1, vs 1, am 0 | three 1, vs 1, am 1 |      -5.48 | 3.01 | [-11.68,  0.73] | -1.82 |  0.081
      
      Variable predicted: mpg
      Predictors contrasted: three, vs, am
      p-values are uncorrected.

---

    Code
      print(estimate_contrasts(model, contrast = "am", backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1 | Level2 | Difference |   SE |         95% CI | t(24) |      p
      ---------------------------------------------------------------------
      0      | 1      |      -6.15 | 1.34 | [-8.91, -3.40] | -4.61 | < .001
      
      Variable predicted: mpg
      Predictors contrasted: am
      Predictors averaged: three, vs
      p-values are uncorrected.

# estimate_contrasts - marginaleffects

    Code
      print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects",
      p_adjust = "none"), zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1            | Level2             | Difference |   SE |          95% CI | t(113) |      p
      ----------------------------------------------------------------------------------------------
      morning, coffee   | morning, control   |       5.78 | 1.99 | [  1.83,  9.73] |   2.90 |  0.004
      morning, coffee   | noon, coffee       |       1.93 | 1.99 | [ -2.02,  5.88] |   0.97 |  0.336
      morning, coffee   | noon, control      |       0.00 | 1.99 | [ -3.95,  3.95] |   0.00 | > .999
      morning, coffee   | afternoon, coffee  |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 |  0.336
      morning, coffee   | afternoon, control |       0.00 | 1.99 | [ -3.95,  3.95] |   0.00 | > .999
      morning, control  | noon, coffee       |      -3.86 | 1.99 | [ -7.81,  0.09] |  -1.93 |  0.056
      morning, control  | noon, control      |      -5.78 | 1.99 | [ -9.73, -1.83] |  -2.90 |  0.004
      morning, control  | afternoon, coffee  |      -7.71 | 1.99 | [-11.66, -3.76] |  -3.87 | < .001
      morning, control  | afternoon, control |      -5.78 | 1.99 | [ -9.73, -1.83] |  -2.90 |  0.004
      noon, coffee      | noon, control      |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 |  0.336
      noon, coffee      | afternoon, coffee  |      -3.86 | 1.99 | [ -7.81,  0.09] |  -1.93 |  0.056
      noon, coffee      | afternoon, control |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 |  0.336
      noon, control     | afternoon, coffee  |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 |  0.336
      noon, control     | afternoon, control |       0.00 | 1.99 | [ -3.95,  3.95] |   0.00 | > .999
      afternoon, coffee | afternoon, control |       1.93 | 1.99 | [ -2.02,  5.88] |   0.97 |  0.336
      
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
      
      hypothesis              | coffee  | Difference |   SE |       95% CI | t(113) |      p
      --------------------------------------------------------------------------------------
      (noon) / (morning)      | coffee  |       0.89 | 0.11 | [0.67, 1.11] |   8.08 | < .001
      (afternoon) / (morning) | coffee  |       1.11 | 0.12 | [0.87, 1.36] |   9.05 | < .001
      (noon) / (morning)      | control |       1.51 | 0.22 | [1.06, 1.95] |   6.73 | < .001
      (afternoon) / (morning) | control |       1.51 | 0.22 | [1.06, 1.95] |   6.73 | < .001
      
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
      Predictors averaged: c12hour, barthtot, c161sex
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
      setosa     | versicolor |      -0.68 | 0.07 | [-0.82, -0.54] | -9.27 | < .001
      setosa     | virginica  |      -0.50 | 0.08 | [-0.67, -0.33] | -5.90 | < .001
      versicolor | virginica  |       0.18 | 0.08 | [ 0.01,  0.35] |  2.12 |  0.034
      
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
      setosa     | versicolor |      -0.68 | 0.07 | [-0.82, -0.54] | -9.27 | < .001
      setosa     | virginica  |      -0.50 | 0.08 | [-0.67, -0.33] | -5.90 | < .001
      versicolor | virginica  |       0.18 | 0.08 | [ 0.01,  0.35] |  2.12 |  0.034
      
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
      low    | mid    |       0.09 | 0.34 | [-0.58, 0.76] |   0.25 | 0.802
      low    | high   |      -0.61 | 0.43 | [-1.45, 0.24] |  -1.40 | 0.162
      mid    | high   |      -0.69 | 0.36 | [-1.40, 0.02] |  -1.92 | 0.055
      
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
      
      Level1      | Level2       | Difference |   SE |         95% CI | t(825) |     p
      --------------------------------------------------------------------------------
      Male, low   | Male, mid    |      -0.29 | 0.71 | [-1.68,  1.10] |  -0.41 | 0.684
      Male, low   | Male, high   |      -0.69 | 0.83 | [-2.32,  0.95] |  -0.82 | 0.410
      Male, low   | Female, low  |      -1.05 | 0.69 | [-2.40,  0.31] |  -1.51 | 0.131
      Male, low   | Female, mid  |      -0.85 | 0.64 | [-2.10,  0.40] |  -1.33 | 0.183
      Male, low   | Female, high |      -1.65 | 0.71 | [-3.05, -0.24] |  -2.30 | 0.022
      Male, mid   | Male, high   |      -0.40 | 0.68 | [-1.73,  0.94] |  -0.59 | 0.558
      Male, mid   | Female, low  |      -0.76 | 0.50 | [-1.73,  0.22] |  -1.52 | 0.129
      Male, mid   | Female, mid  |      -0.56 | 0.42 | [-1.38,  0.26] |  -1.35 | 0.178
      Male, mid   | Female, high |      -1.36 | 0.53 | [-2.39, -0.32] |  -2.58 | 0.010
      Male, high  | Female, low  |      -0.36 | 0.66 | [-1.66,  0.95] |  -0.54 | 0.589
      Male, high  | Female, mid  |      -0.16 | 0.61 | [-1.35,  1.03] |  -0.27 | 0.789
      Male, high  | Female, high |      -0.96 | 0.69 | [-2.30,  0.39] |  -1.40 | 0.163
      Female, low | Female, mid  |       0.20 | 0.39 | [-0.57,  0.96] |   0.51 | 0.613
      Female, low | Female, high |      -0.60 | 0.51 | [-1.59,  0.39] |  -1.18 | 0.236
      Female, mid | Female, high |      -0.80 | 0.43 | [-1.63,  0.04] |  -1.87 | 0.061
      
      Variable predicted: neg_c_7
      Predictors contrasted: c161sex, c172code
      Predictors averaged: e16sex
      p-values are uncorrected.

# estimate_contrasts - simple contrasts and with - in levels works

    Code
      print(estimate_contrasts(model, "Species", backend = "marginaleffects"),
      table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |   SE |         95% CI | t(146) |      p
      ------------------------------------------------------------------------------
      setosa     | versicolor |      -1.46 | 0.11 | [-1.68, -1.24] | -13.01 | < .001
      setosa     | virginica  |      -1.95 | 0.10 | [-2.14, -1.75] | -19.47 | < .001
      versicolor | virginica  |      -0.49 | 0.09 | [-0.67, -0.31] |  -5.41 | < .001
      
      Variable predicted: Sepal.Length
      Predictors contrasted: Species
      Predictors averaged: Sepal.Width
      p-values are uncorrected.

---

    Code
      print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1            | Level2             | Difference |   SE |          95% CI | t(113) |      p
      ----------------------------------------------------------------------------------------------
      morning, coffee   | morning, control   |       5.78 | 1.99 | [  1.83,  9.73] |   2.90 |  0.004
      morning, coffee   | noon, coffee       |       1.93 | 1.99 | [ -2.02,  5.88] |   0.97 |  0.336
      morning, coffee   | noon, control      |       0.00 | 1.99 | [ -3.95,  3.95] |   0.00 | > .999
      morning, coffee   | afternoon, coffee  |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 |  0.336
      morning, coffee   | afternoon, control |       0.00 | 1.99 | [ -3.95,  3.95] |   0.00 | > .999
      morning, control  | noon, coffee       |      -3.86 | 1.99 | [ -7.81,  0.09] |  -1.93 |  0.056
      morning, control  | noon, control      |      -5.78 | 1.99 | [ -9.73, -1.83] |  -2.90 |  0.004
      morning, control  | afternoon, coffee  |      -7.71 | 1.99 | [-11.66, -3.76] |  -3.87 | < .001
      morning, control  | afternoon, control |      -5.78 | 1.99 | [ -9.73, -1.83] |  -2.90 |  0.004
      noon, coffee      | noon, control      |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 |  0.336
      noon, coffee      | afternoon, coffee  |      -3.86 | 1.99 | [ -7.81,  0.09] |  -1.93 |  0.056
      noon, coffee      | afternoon, control |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 |  0.336
      noon, control     | afternoon, coffee  |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 |  0.336
      noon, control     | afternoon, control |       0.00 | 1.99 | [ -3.95,  3.95] |   0.00 | > .999
      afternoon, coffee | afternoon, control |       1.93 | 1.99 | [ -2.02,  5.88] |   0.97 |  0.336
      
      Variable predicted: alertness
      Predictors contrasted: time, coffee
      Predictors averaged: sex
      p-values are uncorrected.

