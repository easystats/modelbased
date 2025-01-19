# estimate_contrasts - marginaleffects

    Code
      print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects",
      p_adjust = "none"), zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level 1           | Level 2            | Difference |   SE |          95% CI | t(113) |      p
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
      
      Species                | Difference |   SE |         95% CI |     z |      p
      ----------------------------------------------------------------------------
      setosa - versicolor    |      -0.68 | 0.07 | [-0.82, -0.54] | -9.27 | < .001
      setosa - virginica     |      -0.50 | 0.08 | [-0.67, -0.33] | -5.90 | < .001
      versicolor - virginica |       0.18 | 0.08 | [ 0.01,  0.35] |  2.12 |  0.034
      
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
      
      Species                | Difference |   SE |         95% CI |     z |      p
      ----------------------------------------------------------------------------
      setosa - versicolor    |      -0.68 | 0.07 | [-0.82, -0.54] | -9.27 | < .001
      setosa - virginica     |      -0.50 | 0.08 | [-0.67, -0.33] | -5.90 | < .001
      versicolor - virginica |       0.18 | 0.08 | [ 0.01,  0.35] |  2.12 |  0.034
      
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
      
      c172code   | Difference |   SE |        95% CI | t(827) |     p
      ---------------------------------------------------------------
      low - mid  |       0.09 | 0.34 | [-0.58, 0.76] |   0.25 | 0.802
      low - high |      -0.61 | 0.43 | [-1.45, 0.24] |  -1.40 | 0.162
      mid - high |      -0.69 | 0.36 | [-1.40, 0.02] |  -1.92 | 0.055
      
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
      
      Level 1     | Level 2      | Difference |   SE |         95% CI | t(825) |     p
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

---

    Code
      print(estimate_contrasts(fit, "c161sex", "c172code", backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      c161sex       | c172code | Difference |   SE |        95% CI | t(825) |     p
      -----------------------------------------------------------------------------
      Male - Female | low      |      -1.05 | 0.69 | [-2.40, 0.31] |  -1.51 | 0.131
      Male - Female | mid      |      -0.56 | 0.42 | [-1.38, 0.26] |  -1.35 | 0.178
      Male - Female | high     |      -0.96 | 0.69 | [-2.30, 0.39] |  -1.40 | 0.163
      
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
      
      c172code   | Difference |   SE |        95% CI |     t |     p
      --------------------------------------------------------------
      low - mid  |      -0.01 | 0.01 | [-0.03, 0.01] | -1.17 | 0.243
      low - high |      -0.02 | 0.01 | [-0.04, 0.01] | -1.10 | 0.271
      mid - high |       0.00 | 0.01 | [-0.03, 0.02] | -0.27 | 0.786
      
      Variable predicted: neg_c_7
      Predictors contrasted: barthtot
      Predictors averaged: e16sex, barthtot
      p-values are uncorrected.

---

    Code
      print(estimate_contrasts(fit, "barthtot", c("c172code", "e16sex"), backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      Level 1     | Level 2      | Difference |   SE |         95% CI |     t |     p
      -------------------------------------------------------------------------------
      low, male   | low, female  |       0.01 | 0.02 | [-0.03,  0.04] |  0.35 | 0.729
      low, male   | mid, male    |       0.01 | 0.02 | [-0.02,  0.04] |  0.46 | 0.648
      low, male   | mid, female  |      -0.02 | 0.01 | [-0.05,  0.01] | -1.24 | 0.216
      low, male   | high, male   |       0.00 | 0.02 | [-0.04,  0.05] |  0.16 | 0.876
      low, male   | high, female |      -0.02 | 0.02 | [-0.06,  0.01] | -1.19 | 0.236
      low, female | mid, male    |       0.00 | 0.02 | [-0.03,  0.03] |  0.08 | 0.940
      low, female | mid, female  |      -0.02 | 0.01 | [-0.05,  0.00] | -1.76 | 0.079
      low, female | high, male   |       0.00 | 0.02 | [-0.05,  0.04] | -0.12 | 0.908
      low, female | high, female |      -0.03 | 0.02 | [-0.06,  0.01] | -1.58 | 0.115
      mid, male   | mid, female  |      -0.03 | 0.01 | [-0.05,  0.00] | -2.24 | 0.025
      mid, male   | high, male   |       0.00 | 0.02 | [-0.05,  0.04] | -0.18 | 0.859
      mid, male   | high, female |      -0.03 | 0.02 | [-0.06,  0.00] | -1.83 | 0.067
      mid, female | high, male   |       0.02 | 0.02 | [-0.02,  0.06] |  1.08 | 0.280
      mid, female | high, female |       0.00 | 0.01 | [-0.03,  0.02] | -0.26 | 0.792
      high, male  | high, female |      -0.03 | 0.02 | [-0.07,  0.02] | -1.11 | 0.268
      
      Variable predicted: neg_c_7
      Predictors contrasted: barthtot
      Predictors averaged: barthtot
      p-values are uncorrected.

# estimate_contrasts - simple contrasts and with - in levels works

    Code
      print(estimate_contrasts(model, "Species", backend = "marginaleffects"),
      table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Species                | Difference |   SE |         95% CI | t(146) |      p
      -----------------------------------------------------------------------------
      setosa - versicolor    |      -1.46 | 0.11 | [-1.68, -1.24] | -13.01 | < .001
      setosa - virginica     |      -1.95 | 0.10 | [-2.14, -1.75] | -19.47 | < .001
      versicolor - virginica |      -0.49 | 0.09 | [-0.67, -0.31] |  -5.41 | < .001
      
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
      
      Level 1           | Level 2            | Difference |   SE |          95% CI | t(113) |      p
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
      print(estimate_contrasts(m, contrast = "time", by = "coffee", backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      time                | coffee  | Difference |   SE |         95% CI | t(113) |      p
      ------------------------------------------------------------------------------------
      morning - noon      | coffee  |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 |  0.336
      morning - afternoon | coffee  |      -1.93 | 1.99 | [-5.88,  2.02] |  -0.97 |  0.336
      noon - afternoon    | coffee  |      -3.86 | 1.99 | [-7.81,  0.09] |  -1.93 |  0.056
      morning - noon      | control |      -5.78 | 1.99 | [-9.73, -1.83] |  -2.90 |  0.004
      morning - afternoon | control |      -5.78 | 1.99 | [-9.73, -1.83] |  -2.90 |  0.004
      noon - afternoon    | control |       0.00 | 1.99 | [-3.95,  3.95] |   0.00 | > .999
      
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
      
      Level 1    | Level 2    | Difference |   SE |         95% CI |     z |      p
      -----------------------------------------------------------------------------
      yes, GP    | yes, PR    |      -0.06 | 0.05 | [-0.16,  0.04] | -1.10 |  0.271
      yes, GP    | yes, DM    |      -0.32 | 0.11 | [-0.54, -0.10] | -2.89 |  0.004
      yes, GP    | yes, EC-A  |      -0.04 | 0.05 | [-0.13,  0.05] | -0.80 |  0.421
      yes, GP    | yes, EC-L  |      -0.17 | 0.08 | [-0.32, -0.02] | -2.19 |  0.028
      yes, GP    | yes, DES-L |      -0.43 | 0.14 | [-0.70, -0.17] | -3.19 |  0.001
      yes, GP    | yes, DF    |      -0.43 | 0.14 | [-0.70, -0.17] | -3.19 |  0.001
      yes, GP    | no, GP     |      -2.59 | 0.55 | [-3.67, -1.50] | -4.67 | < .001
      yes, GP    | no, PR     |      -0.51 | 0.16 | [-0.82, -0.20] | -3.21 |  0.001
      yes, GP    | no, DM     |      -2.86 | 0.61 | [-4.05, -1.68] | -4.73 | < .001
      yes, GP    | no, EC-A   |      -1.10 | 0.27 | [-1.64, -0.57] | -4.03 | < .001
      yes, GP    | no, EC-L   |      -4.67 | 0.94 | [-6.52, -2.82] | -4.95 | < .001
      yes, GP    | no, DES-L  |      -4.62 | 0.93 | [-6.45, -2.79] | -4.95 | < .001
      yes, GP    | no, DF     |      -2.24 | 0.49 | [-3.20, -1.28] | -4.58 | < .001
      yes, PR    | yes, DM    |      -0.26 | 0.11 | [-0.48, -0.05] | -2.43 |  0.015
      yes, PR    | yes, EC-A  |       0.02 | 0.06 | [-0.09,  0.13] |  0.33 |  0.740
      yes, PR    | yes, EC-L  |      -0.11 | 0.08 | [-0.27,  0.04] | -1.43 |  0.154
      yes, PR    | yes, DES-L |      -0.38 | 0.13 | [-0.64, -0.12] | -2.86 |  0.004
      yes, PR    | yes, DF    |      -0.38 | 0.13 | [-0.64, -0.12] | -2.86 |  0.004
      yes, PR    | no, GP     |      -2.53 | 0.56 | [-3.62, -1.44] | -4.54 | < .001
      yes, PR    | no, PR     |      -0.45 | 0.16 | [-0.77, -0.13] | -2.75 |  0.006
      yes, PR    | no, DM     |      -2.80 | 0.61 | [-4.00, -1.61] | -4.61 | < .001
      yes, PR    | no, EC-A   |      -1.05 | 0.28 | [-1.59, -0.50] | -3.76 | < .001
      yes, PR    | no, EC-L   |      -4.61 | 0.95 | [-6.47, -2.76] | -4.87 | < .001
      yes, PR    | no, DES-L  |      -4.56 | 0.94 | [-6.40, -2.73] | -4.87 | < .001
      yes, PR    | no, DF     |      -2.18 | 0.49 | [-3.15, -1.22] | -4.44 | < .001
      yes, DM    | yes, EC-A  |       0.28 | 0.11 | [ 0.07,  0.50] |  2.59 |  0.010
      yes, DM    | yes, EC-L  |       0.15 | 0.11 | [-0.06,  0.36] |  1.39 |  0.164
      yes, DM    | yes, DES-L |      -0.11 | 0.13 | [-0.36,  0.14] | -0.89 |  0.375
      yes, DM    | yes, DF    |      -0.11 | 0.13 | [-0.36,  0.14] | -0.89 |  0.375
      yes, DM    | no, GP     |      -2.27 | 0.58 | [-3.40, -1.14] | -3.93 | < .001
      yes, DM    | no, PR     |      -0.19 | 0.20 | [-0.58,  0.21] | -0.93 |  0.352
      yes, DM    | no, DM     |      -2.54 | 0.63 | [-3.77, -1.31] | -4.04 | < .001
      yes, DM    | no, EC-A   |      -0.78 | 0.31 | [-1.38, -0.18] | -2.56 |  0.011
      yes, DM    | no, EC-L   |      -4.35 | 0.96 | [-6.24, -2.46] | -4.51 | < .001
      yes, DM    | no, DES-L  |      -4.30 | 0.95 | [-6.17, -2.43] | -4.51 | < .001
      yes, DM    | no, DF     |      -1.92 | 0.51 | [-2.93, -0.91] | -3.74 | < .001
      yes, EC-A  | yes, EC-L  |      -0.13 | 0.08 | [-0.29,  0.02] | -1.68 |  0.093
      yes, EC-A  | yes, DES-L |      -0.40 | 0.13 | [-0.66, -0.14] | -2.97 |  0.003
      yes, EC-A  | yes, DF    |      -0.40 | 0.13 | [-0.66, -0.14] | -2.97 |  0.003
      yes, EC-A  | no, GP     |      -2.55 | 0.56 | [-3.64, -1.46] | -4.58 | < .001
      yes, EC-A  | no, PR     |      -0.47 | 0.16 | [-0.79, -0.15] | -2.90 |  0.004
      yes, EC-A  | no, DM     |      -2.82 | 0.61 | [-4.01, -1.63] | -4.65 | < .001
      yes, EC-A  | no, EC-A   |      -1.06 | 0.28 | [-1.61, -0.52] | -3.85 | < .001
      yes, EC-A  | no, EC-L   |      -4.63 | 0.95 | [-6.48, -2.78] | -4.90 | < .001
      yes, EC-A  | no, DES-L  |      -4.58 | 0.94 | [-6.42, -2.75] | -4.90 | < .001
      yes, EC-A  | no, DF     |      -2.20 | 0.49 | [-3.17, -1.24] | -4.49 | < .001
      yes, EC-L  | yes, DES-L |      -0.26 | 0.13 | [-0.51, -0.02] | -2.08 |  0.037
      yes, EC-L  | yes, DF    |      -0.26 | 0.13 | [-0.51, -0.02] | -2.08 |  0.037
      yes, EC-L  | no, GP     |      -2.42 | 0.57 | [-3.53, -1.31] | -4.28 | < .001
      yes, EC-L  | no, PR     |      -0.34 | 0.18 | [-0.69,  0.01] | -1.89 |  0.058
      yes, EC-L  | no, DM     |      -2.69 | 0.62 | [-3.90, -1.48] | -4.37 | < .001
      yes, EC-L  | no, EC-A   |      -0.93 | 0.29 | [-1.50, -0.37] | -3.23 |  0.001
      yes, EC-L  | no, EC-L   |      -4.50 | 0.95 | [-6.37, -2.63] | -4.72 | < .001
      yes, EC-L  | no, DES-L  |      -4.45 | 0.94 | [-6.30, -2.60] | -4.71 | < .001
      yes, EC-L  | no, DF     |      -2.07 | 0.50 | [-3.05, -1.09] | -4.14 | < .001
      yes, DES-L | yes, DF    |       0.00 | 0.13 | [-0.26,  0.26] |  0.00 | > .999
      yes, DES-L | no, GP     |      -2.15 | 0.59 | [-3.31, -1.00] | -3.67 | < .001
      yes, DES-L | no, PR     |      -0.07 | 0.22 | [-0.50,  0.36] | -0.33 |  0.738
      yes, DES-L | no, DM     |      -2.43 | 0.64 | [-3.68, -1.18] | -3.81 | < .001
      yes, DES-L | no, EC-A   |      -0.67 | 0.32 | [-1.29, -0.04] | -2.09 |  0.037
      yes, DES-L | no, EC-L   |      -4.24 | 0.97 | [-6.14, -2.33] | -4.36 | < .001
      yes, DES-L | no, DES-L  |      -4.19 | 0.96 | [-6.07, -2.30] | -4.35 | < .001
      yes, DES-L | no, DF     |      -1.81 | 0.52 | [-2.83, -0.78] | -3.45 | < .001
      yes, DF    | no, GP     |      -2.15 | 0.59 | [-3.31, -1.00] | -3.67 | < .001
      yes, DF    | no, PR     |      -0.07 | 0.22 | [-0.50,  0.36] | -0.33 |  0.738
      yes, DF    | no, DM     |      -2.43 | 0.64 | [-3.68, -1.18] | -3.81 | < .001
      yes, DF    | no, EC-A   |      -0.67 | 0.32 | [-1.29, -0.04] | -2.09 |  0.037
      yes, DF    | no, EC-L   |      -4.24 | 0.97 | [-6.14, -2.33] | -4.36 | < .001
      yes, DF    | no, DES-L  |      -4.19 | 0.96 | [-6.07, -2.30] | -4.35 | < .001
      yes, DF    | no, DF     |      -1.81 | 0.52 | [-2.83, -0.78] | -3.45 | < .001
      no, GP     | no, PR     |       2.08 | 0.48 | [ 1.14,  3.02] |  4.35 | < .001
      no, GP     | no, DM     |      -0.27 | 0.37 | [-1.00,  0.46] | -0.73 |  0.466
      no, GP     | no, EC-A   |       1.49 | 0.41 | [ 0.68,  2.29] |  3.61 | < .001
      no, GP     | no, EC-L   |      -2.08 | 0.58 | [-3.21, -0.95] | -3.61 | < .001
      no, GP     | no, DES-L  |      -2.03 | 0.57 | [-3.15, -0.92] | -3.57 | < .001
      no, GP     | no, DF     |       0.35 | 0.35 | [-0.35,  1.04] |  0.98 |  0.328
      no, PR     | no, DM     |      -2.35 | 0.53 | [-3.39, -1.32] | -4.47 | < .001
      no, PR     | no, EC-A   |      -0.59 | 0.23 | [-1.05, -0.14] | -2.56 |  0.011
      no, PR     | no, EC-L   |      -4.16 | 0.86 | [-5.84, -2.49] | -4.87 | < .001
      no, PR     | no, DES-L  |      -4.11 | 0.85 | [-5.77, -2.45] | -4.86 | < .001
      no, PR     | no, DF     |      -1.73 | 0.42 | [-2.55, -0.92] | -4.15 | < .001
      no, DM     | no, EC-A   |       1.76 | 0.46 | [ 0.87,  2.65] |  3.86 | < .001
      no, DM     | no, EC-L   |      -1.81 | 0.55 | [-2.89, -0.73] | -3.29 | < .001
      no, DM     | no, DES-L  |      -1.76 | 0.54 | [-2.82, -0.70] | -3.24 |  0.001
      no, DM     | no, DF     |       0.62 | 0.38 | [-0.12,  1.36] |  1.65 |  0.100
      no, EC-A   | no, EC-L   |      -3.57 | 0.77 | [-5.07, -2.07] | -4.66 | < .001
      no, EC-A   | no, DES-L  |      -3.52 | 0.76 | [-5.00, -2.03] | -4.65 | < .001
      no, EC-A   | no, DF     |      -1.14 | 0.36 | [-1.85, -0.43] | -3.16 |  0.002
      no, EC-L   | no, DES-L  |       0.05 | 0.48 | [-0.89,  0.99] |  0.10 |  0.918
      no, EC-L   | no, DF     |       2.43 | 0.61 | [ 1.22,  3.63] |  3.95 | < .001
      no, DES-L  | no, DF     |       2.38 | 0.61 | [ 1.19,  3.57] |  3.92 | < .001
      
      Variable predicted: count
      Predictors contrasted: mined, spp
      Predictors averaged: cover, site
      p-values are uncorrected.
      Contrasts are on the response-scale.

---

    Code
      print(estimate_contrasts(model, contrast = "mined", by = "spp", backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      mined    | spp   | Difference |   SE |         95% CI |     z |      p
      ----------------------------------------------------------------------
      yes - no | GP    |      -2.59 | 0.55 | [-3.67, -1.50] | -4.67 | < .001
      yes - no | PR    |      -0.45 | 0.16 | [-0.77, -0.13] | -2.75 |  0.006
      yes - no | DM    |      -2.54 | 0.63 | [-3.77, -1.31] | -4.04 | < .001
      yes - no | EC-A  |      -1.06 | 0.28 | [-1.61, -0.52] | -3.85 | < .001
      yes - no | EC-L  |      -4.50 | 0.95 | [-6.37, -2.63] | -4.72 | < .001
      yes - no | DES-L |      -4.19 | 0.96 | [-6.07, -2.30] | -4.35 | < .001
      yes - no | DF    |      -1.81 | 0.52 | [-2.83, -0.78] | -3.45 | < .001
      
      Variable predicted: count
      Predictors contrasted: mined
      Predictors averaged: cover, site
      p-values are uncorrected.
      Contrasts are on the response-scale.

