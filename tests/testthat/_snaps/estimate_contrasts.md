# estimate_contrasts - marginaleffects

    Code
      print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects",
      p_adjust = "none"), zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      time                  |            coffee | Difference |   SE |          95% CI | t(113) |      p
      -------------------------------------------------------------------------------------------------
      morning - morning     |  coffee - control |       5.78 | 1.99 | [  1.83,  9.73] |   2.90 | 0.004 
      morning - noon        |  coffee - control |       0.00 | 1.99 | [ -3.95,  3.95] |   0.00 | > .999
      morning - afternoon   |  coffee - control |       0.00 | 1.99 | [ -3.95,  3.95] |   0.00 | > .999
      noon - noon           |  coffee - control |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 | 0.336 
      noon - afternoon      |  coffee - control |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 | 0.336 
      afternoon - afternoon |  coffee - control |       1.93 | 1.99 | [ -2.02,  5.88] |   0.97 | 0.336 
      morning - noon        |   coffee - coffee |       1.93 | 1.99 | [ -2.02,  5.88] |   0.97 | 0.336 
      morning - afternoon   |   coffee - coffee |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 | 0.336 
      noon - afternoon      |   coffee - coffee |      -3.86 | 1.99 | [ -7.81,  0.09] |  -1.93 | 0.056 
      morning - noon        |  control - coffee |      -3.86 | 1.99 | [ -7.81,  0.09] |  -1.93 | 0.056 
      morning - afternoon   |  control - coffee |      -7.71 | 1.99 | [-11.66, -3.76] |  -3.87 | < .001
      noon - afternoon      |  control - coffee |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 | 0.336 
      morning - noon        | control - control |      -5.78 | 1.99 | [ -9.73, -1.83] |  -2.90 | 0.004 
      morning - afternoon   | control - control |      -5.78 | 1.99 | [ -9.73, -1.83] |  -2.90 | 0.004 
      noon - afternoon      | control - control |       0.00 | 1.99 | [ -3.95,  3.95] |   0.00 | > .999
      
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
      
      coffee  |              hypothesis | Difference |   SE |       95% CI | t(113) |      p
      --------------------------------------------------------------------------------------
      coffee  |      (noon) / (morning) |       0.89 | 0.11 | [0.67, 1.11] |   8.08 | < .001
      coffee  | (afternoon) / (morning) |       1.11 | 0.12 | [0.87, 1.36] |   9.05 | < .001
      control |      (noon) / (morning) |       1.51 | 0.22 | [1.06, 1.95] |   6.73 | < .001
      control | (afternoon) / (morning) |       1.51 | 0.22 | [1.06, 1.95] |   6.73 | < .001
      
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
      
      Parameter       | Difference |   SE |          95% CI | t(113) |     p
      ----------------------------------------------------------------------
      (b2-b1)=(b4-b3) |      -7.71 | 2.82 | [-13.30, -2.12] |  -2.73 | 0.007
      
      Variable predicted: alertness
      Predictors contrasted: time, coffee
      Predictors averaged: sex
      p-values are uncorrected.
      Parameters:
      b2 = time [noon], coffee [coffee], sex [male]
      b1 = time [morning], coffee [coffee], sex [male]
      b4 = time [morning], coffee [control], sex [male]
      b3 = time [afternoon], coffee [coffee], sex [male]

---

    Code
      estimate_contrasts(model)
    Message
      No variable was specified for contrast estimation. Selecting `contrast =
        "Species"`.
    Output
      Marginal Contrasts Analysis
      
      Level1     |     Level2 | Difference |         95% CI |   SE |  df |     z |      p
      -----------------------------------------------------------------------------------
      setosa     | versicolor |      -0.68 | [-0.86, -0.50] | 0.07 | Inf | -9.27 | < .001
      setosa     |  virginica |      -0.50 | [-0.70, -0.30] | 0.08 | Inf | -5.90 | < .001
      versicolor |  virginica |       0.18 | [-0.02,  0.38] | 0.08 | Inf |  2.12 | 0.034 
      
      Variable predicted: y
      Predictors contrasted: Species
      p-value adjustment method: Holm (1979)
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
      versicolor - virginica |       0.18 | 0.08 | [ 0.01,  0.35] |  2.12 | 0.034 
      
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
      low - high |      -0.61 | 0.43 | [-1.45, 0.24] |  -1.40 | 0.323
      mid - high |      -0.69 | 0.36 | [-1.40, 0.02] |  -1.92 | 0.166
      
      Variable predicted: neg_c_7
      Predictors contrasted: c172code
      Predictors averaged: e16sex, c161sex
      p-value adjustment method: Holm (1979)

---

    Code
      print(estimate_contrasts(fit, c("c161sex", "c172code"), backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      c161sex         |    c172code | Difference |   SE |         95% CI | t(825) |      p
      ------------------------------------------------------------------------------------
      Male - Male     |   low - mid |      -0.29 | 0.71 | [-1.68,  1.10] |  -0.41 | > .999
      Male - Female   |   low - mid |      -0.85 | 0.64 | [-2.10,  0.40] |  -1.33 | > .999
      Female - Female |   low - mid |       0.20 | 0.39 | [-0.57,  0.96] |   0.51 | > .999
      Male - Male     |  low - high |      -0.69 | 0.83 | [-2.32,  0.95] |  -0.82 | > .999
      Male - Female   |  low - high |      -1.65 | 0.71 | [-3.05, -0.24] |  -2.30 | 0.301 
      Female - Female |  low - high |      -0.60 | 0.51 | [-1.59,  0.39] |  -1.18 | > .999
      Male - Female   |   low - low |      -1.05 | 0.69 | [-2.40,  0.31] |  -1.51 | > .999
      Male - Male     |  mid - high |      -0.40 | 0.68 | [-1.73,  0.94] |  -0.59 | > .999
      Male - Female   |  mid - high |      -1.36 | 0.53 | [-2.39, -0.32] |  -2.58 | 0.151 
      Female - Female |  mid - high |      -0.80 | 0.43 | [-1.63,  0.04] |  -1.87 | 0.799 
      Male - Female   |   mid - low |      -0.76 | 0.50 | [-1.73,  0.22] |  -1.52 | > .999
      Male - Female   |   mid - mid |      -0.56 | 0.42 | [-1.38,  0.26] |  -1.35 | > .999
      Male - Female   |  high - low |      -0.36 | 0.66 | [-1.66,  0.95] |  -0.54 | > .999
      Male - Female   |  high - mid |      -0.16 | 0.61 | [-1.35,  1.03] |  -0.27 | > .999
      Male - Female   | high - high |      -0.96 | 0.69 | [-2.30,  0.39] |  -1.40 | > .999
      
      Variable predicted: neg_c_7
      Predictors contrasted: c161sex, c172code
      Predictors averaged: e16sex
      p-value adjustment method: Holm (1979)

---

    Code
      print(estimate_contrasts(fit, "c161sex", "c172code", backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      c161sex       | c172code | Difference |   SE |        95% CI | t(825) |      p
      ------------------------------------------------------------------------------
      Male - Female |      low |      -1.05 | 0.69 | [-2.40, 0.31] |  -1.51 | > .999
      Male - Female |      mid |      -0.56 | 0.42 | [-1.38, 0.26] |  -1.35 | > .999
      Male - Female |     high |      -0.96 | 0.69 | [-2.30, 0.39] |  -1.40 | > .999
      
      Variable predicted: neg_c_7
      Predictors contrasted: c161sex
      Predictors averaged: e16sex
      p-value adjustment method: Holm (1979)

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

---

    Code
      print(estimate_contrasts(fit, "barthtot", "c172code", backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      c172code   | Difference |   SE |        95% CI |     t |     p
      --------------------------------------------------------------
      low - mid  |      -0.01 | 0.01 | [-0.03, 0.01] | -1.17 | 0.728
      low - high |      -0.02 | 0.01 | [-0.04, 0.01] | -1.10 | 0.728
      mid - high |       0.00 | 0.01 | [-0.03, 0.02] | -0.27 | 0.786
      
      Variable predicted: neg_c_7
      Predictors contrasted: barthtot
      Predictors averaged: e16sex, barthtot
      p-value adjustment method: Holm (1979)

---

    Code
      print(estimate_contrasts(fit, "barthtot", c("c172code", "e16sex"), backend = "marginaleffects"),
      table_width = Inf, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      c172code    |          e16sex | Difference |   SE |         95% CI |     t |      p
      -----------------------------------------------------------------------------------
      low - low   |   male - female |       0.01 | 0.02 | [-0.03,  0.04] |  0.35 | > .999
      low - mid   |   male - female |      -0.02 | 0.01 | [-0.05,  0.01] | -1.24 | > .999
      low - high  |   male - female |      -0.02 | 0.02 | [-0.06,  0.01] | -1.19 | > .999
      mid - mid   |   male - female |      -0.03 | 0.01 | [-0.05,  0.00] | -2.24 | 0.381 
      mid - high  |   male - female |      -0.03 | 0.02 | [-0.06,  0.00] | -1.83 | 0.943 
      high - high |   male - female |      -0.03 | 0.02 | [-0.07,  0.02] | -1.11 | > .999
      low - mid   |     male - male |       0.01 | 0.02 | [-0.02,  0.04] |  0.46 | > .999
      low - high  |     male - male |       0.00 | 0.02 | [-0.04,  0.05] |  0.16 | > .999
      mid - high  |     male - male |       0.00 | 0.02 | [-0.05,  0.04] | -0.18 | > .999
      low - mid   |   female - male |       0.00 | 0.02 | [-0.03,  0.03] |  0.08 | > .999
      low - high  |   female - male |       0.00 | 0.02 | [-0.05,  0.04] | -0.12 | > .999
      mid - high  |   female - male |       0.02 | 0.02 | [-0.02,  0.06] |  1.08 | > .999
      low - mid   | female - female |      -0.02 | 0.01 | [-0.05,  0.00] | -1.76 | > .999
      low - high  | female - female |      -0.03 | 0.02 | [-0.06,  0.01] | -1.58 | > .999
      mid - high  | female - female |       0.00 | 0.01 | [-0.03,  0.02] | -0.26 | > .999
      
      Variable predicted: neg_c_7
      Predictors contrasted: barthtot
      Predictors averaged: barthtot
      p-value adjustment method: Holm (1979)

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
      p-value adjustment method: Holm (1979)

---

    Code
      print(estimate_contrasts(m, c("time", "coffee"), backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      time                  |            coffee | Difference |   SE |          95% CI | t(113) |      p
      -------------------------------------------------------------------------------------------------
      morning - morning     |  coffee - control |       5.78 | 1.99 | [  1.83,  9.73] |   2.90 | 0.063 
      morning - noon        |  coffee - control |       0.00 | 1.99 | [ -3.95,  3.95] |   0.00 | > .999
      morning - afternoon   |  coffee - control |       0.00 | 1.99 | [ -3.95,  3.95] |   0.00 | > .999
      noon - noon           |  coffee - control |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 | > .999
      noon - afternoon      |  coffee - control |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 | > .999
      afternoon - afternoon |  coffee - control |       1.93 | 1.99 | [ -2.02,  5.88] |   0.97 | > .999
      morning - noon        |   coffee - coffee |       1.93 | 1.99 | [ -2.02,  5.88] |   0.97 | > .999
      morning - afternoon   |   coffee - coffee |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 | > .999
      noon - afternoon      |   coffee - coffee |      -3.86 | 1.99 | [ -7.81,  0.09] |  -1.93 | 0.612 
      morning - noon        |  control - coffee |      -3.86 | 1.99 | [ -7.81,  0.09] |  -1.93 | 0.612 
      morning - afternoon   |  control - coffee |      -7.71 | 1.99 | [-11.66, -3.76] |  -3.87 | 0.003 
      noon - afternoon      |  control - coffee |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 | > .999
      morning - noon        | control - control |      -5.78 | 1.99 | [ -9.73, -1.83] |  -2.90 | 0.063 
      morning - afternoon   | control - control |      -5.78 | 1.99 | [ -9.73, -1.83] |  -2.90 | 0.063 
      noon - afternoon      | control - control |       0.00 | 1.99 | [ -3.95,  3.95] |   0.00 | > .999
      
      Variable predicted: alertness
      Predictors contrasted: time, coffee
      Predictors averaged: sex
      p-value adjustment method: Holm (1979)

---

    Code
      print(estimate_contrasts(m, contrast = "time", by = "coffee", backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      time                |  coffee | Difference |   SE |         95% CI | t(113) |      p
      ------------------------------------------------------------------------------------
      morning - noon      |  coffee |       1.93 | 1.99 | [-2.02,  5.88] |   0.97 | > .999
      morning - afternoon |  coffee |      -1.93 | 1.99 | [-5.88,  2.02] |  -0.97 | > .999
      noon - afternoon    |  coffee |      -3.86 | 1.99 | [-7.81,  0.09] |  -1.93 | 0.612 
      morning - noon      | control |      -5.78 | 1.99 | [-9.73, -1.83] |  -2.90 | 0.063 
      morning - afternoon | control |      -5.78 | 1.99 | [-9.73, -1.83] |  -2.90 | 0.063 
      noon - afternoon    | control |       0.00 | 1.99 | [-3.95,  3.95] |   0.00 | > .999
      
      Variable predicted: alertness
      Predictors contrasted: time
      Predictors averaged: sex
      p-value adjustment method: Holm (1979)

---

    Code
      print(estimate_contrasts(model, contrast = c("mined", "spp"), backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      mined     |           spp | Difference |   SE |         95% CI |     z |      p
      -------------------------------------------------------------------------------
      yes - yes |       GP - PR |      -0.06 | 0.05 | [-0.16,  0.04] | -1.10 | > .999
      yes - no  |       GP - PR |      -0.51 | 0.16 | [-0.82, -0.20] | -3.21 | 0.049 
      no - no   |       GP - PR |       2.08 | 0.48 | [ 1.14,  3.02] |  4.35 | < .001
      yes - yes |       GP - DM |      -0.32 | 0.11 | [-0.54, -0.10] | -2.89 | 0.117 
      yes - no  |       GP - DM |      -2.86 | 0.61 | [-4.05, -1.68] | -4.73 | < .001
      no - no   |       GP - DM |      -0.27 | 0.37 | [-1.00,  0.46] | -0.73 | > .999
      yes - yes |     GP - EC-A |      -0.04 | 0.05 | [-0.13,  0.05] | -0.80 | > .999
      yes - no  |     GP - EC-A |      -1.10 | 0.27 | [-1.64, -0.57] | -4.03 | 0.003 
      no - no   |     GP - EC-A |       1.49 | 0.41 | [ 0.68,  2.29] |  3.61 | 0.014 
      yes - yes |     GP - EC-L |      -0.17 | 0.08 | [-0.32, -0.02] | -2.19 | 0.622 
      yes - no  |     GP - EC-L |      -4.67 | 0.94 | [-6.52, -2.82] | -4.95 | < .001
      no - no   |     GP - EC-L |      -2.08 | 0.58 | [-3.21, -0.95] | -3.61 | 0.014 
      yes - yes |    GP - DES-L |      -0.43 | 0.14 | [-0.70, -0.17] | -3.19 | 0.051 
      yes - no  |    GP - DES-L |      -4.62 | 0.93 | [-6.45, -2.79] | -4.95 | < .001
      no - no   |    GP - DES-L |      -2.03 | 0.57 | [-3.15, -0.92] | -3.57 | 0.015 
      yes - yes |       GP - DF |      -0.43 | 0.14 | [-0.70, -0.17] | -3.19 | 0.051 
      yes - no  |       GP - DF |      -2.24 | 0.49 | [-3.20, -1.28] | -4.58 | < .001
      no - no   |       GP - DF |       0.35 | 0.35 | [-0.35,  1.04] |  0.98 | > .999
      yes - no  |       GP - GP |      -2.59 | 0.55 | [-3.67, -1.50] | -4.67 | < .001
      yes - yes |       PR - DM |      -0.26 | 0.11 | [-0.48, -0.05] | -2.43 | 0.350 
      yes - no  |       PR - DM |      -2.80 | 0.61 | [-4.00, -1.61] | -4.61 | < .001
      no - no   |       PR - DM |      -2.35 | 0.53 | [-3.39, -1.32] | -4.47 | < .001
      yes - yes |     PR - EC-A |       0.02 | 0.06 | [-0.09,  0.13] |  0.33 | > .999
      yes - no  |     PR - EC-A |      -1.05 | 0.28 | [-1.59, -0.50] | -3.76 | 0.008 
      no - no   |     PR - EC-A |      -0.59 | 0.23 | [-1.05, -0.14] | -2.56 | 0.263 
      yes - yes |     PR - EC-L |      -0.11 | 0.08 | [-0.27,  0.04] | -1.43 | > .999
      yes - no  |     PR - EC-L |      -4.61 | 0.95 | [-6.47, -2.76] | -4.87 | < .001
      no - no   |     PR - EC-L |      -4.16 | 0.86 | [-5.84, -2.49] | -4.87 | < .001
      yes - yes |    PR - DES-L |      -0.38 | 0.13 | [-0.64, -0.12] | -2.86 | 0.123 
      yes - no  |    PR - DES-L |      -4.56 | 0.94 | [-6.40, -2.73] | -4.87 | < .001
      no - no   |    PR - DES-L |      -4.11 | 0.85 | [-5.77, -2.45] | -4.86 | < .001
      yes - yes |       PR - DF |      -0.38 | 0.13 | [-0.64, -0.12] | -2.86 | 0.123 
      yes - no  |       PR - DF |      -2.18 | 0.49 | [-3.15, -1.22] | -4.44 | < .001
      no - no   |       PR - DF |      -1.73 | 0.42 | [-2.55, -0.92] | -4.15 | 0.002 
      yes - no  |       PR - GP |      -2.53 | 0.56 | [-3.62, -1.44] | -4.54 | < .001
      yes - no  |       PR - PR |      -0.45 | 0.16 | [-0.77, -0.13] | -2.75 | 0.162 
      yes - yes |     DM - EC-A |       0.28 | 0.11 | [ 0.07,  0.50] |  2.59 | 0.252 
      yes - no  |     DM - EC-A |      -0.78 | 0.31 | [-1.38, -0.18] | -2.56 | 0.263 
      no - no   |     DM - EC-A |       1.76 | 0.46 | [ 0.87,  2.65] |  3.86 | 0.006 
      yes - yes |     DM - EC-L |       0.15 | 0.11 | [-0.06,  0.36] |  1.39 | > .999
      yes - no  |     DM - EC-L |      -4.35 | 0.96 | [-6.24, -2.46] | -4.51 | < .001
      no - no   |     DM - EC-L |      -1.81 | 0.55 | [-2.89, -0.73] | -3.29 | 0.040 
      yes - yes |    DM - DES-L |      -0.11 | 0.13 | [-0.36,  0.14] | -0.89 | > .999
      yes - no  |    DM - DES-L |      -4.30 | 0.95 | [-6.17, -2.43] | -4.51 | < .001
      no - no   |    DM - DES-L |      -1.76 | 0.54 | [-2.82, -0.70] | -3.24 | 0.046 
      yes - yes |       DM - DF |      -0.11 | 0.13 | [-0.36,  0.14] | -0.89 | > .999
      yes - no  |       DM - DF |      -1.92 | 0.51 | [-2.93, -0.91] | -3.74 | 0.009 
      no - no   |       DM - DF |       0.62 | 0.38 | [-0.12,  1.36] |  1.65 | > .999
      yes - no  |       DM - GP |      -2.27 | 0.58 | [-3.40, -1.14] | -3.93 | 0.005 
      yes - no  |       DM - PR |      -0.19 | 0.20 | [-0.58,  0.21] | -0.93 | > .999
      yes - no  |       DM - DM |      -2.54 | 0.63 | [-3.77, -1.31] | -4.04 | 0.003 
      yes - yes |   EC-A - EC-L |      -0.13 | 0.08 | [-0.29,  0.02] | -1.68 | > .999
      yes - no  |   EC-A - EC-L |      -4.63 | 0.95 | [-6.48, -2.78] | -4.90 | < .001
      no - no   |   EC-A - EC-L |      -3.57 | 0.77 | [-5.07, -2.07] | -4.66 | < .001
      yes - yes |  EC-A - DES-L |      -0.40 | 0.13 | [-0.66, -0.14] | -2.97 | 0.097 
      yes - no  |  EC-A - DES-L |      -4.58 | 0.94 | [-6.42, -2.75] | -4.90 | < .001
      no - no   |  EC-A - DES-L |      -3.52 | 0.76 | [-5.00, -2.03] | -4.65 | < .001
      yes - yes |     EC-A - DF |      -0.40 | 0.13 | [-0.66, -0.14] | -2.97 | 0.097 
      yes - no  |     EC-A - DF |      -2.20 | 0.49 | [-3.17, -1.24] | -4.49 | < .001
      no - no   |     EC-A - DF |      -1.14 | 0.36 | [-1.85, -0.43] | -3.16 | 0.053 
      yes - no  |     EC-A - GP |      -2.55 | 0.56 | [-3.64, -1.46] | -4.58 | < .001
      yes - no  |     EC-A - PR |      -0.47 | 0.16 | [-0.79, -0.15] | -2.90 | 0.116 
      yes - no  |     EC-A - DM |      -2.82 | 0.61 | [-4.01, -1.63] | -4.65 | < .001
      yes - no  |   EC-A - EC-A |      -1.06 | 0.28 | [-1.61, -0.52] | -3.85 | 0.006 
      yes - yes |  EC-L - DES-L |      -0.26 | 0.13 | [-0.51, -0.02] | -2.08 | 0.770 
      yes - no  |  EC-L - DES-L |      -4.45 | 0.94 | [-6.30, -2.60] | -4.71 | < .001
      no - no   |  EC-L - DES-L |       0.05 | 0.48 | [-0.89,  0.99] |  0.10 | > .999
      yes - yes |     EC-L - DF |      -0.26 | 0.13 | [-0.51, -0.02] | -2.08 | 0.770 
      yes - no  |     EC-L - DF |      -2.07 | 0.50 | [-3.05, -1.09] | -4.14 | 0.002 
      no - no   |     EC-L - DF |       2.43 | 0.61 | [ 1.22,  3.63] |  3.95 | 0.004 
      yes - no  |     EC-L - GP |      -2.42 | 0.57 | [-3.53, -1.31] | -4.28 | 0.001 
      yes - no  |     EC-L - PR |      -0.34 | 0.18 | [-0.69,  0.01] | -1.89 | 0.991 
      yes - no  |     EC-L - DM |      -2.69 | 0.62 | [-3.90, -1.48] | -4.37 | < .001
      yes - no  |   EC-L - EC-A |      -0.93 | 0.29 | [-1.50, -0.37] | -3.23 | 0.047 
      yes - no  |   EC-L - EC-L |      -4.50 | 0.95 | [-6.37, -2.63] | -4.72 | < .001
      yes - yes |    DES-L - DF |       0.00 | 0.13 | [-0.26,  0.26] |  0.00 | > .999
      yes - no  |    DES-L - DF |      -1.81 | 0.52 | [-2.83, -0.78] | -3.45 | 0.024 
      no - no   |    DES-L - DF |       2.38 | 0.61 | [ 1.19,  3.57] |  3.92 | 0.005 
      yes - no  |    DES-L - GP |      -2.15 | 0.59 | [-3.31, -1.00] | -3.67 | 0.012 
      yes - no  |    DES-L - PR |      -0.07 | 0.22 | [-0.50,  0.36] | -0.33 | > .999
      yes - no  |    DES-L - DM |      -2.43 | 0.64 | [-3.68, -1.18] | -3.81 | 0.007 
      yes - no  |  DES-L - EC-A |      -0.67 | 0.32 | [-1.29, -0.04] | -2.09 | 0.770 
      yes - no  |  DES-L - EC-L |      -4.24 | 0.97 | [-6.14, -2.33] | -4.36 | < .001
      yes - no  | DES-L - DES-L |      -4.19 | 0.96 | [-6.07, -2.30] | -4.35 | < .001
      yes - no  |       DF - GP |      -2.15 | 0.59 | [-3.31, -1.00] | -3.67 | 0.012 
      yes - no  |       DF - PR |      -0.07 | 0.22 | [-0.50,  0.36] | -0.33 | > .999
      yes - no  |       DF - DM |      -2.43 | 0.64 | [-3.68, -1.18] | -3.81 | 0.007 
      yes - no  |     DF - EC-A |      -0.67 | 0.32 | [-1.29, -0.04] | -2.09 | 0.770 
      yes - no  |     DF - EC-L |      -4.24 | 0.97 | [-6.14, -2.33] | -4.36 | < .001
      yes - no  |    DF - DES-L |      -4.19 | 0.96 | [-6.07, -2.30] | -4.35 | < .001
      yes - no  |       DF - DF |      -1.81 | 0.52 | [-2.83, -0.78] | -3.45 | 0.024 
      
      Variable predicted: count
      Predictors contrasted: mined, spp
      Predictors averaged: cover, site
      p-value adjustment method: Holm (1979)
      Contrasts are on the response-scale.

---

    Code
      print(estimate_contrasts(model, contrast = "mined", by = "spp", backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      mined    |   spp | Difference |   SE |         95% CI |     z |      p
      ----------------------------------------------------------------------
      yes - no |    GP |      -2.59 | 0.55 | [-3.67, -1.50] | -4.67 | < .001
      yes - no |    PR |      -0.45 | 0.16 | [-0.77, -0.13] | -2.75 | 0.162 
      yes - no |    DM |      -2.54 | 0.63 | [-3.77, -1.31] | -4.04 | 0.003 
      yes - no |  EC-A |      -1.06 | 0.28 | [-1.61, -0.52] | -3.85 | 0.006 
      yes - no |  EC-L |      -4.50 | 0.95 | [-6.37, -2.63] | -4.72 | < .001
      yes - no | DES-L |      -4.19 | 0.96 | [-6.07, -2.30] | -4.35 | < .001
      yes - no |    DF |      -1.81 | 0.52 | [-2.83, -0.78] | -3.45 | 0.024 
      
      Variable predicted: count
      Predictors contrasted: mined
      Predictors averaged: cover, site
      p-value adjustment method: Holm (1979)
      Contrasts are on the response-scale.

