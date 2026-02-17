# estimate_contrasts - Frequentist, Three factors 1

    Code
      print(estimate_contrasts(model, contrast = c("vs", "am"), by = "gear", backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | gear | Difference |   SE |         95% CI | t(25) |      p
      ------------------------------------------------------------------------------------
      vs 0, am 1 | vs 0, am 0 | 3    |       6.98 | 2.33 | [ 2.17, 11.79] |  2.99 |  0.006
      vs 1, am 0 | vs 0, am 0 | 3    |       5.28 | 2.33 | [ 0.47, 10.09] |  2.26 |  0.033
      vs 1, am 1 | vs 0, am 0 | 3    |      12.27 | 3.30 | [ 5.47, 19.07] |  3.71 |  0.001
      vs 1, am 0 | vs 0, am 1 | 3    |      -1.70 | 3.30 | [-8.50,  5.10] | -0.51 |  0.611
      vs 1, am 1 | vs 0, am 1 | 3    |       5.28 | 2.33 | [ 0.47, 10.09] |  2.26 |  0.033
      vs 1, am 1 | vs 1, am 0 | 3    |       6.98 | 2.33 | [ 2.17, 11.79] |  2.99 |  0.006
      vs 0, am 1 | vs 0, am 0 | 4    |       6.98 | 2.33 | [ 2.17, 11.79] |  2.99 |  0.006
      vs 1, am 0 | vs 0, am 0 | 4    |       7.03 | 2.95 | [ 0.95, 13.12] |  2.38 |  0.025
      vs 1, am 1 | vs 0, am 0 | 4    |      14.02 | 4.31 | [ 5.15, 22.88] |  3.26 |  0.003
      vs 1, am 0 | vs 0, am 1 | 4    |       0.05 | 3.13 | [-6.40,  6.50] |  0.02 |  0.987
      vs 1, am 1 | vs 0, am 1 | 4    |       7.03 | 2.95 | [ 0.95, 13.12] |  2.38 |  0.025
      vs 1, am 1 | vs 1, am 0 | 4    |       6.98 | 2.33 | [ 2.17, 11.79] |  2.99 |  0.006
      vs 0, am 1 | vs 0, am 0 | 5    |       6.98 | 2.33 | [ 2.17, 11.79] |  2.99 |  0.006
      vs 1, am 0 | vs 0, am 0 | 5    |      11.27 | 4.04 | [ 2.95, 19.60] |  2.79 |  0.010
      vs 1, am 1 | vs 0, am 0 | 5    |      18.26 | 4.67 | [ 8.64, 27.88] |  3.91 | < .001
      vs 1, am 0 | vs 0, am 1 | 5    |       4.29 | 4.67 | [-5.33, 13.91] |  0.92 |  0.367
      vs 1, am 1 | vs 0, am 1 | 5    |      11.27 | 4.04 | [ 2.95, 19.60] |  2.79 |  0.010
      vs 1, am 1 | vs 1, am 0 | 5    |       6.98 | 2.33 | [ 2.17, 11.79] |  2.99 |  0.006
      
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
      
      Level1    | Level2  | coffee  | Ratio |   SE |       95% CI | t(113) |     p
      ----------------------------------------------------------------------------
      noon      | morning | coffee  |  0.89 | 0.11 | [0.67, 1.11] |  -1.02 | 0.309
      afternoon | morning | coffee  |  1.11 | 0.12 | [0.87, 1.36] |   0.91 | 0.363
      noon      | morning | control |  1.51 | 0.22 | [1.06, 1.95] |   2.27 | 0.025
      afternoon | morning | control |  1.51 | 0.22 | [1.06, 1.95] |   2.27 | 0.025
      
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
      
      Level1     | Level2     | Difference |         95% CI |   SE |     z |      p
      -----------------------------------------------------------------------------
      setosa     | versicolor |      -0.68 | [-0.82, -0.54] | 0.07 | -9.27 | < .001
      setosa     | virginica  |      -0.50 | [-0.67, -0.33] | 0.08 | -5.90 | < .001
      versicolor | virginica  |       0.18 | [ 0.01,  0.35] | 0.08 |  2.12 |  0.034
      
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
      
      Level1     | Level2     | Difference |         95% CI |   SE |     z |      p
      -----------------------------------------------------------------------------
      setosa     | versicolor |      -0.68 | [-0.86, -0.50] | 0.07 | -9.27 | < .001
      setosa     | virginica  |      -0.50 | [-0.70, -0.30] | 0.08 | -5.90 | < .001
      versicolor | virginica  |       0.18 | [-0.02,  0.38] | 0.08 |  2.12 |  0.034
      
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

