# estimate_contrasts - Frequentist

    Code
      print(estimate_contrasts(model, contrast = c("three", "vs", "am"), backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1              | Level2              | Difference |   SE |           95% CI | t(24) |      p
      -------------------------------------------------------------------------------------------------
      three 0, vs 0, am 1 | three 0, vs 0, am 0 |       2.87 | 2.24 | [ -1.76,   7.49] |  1.28 |  0.213
      three 0, vs 1, am 0 | three 0, vs 0, am 0 |       5.83 | 2.46 | [  0.76,  10.90] |  2.37 |  0.026
      three 0, vs 1, am 0 | three 0, vs 0, am 1 |       2.97 | 2.65 | [ -2.51,   8.44] |  1.12 |  0.275
      three 0, vs 1, am 1 | three 0, vs 0, am 0 |      14.03 | 2.10 | [  9.69,  18.37] |  6.67 | < .001
      three 0, vs 1, am 1 | three 0, vs 0, am 1 |      11.16 | 2.33 | [  6.35,  15.97] |  4.79 | < .001
      three 0, vs 1, am 1 | three 0, vs 1, am 0 |       8.19 | 2.54 | [  2.96,  13.43] |  3.23 |  0.004
      three 1, vs 0, am 0 | three 0, vs 0, am 0 |      -0.57 | 2.01 | [ -4.71,   3.57] | -0.28 |  0.780
      three 1, vs 0, am 0 | three 0, vs 0, am 1 |      -3.43 | 2.24 | [ -8.06,   1.19] | -1.53 |  0.139
      three 1, vs 0, am 0 | three 0, vs 1, am 0 |      -6.40 | 2.46 | [-11.47,  -1.33] | -2.61 |  0.016
      three 1, vs 0, am 0 | three 0, vs 1, am 1 |     -14.59 | 2.10 | [-18.93, -10.25] | -6.94 | < .001
      three 1, vs 0, am 1 | three 0, vs 0, am 0 |       7.52 | 2.84 | [  1.66,  13.37] |  2.65 |  0.014
      three 1, vs 0, am 1 | three 0, vs 0, am 1 |       4.65 | 3.01 | [ -1.56,  10.86] |  1.55 |  0.135
      three 1, vs 0, am 1 | three 0, vs 1, am 0 |       1.68 | 3.17 | [ -4.86,   8.23] |  0.53 |  0.600
      three 1, vs 0, am 1 | three 0, vs 1, am 1 |      -6.51 | 2.91 | [-12.51,  -0.51] | -2.24 |  0.035
      three 1, vs 0, am 1 | three 1, vs 0, am 0 |       8.08 | 2.84 | [  2.23,  13.94] |  2.85 |  0.009
      three 1, vs 1, am 0 | three 0, vs 0, am 0 |       5.09 | 2.24 | [  0.46,   9.72] |  2.27 |  0.032
      three 1, vs 1, am 0 | three 0, vs 0, am 1 |       2.23 | 2.46 | [ -2.84,   7.29] |  0.91 |  0.374
      three 1, vs 1, am 0 | three 0, vs 1, am 0 |      -0.74 | 2.65 | [ -6.22,   4.73] | -0.28 |  0.782
      three 1, vs 1, am 0 | three 0, vs 1, am 1 |      -8.94 | 2.33 | [-13.74,  -4.13] | -3.83 | < .001
      three 1, vs 1, am 0 | three 1, vs 0, am 0 |       5.66 | 2.24 | [  1.03,  10.29] |  2.52 |  0.019
      three 1, vs 1, am 0 | three 1, vs 0, am 1 |      -2.42 | 3.01 | [ -8.63,   3.78] | -0.81 |  0.428
      three 1, vs 1, am 1 | three 0, vs 0, am 0 |      10.57 | 2.84 | [  4.71,  16.42] |  3.73 |  0.001
      three 1, vs 1, am 1 | three 0, vs 0, am 1 |       7.70 | 3.01 | [  1.49,  13.91] |  2.56 |  0.017
      three 1, vs 1, am 1 | three 0, vs 1, am 0 |       4.73 | 3.17 | [ -1.81,  11.28] |  1.49 |  0.149
      three 1, vs 1, am 1 | three 0, vs 1, am 1 |      -3.46 | 2.91 | [ -9.46,   2.54] | -1.19 |  0.246
      three 1, vs 1, am 1 | three 1, vs 0, am 0 |      11.13 | 2.84 | [  5.28,  16.99] |  3.93 | < .001
      three 1, vs 1, am 1 | three 1, vs 0, am 1 |       3.05 | 3.47 | [ -4.12,  10.22] |  0.88 |  0.389
      three 1, vs 1, am 1 | three 1, vs 1, am 0 |       5.48 | 3.01 | [ -0.73,  11.68] |  1.82 |  0.081
      
      Variable predicted: mpg
      Predictors contrasted: three, vs, am
      p-values are uncorrected.

---

    Code
      print(estimate_contrasts(model, contrast = "am", backend = "marginaleffects"),
      zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1 | Level2 | Difference |   SE |       95% CI | t(24) |      p
      -------------------------------------------------------------------
      1      | 0      |       6.15 | 1.34 | [3.40, 8.91] |  4.61 | < .001
      
      Variable predicted: mpg
      Predictors contrasted: am
      Predictors averaged: three, vs
      p-values are uncorrected.

