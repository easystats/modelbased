# estimate_contrasts - Frequentist, duplicated levels

    Code
      print(estimate_contrasts(model, contrast = c("three", "vs", "am"), backend = "marginaleffects"),
      digits = 1, zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Level1              | Level2              | Difference |  SE |         95% CI | t(24) |      p
      ----------------------------------------------------------------------------------------------
      three 0, vs 0, am 1 | three 0, vs 0, am 0 |        2.9 | 2.2 | [ -1.8,   7.5] |   1.3 |  0.213
      three 0, vs 1, am 0 | three 0, vs 0, am 0 |        5.8 | 2.5 | [  0.8,  10.9] |   2.4 |  0.026
      three 0, vs 1, am 1 | three 0, vs 0, am 0 |       14.0 | 2.1 | [  9.7,  18.4] |   6.7 | < .001
      three 1, vs 0, am 0 | three 0, vs 0, am 0 |       -0.6 | 2.0 | [ -4.7,   3.6] |  -0.3 |  0.780
      three 1, vs 0, am 1 | three 0, vs 0, am 0 |        7.5 | 2.8 | [  1.7,  13.4] |   2.7 |  0.014
      three 1, vs 1, am 0 | three 0, vs 0, am 0 |        5.1 | 2.2 | [  0.5,   9.7] |   2.3 |  0.032
      three 1, vs 1, am 1 | three 0, vs 0, am 0 |       10.6 | 2.8 | [  4.7,  16.4] |   3.7 |  0.001
      three 0, vs 1, am 0 | three 0, vs 0, am 1 |        3.0 | 2.7 | [ -2.5,   8.4] |   1.1 |  0.275
      three 0, vs 1, am 1 | three 0, vs 0, am 1 |       11.2 | 2.3 | [  6.4,  16.0] |   4.8 | < .001
      three 1, vs 0, am 0 | three 0, vs 0, am 1 |       -3.4 | 2.2 | [ -8.1,   1.2] |  -1.5 |  0.139
      three 1, vs 0, am 1 | three 0, vs 0, am 1 |        4.7 | 3.0 | [ -1.6,  10.9] |   1.5 |  0.135
      three 1, vs 1, am 0 | three 0, vs 0, am 1 |        2.2 | 2.5 | [ -2.8,   7.3] |   0.9 |  0.374
      three 1, vs 1, am 1 | three 0, vs 0, am 1 |        7.7 | 3.0 | [  1.5,  13.9] |   2.6 |  0.017
      three 0, vs 1, am 1 | three 0, vs 1, am 0 |        8.2 | 2.5 | [  3.0,  13.4] |   3.2 |  0.004
      three 1, vs 0, am 0 | three 0, vs 1, am 0 |       -6.4 | 2.5 | [-11.5,  -1.3] |  -2.6 |  0.016
      three 1, vs 0, am 1 | three 0, vs 1, am 0 |        1.7 | 3.2 | [ -4.9,   8.2] |   0.5 |  0.600
      three 1, vs 1, am 0 | three 0, vs 1, am 0 |       -0.7 | 2.7 | [ -6.2,   4.7] |  -0.3 |  0.782
      three 1, vs 1, am 1 | three 0, vs 1, am 0 |        4.7 | 3.2 | [ -1.8,  11.3] |   1.5 |  0.149
      three 1, vs 0, am 0 | three 0, vs 1, am 1 |      -14.6 | 2.1 | [-18.9, -10.3] |  -6.9 | < .001
      three 1, vs 0, am 1 | three 0, vs 1, am 1 |       -6.5 | 2.9 | [-12.5,  -0.5] |  -2.2 |  0.035
      three 1, vs 1, am 0 | three 0, vs 1, am 1 |       -8.9 | 2.3 | [-13.7,  -4.1] |  -3.8 | < .001
      three 1, vs 1, am 1 | three 0, vs 1, am 1 |       -3.5 | 2.9 | [ -9.5,   2.5] |  -1.2 |  0.246
      three 1, vs 0, am 1 | three 1, vs 0, am 0 |        8.1 | 2.8 | [  2.2,  13.9] |   2.8 |  0.009
      three 1, vs 1, am 0 | three 1, vs 0, am 0 |        5.7 | 2.2 | [  1.0,  10.3] |   2.5 |  0.019
      three 1, vs 1, am 1 | three 1, vs 0, am 0 |       11.1 | 2.8 | [  5.3,  17.0] |   3.9 | < .001
      three 1, vs 1, am 0 | three 1, vs 0, am 1 |       -2.4 | 3.0 | [ -8.6,   3.8] |  -0.8 |  0.428
      three 1, vs 1, am 1 | three 1, vs 0, am 1 |        3.1 | 3.5 | [ -4.1,  10.2] |   0.9 |  0.389
      three 1, vs 1, am 1 | three 1, vs 1, am 0 |        5.5 | 3.0 | [ -0.7,  11.7] |   1.8 |  0.081
      
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

