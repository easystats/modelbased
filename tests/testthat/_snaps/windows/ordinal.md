# estimate_relation prints ordinal models correctly

    Code
      print(out, zap_small = TRUE)
    Output
      Model-based Predictions
      
      Row | Response   | Sepal.Width | Predicted |   SE |       95% CI
      ----------------------------------------------------------------
      1   | setosa     |        2.00 |      0.00 | 0.00 | [0.00, 0.01]
      1   | versicolor |        2.00 |      0.85 | 0.08 | [0.65, 0.95]
      1   | virginica  |        2.00 |      0.15 | 0.08 | [0.05, 0.34]
      2   | setosa     |        2.27 |      0.01 | 0.01 | [0.00, 0.02]
      2   | versicolor |        2.27 |      0.77 | 0.08 | [0.60, 0.90]
      2   | virginica  |        2.27 |      0.22 | 0.08 | [0.10, 0.40]
      3   | setosa     |        2.53 |      0.02 | 0.01 | [0.01, 0.06]
      3   | versicolor |        2.53 |      0.65 | 0.07 | [0.52, 0.78]
      3   | virginica  |        2.53 |      0.32 | 0.07 | [0.20, 0.45]
      4   | setosa     |        2.80 |      0.09 | 0.03 | [0.04, 0.16]
      4   | versicolor |        2.80 |      0.49 | 0.05 | [0.39, 0.59]
      4   | virginica  |        2.80 |      0.42 | 0.05 | [0.32, 0.52]
      5   | setosa     |        3.07 |      0.27 | 0.05 | [0.19, 0.36]
      5   | versicolor |        3.07 |      0.29 | 0.05 | [0.21, 0.39]
      5   | virginica  |        3.07 |      0.43 | 0.05 | [0.33, 0.53]
      6   | setosa     |        3.33 |      0.58 | 0.06 | [0.47, 0.68]
      6   | versicolor |        3.33 |      0.12 | 0.04 | [0.06, 0.21]
      6   | virginica  |        3.33 |      0.30 | 0.05 | [0.20, 0.40]
      7   | setosa     |        3.60 |      0.82 | 0.06 | [0.68, 0.91]
      7   | versicolor |        3.60 |      0.04 | 0.02 | [0.01, 0.08]
      7   | virginica  |        3.60 |      0.15 | 0.05 | [0.07, 0.26]
      8   | setosa     |        3.87 |      0.93 | 0.04 | [0.83, 0.98]
      8   | versicolor |        3.87 |      0.01 | 0.01 | [0.00, 0.03]
      8   | virginica  |        3.87 |      0.06 | 0.04 | [0.02, 0.15]
      9   | setosa     |        4.13 |      0.97 | 0.02 | [0.91, 1.00]
      9   | versicolor |        4.13 |      0.00 | 0.00 | [0.00, 0.01]
      9   | virginica  |        4.13 |      0.02 | 0.02 | [0.00, 0.08]
      10  | setosa     |        4.40 |      0.99 | 0.01 | [0.95, 1.00]
      10  | versicolor |        4.40 |      0.00 | 0.00 | [0.00, 0.00]
      10  | virginica  |        4.40 |      0.01 | 0.01 | [0.00, 0.04]
      
      Variable predicted: Species
      Predictors modulated: Sepal.Width
      Predictions are on the response-scale.

---

    Code
      print(out, zap_small = TRUE)
    Output
      Estimated Marginal Means
      
      Sepal.Width |   Response | Median |       95% CI |   pd |          ROPE | % in ROPE
      -----------------------------------------------------------------------------------
      2.00        |     setosa | 0.00   | [0.00, 0.01] | 100% | [-0.10, 0.10] |      100%
      2.00        | versicolor | 0.86   | [0.65, 0.95] | 100% | [-0.10, 0.10] |        0%
      2.00        |  virginica | 0.14   | [0.05, 0.34] | 100% | [-0.10, 0.10] |    28.00%
      2.27        |     setosa | 0.00   | [0.00, 0.02] | 100% | [-0.10, 0.10] |      100%
      2.27        | versicolor | 0.78   | [0.60, 0.90] | 100% | [-0.10, 0.10] |        0%
      2.27        |  virginica | 0.21   | [0.10, 0.40] | 100% | [-0.10, 0.10] |        0%
      2.53        |     setosa | 0.02   | [0.01, 0.06] | 100% | [-0.10, 0.10] |      100%
      2.53        | versicolor | 0.66   | [0.52, 0.78] | 100% | [-0.10, 0.10] |        0%
      2.53        |  virginica | 0.32   | [0.20, 0.45] | 100% | [-0.10, 0.10] |        0%
      2.80        |     setosa | 0.09   | [0.04, 0.16] | 100% | [-0.10, 0.10] |    65.72%
      2.80        | versicolor | 0.49   | [0.39, 0.59] | 100% | [-0.10, 0.10] |        0%
      2.80        |  virginica | 0.41   | [0.32, 0.52] | 100% | [-0.10, 0.10] |        0%
      3.07        |     setosa | 0.27   | [0.19, 0.36] | 100% | [-0.10, 0.10] |        0%
      3.07        | versicolor | 0.30   | [0.21, 0.39] | 100% | [-0.10, 0.10] |        0%
      3.07        |  virginica | 0.43   | [0.33, 0.53] | 100% | [-0.10, 0.10] |        0%
      3.33        |     setosa | 0.57   | [0.47, 0.68] | 100% | [-0.10, 0.10] |        0%
      3.33        | versicolor | 0.12   | [0.06, 0.21] | 100% | [-0.10, 0.10] |    29.76%
      3.33        |  virginica | 0.30   | [0.20, 0.40] | 100% | [-0.10, 0.10] |        0%
      3.60        |     setosa | 0.82   | [0.68, 0.91] | 100% | [-0.10, 0.10] |        0%
      3.60        | versicolor | 0.03   | [0.01, 0.08] | 100% | [-0.10, 0.10] |      100%
      3.60        |  virginica | 0.14   | [0.07, 0.26] | 100% | [-0.10, 0.10] |    17.37%
      3.87        |     setosa | 0.94   | [0.83, 0.98] | 100% | [-0.10, 0.10] |        0%
      3.87        | versicolor | 0.01   | [0.00, 0.03] | 100% | [-0.10, 0.10] |      100%
      3.87        |  virginica | 0.05   | [0.02, 0.15] | 100% | [-0.10, 0.10] |    89.59%
      4.13        |     setosa | 0.98   | [0.91, 1.00] | 100% | [-0.10, 0.10] |        0%
      4.13        | versicolor | 0.00   | [0.00, 0.01] | 100% | [-0.10, 0.10] |      100%
      4.13        |  virginica | 0.02   | [0.00, 0.08] | 100% | [-0.10, 0.10] |      100%
      4.40        |     setosa | 0.99   | [0.95, 1.00] | 100% | [-0.10, 0.10] |        0%
      4.40        | versicolor | 0.00   | [0.00, 0.00] | 100% | [-0.10, 0.10] |      100%
      4.40        |  virginica | 0.01   | [0.00, 0.04] | 100% | [-0.10, 0.10] |      100%
      
      Variable predicted: Species
      Predictors modulated: Sepal.Width
      Predictions are on the response-scale.

---

    Code
      print(out, zap_small = TRUE)
    Output
      Model-based Predictions
      
      Row | Response   | Sepal.Width | Predicted |       95% CI
      ---------------------------------------------------------
      1   | setosa     |        2.00 |      0.05 | [0.01, 0.22]
      1   | versicolor |        2.00 |      0.17 | [0.04, 0.51]
      1   | virginica  |        2.00 |      0.78 | [0.41, 0.95]
      2   | setosa     |        2.27 |      0.09 | [0.02, 0.38]
      2   | versicolor |        2.27 |      0.24 | [0.05, 0.67]
      2   | virginica  |        2.27 |      0.67 | [0.24, 0.93]
      3   | setosa     |        2.53 |      0.15 | [0.02, 0.57]
      3   | versicolor |        2.53 |      0.32 | [0.06, 0.78]
      3   | virginica  |        2.53 |      0.54 | [0.13, 0.90]
      4   | setosa     |        2.80 |      0.23 | [0.03, 0.74]
      4   | versicolor |        2.80 |      0.37 | [0.06, 0.85]
      4   | virginica  |        2.80 |      0.40 | [0.06, 0.86]
      5   | setosa     |        3.07 |      0.34 | [0.04, 0.86]
      5   | versicolor |        3.07 |      0.38 | [0.05, 0.88]
      5   | virginica  |        3.07 |      0.28 | [0.03, 0.82]
      6   | setosa     |        3.33 |      0.47 | [0.06, 0.93]
      6   | versicolor |        3.33 |      0.35 | [0.03, 0.89]
      6   | virginica  |        3.33 |      0.18 | [0.01, 0.77]
      7   | setosa     |        3.60 |      0.61 | [0.08, 0.97]
      7   | versicolor |        3.60 |      0.28 | [0.02, 0.88]
      7   | virginica  |        3.60 |      0.11 | [0.01, 0.70]
      8   | setosa     |        3.87 |      0.73 | [0.11, 0.98]
      8   | versicolor |        3.87 |      0.20 | [0.01, 0.85]
      8   | virginica  |        3.87 |      0.07 | [0.00, 0.63]
      9   | setosa     |        4.13 |      0.82 | [0.14, 0.99]
      9   | versicolor |        4.13 |      0.14 | [0.01, 0.82]
      9   | virginica  |        4.13 |      0.04 | [0.00, 0.55]
      10  | setosa     |        4.40 |      0.89 | [0.19, 1.00]
      10  | versicolor |        4.40 |      0.09 | [0.00, 0.77]
      10  | virginica  |        4.40 |      0.02 | [0.00, 0.46]
      
      Variable predicted: Species
      Predictors modulated: Sepal.Width
      Predictions are on the response-scale.

---

    Code
      print(out, zap_small = TRUE)
    Output
      Estimated Marginal Means
      
      Sepal.Width | Response   | Probability |   SE |        95% CI | t(147)
      ----------------------------------------------------------------------
      2.00        | setosa     |        0.05 | 0.02 | [ 0.01, 0.10] |   2.21
      2.00        | versicolor |        0.17 | 0.05 | [ 0.07, 0.27] |   3.22
      2.00        | virginica  |        0.78 | 0.07 | [ 0.63, 0.92] |  10.56
      2.27        | setosa     |        0.09 | 0.03 | [ 0.03, 0.15] |   2.89
      2.27        | versicolor |        0.24 | 0.05 | [ 0.14, 0.34] |   4.89
      2.27        | virginica  |        0.67 | 0.07 | [ 0.52, 0.81] |   9.15
      2.53        | setosa     |        0.15 | 0.04 | [ 0.07, 0.22] |   4.05
      2.53        | versicolor |        0.32 | 0.04 | [ 0.23, 0.40] |   7.35
      2.53        | virginica  |        0.54 | 0.06 | [ 0.42, 0.66] |   8.81
      2.80        | setosa     |        0.23 | 0.04 | [ 0.15, 0.30] |   5.96
      2.80        | versicolor |        0.37 | 0.04 | [ 0.29, 0.46] |   8.71
      2.80        | virginica  |        0.40 | 0.05 | [ 0.31, 0.49] |   8.76
      3.07        | setosa     |        0.34 | 0.04 | [ 0.26, 0.42] |   8.09
      3.07        | versicolor |        0.38 | 0.04 | [ 0.30, 0.47] |   8.73
      3.07        | virginica  |        0.28 | 0.04 | [ 0.20, 0.35] |   7.23
      3.33        | setosa     |        0.47 | 0.05 | [ 0.36, 0.58] |   8.72
      3.33        | versicolor |        0.35 | 0.04 | [ 0.26, 0.43] |   8.10
      3.33        | virginica  |        0.18 | 0.04 | [ 0.11, 0.25] |   4.94
      3.60        | setosa     |        0.61 | 0.07 | [ 0.47, 0.74] |   8.83
      3.60        | versicolor |        0.28 | 0.05 | [ 0.19, 0.37] |   5.91
      3.60        | virginica  |        0.11 | 0.03 | [ 0.05, 0.18] |   3.40
      3.87        | setosa     |        0.73 | 0.07 | [ 0.58, 0.88] |   9.72
      3.87        | versicolor |        0.20 | 0.05 | [ 0.10, 0.31] |   3.85
      3.87        | virginica  |        0.07 | 0.03 | [ 0.01, 0.12] |   2.51
      4.13        | setosa     |        0.82 | 0.07 | [ 0.69, 0.96] |  11.84
      4.13        | versicolor |        0.14 | 0.05 | [ 0.04, 0.24] |   2.66
      4.13        | virginica  |        0.04 | 0.02 | [ 0.00, 0.08] |   1.97
      4.40        | setosa     |        0.89 | 0.06 | [ 0.78, 1.00] |  15.64
      4.40        | versicolor |        0.09 | 0.04 | [ 0.00, 0.17] |   2.00
      4.40        | virginica  |        0.02 | 0.01 | [-0.01, 0.05] |   1.62
      
      Variable predicted: Species
      Predictors modulated: Sepal.Width
      Predictions are on the probs-scale.

# estimate_means, print bracl

    Code
      print(out, zap_small = TRUE)
    Output
      Estimated Marginal Means
      
      island    | Response  | Probability |   SE |        95% CI |     z
      ------------------------------------------------------------------
      Biscoe    | Adelie    |        0.27 | 0.02 | [ 0.24, 0.30] | 17.09
      Biscoe    | Chinstrap |        0.00 | 0.00 | [ 0.00, 0.00] | 22.91
      Biscoe    | Gentoo    |        0.72 | 0.02 | [ 0.69, 0.76] | 45.96
      Dream     | Adelie    |        0.45 | 0.01 | [ 0.43, 0.46] | 52.85
      Dream     | Chinstrap |        0.55 | 0.02 | [ 0.51, 0.59] | 29.50
      Dream     | Gentoo    |        0.00 | 0.01 | [-0.02, 0.02] |  0.40
      Torgersen | Adelie    |        0.98 | 0.05 | [ 0.89, 1.07] | 21.13
      Torgersen | Chinstrap |        0.01 | 0.03 | [-0.05, 0.08] |  0.32
      Torgersen | Gentoo    |        0.01 | 0.01 | [-0.02, 0.04] |  0.72
      
      Variable predicted: species
      Predictors modulated: island
      Predictors averaged: sex
      Predictions are on the probs-scale.

---

    Code
      print(out, zap_small = TRUE)
    Output
      Estimated Marginal Means
      
      island    | Response  | Probability |   SE |        95% CI |  t(325)
      --------------------------------------------------------------------
      Biscoe    | Adelie    |        0.27 | 0.03 | [ 0.20, 0.34] |    7.76
      Biscoe    | Chinstrap |        0.00 | 0.00 | [ 0.00, 0.00] |    0.00
      Biscoe    | Gentoo    |        0.73 | 0.03 | [ 0.66, 0.80] |   20.99
      Dream     | Adelie    |        0.45 | 0.04 | [ 0.36, 0.54] |    9.97
      Dream     | Chinstrap |        0.55 | 0.04 | [ 0.46, 0.64] |   12.33
      Dream     | Gentoo    |        0.00 | 0.00 | [ 0.00, 0.00] |    0.00
      Torgersen | Adelie    |        1.00 | 0.00 | [ 1.00, 1.00] | 4448.49
      Torgersen | Chinstrap |        0.00 | 0.00 | [ 0.00, 0.00] |    0.00
      Torgersen | Gentoo    |        0.00 | 0.00 | [ 0.00, 0.00] |    0.01
      
      Variable predicted: species
      Predictors modulated: island
      Predictors averaged: sex
      Predictions are on the probs-scale.

