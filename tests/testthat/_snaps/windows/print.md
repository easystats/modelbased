# estimate_slopes - print summary

    Code
      estimate_slopes(model_lm, trend = "x", by = "x", backend = "emmeans")
    Output
      Estimated Marginal Effects
      
      x     | Slope |   SE |         95% CI | t(998) |      p
      -------------------------------------------------------
      -3.38 | 12.75 | 0.25 | [12.26, 13.24] |  50.97 | < .001
      -2.64 | 12.75 | 0.25 | [12.26, 13.24] |  50.97 | < .001
      -1.90 | 12.75 | 0.25 | [12.26, 13.24] |  50.97 | < .001
      -1.16 | 12.75 | 0.25 | [12.26, 13.24] |  50.97 | < .001
      -0.42 | 12.75 | 0.25 | [12.26, 13.24] |  50.97 | < .001
      0.32  | 12.75 | 0.25 | [12.26, 13.24] |  50.97 | < .001
      1.06  | 12.75 | 0.25 | [12.26, 13.24] |  50.97 | < .001
      1.80  | 12.75 | 0.25 | [12.26, 13.24] |  50.97 | < .001
      2.54  | 12.75 | 0.25 | [12.26, 13.24] |  50.97 | < .001
      3.28  | 12.75 | 0.25 | [12.26, 13.24] |  50.97 | < .001
      
      Marginal effects estimated for x

---

    Code
      summary(deriv)
    Message
      There might be too few data to accurately determine intervals. Consider
        setting `length = 100` (or larger) in your call to `estimate_slopes()`.
    Output
      Johnson-Neymann Intervals
      
      Start |  End | Direction | Confidence 
      --------------------------------------
      -3.38 | 3.28 | positive  | Significant
      
      Marginal effects estimated for x

---

    Code
      estimate_slopes(model_lm, trend = "x", by = "x", backend = "marginaleffects")
    Output
      Estimated Marginal Effects
      
      x     | Slope |   SE |         95% CI |     t |      p
      ------------------------------------------------------
      -3.38 | 12.75 | 0.25 | [12.26, 13.24] | 50.97 | < .001
      -2.64 | 12.75 | 0.25 | [12.26, 13.24] | 50.98 | < .001
      -1.90 | 12.75 | 0.25 | [12.26, 13.24] | 50.98 | < .001
      -1.16 | 12.75 | 0.25 | [12.26, 13.24] | 50.97 | < .001
      -0.42 | 12.75 | 0.25 | [12.26, 13.24] | 50.97 | < .001
      0.32  | 12.75 | 0.25 | [12.26, 13.24] | 50.97 | < .001
      1.06  | 12.75 | 0.25 | [12.26, 13.24] | 50.97 | < .001
      1.80  | 12.75 | 0.25 | [12.26, 13.24] | 50.98 | < .001
      2.54  | 12.75 | 0.25 | [12.26, 13.24] | 50.97 | < .001
      3.28  | 12.75 | 0.25 | [12.26, 13.24] | 50.98 | < .001
      
      Marginal effects estimated for x
      Type of slope was dY/dX

---

    Code
      summary(deriv2)
    Message
      There might be too few data to accurately determine intervals. Consider
        setting `length = 100` (or larger) in your call to `estimate_slopes()`.
    Output
      Johnson-Neymann Intervals
      
      Start |  End | Direction | Confidence 
      --------------------------------------
      -3.38 | 3.28 | positive  | Significant
      
      Marginal effects estimated for x
      Type of slope was dY/dX

# estimate_slopes - print regular

    Code
      print(estimate_slopes(model, trend = "Petal.Length", backend = "emmeans"))
    Output
      Estimated Marginal Effects
      
      Slope |   SE |       95% CI | t(144) |      p
      ---------------------------------------------
      0.79  | 0.10 | [0.59, 0.99] |   7.69 | < .001
      
      Marginal effects estimated for Petal.Length

---

    Code
      print(estimate_slopes(model, trend = "Petal.Length", backend = "marginaleffects"))
    Output
      Estimated Marginal Effects
      
      Slope |   SE |       95% CI |    t |      p
      -------------------------------------------
      0.79  | 0.10 | [0.59, 0.99] | 7.69 | < .001
      
      Marginal effects estimated for Petal.Length
      Type of slope was dY/dX

# estimate_means - print multiple by's

    Code
      print(estimate_means(fit, c("c12hour", "c172code", "c161sex"), length = 4,
      backend = "marginaleffects"), table_width = Inf)
    Output
      Estimated Marginal Means
      
      c12hour | c172code                        | c161sex |  Mean |   SE |         95% CI | t(791)
      --------------------------------------------------------------------------------------------
      4.00    | low level of education          | Male    | 11.66 | 0.90 | [ 9.90, 13.42] |  12.99
      58.67   | low level of education          | Male    | 10.93 | 1.01 | [ 8.95, 12.91] |  10.86
      113.33  | low level of education          | Male    | 10.20 | 2.31 | [ 5.66, 14.74] |   4.41
      168.00  | low level of education          | Male    |  9.47 | 3.72 | [ 2.17, 16.77] |   2.55
      4.00    | intermediate level of education | Male    | 11.47 | 0.48 | [10.54, 12.41] |  24.12
      58.67   | intermediate level of education | Male    | 12.52 | 0.69 | [11.16, 13.87] |  18.12
      113.33  | intermediate level of education | Male    | 13.56 | 1.46 | [10.70, 16.42] |   9.31
      168.00  | intermediate level of education | Male    | 14.61 | 2.27 | [10.15, 19.07] |   6.43
      4.00    | high level of education         | Male    | 11.03 | 0.63 | [ 9.79, 12.27] |  17.41
      58.67   | high level of education         | Male    | 11.83 | 0.75 | [10.36, 13.30] |  15.78
      113.33  | high level of education         | Male    | 12.63 | 1.42 | [ 9.84, 15.42] |   8.90
      168.00  | high level of education         | Male    | 13.43 | 2.18 | [ 9.14, 17.71] |   6.15
      4.00    | low level of education          | Female  | 12.24 | 0.41 | [11.43, 13.05] |  29.60
      58.67   | low level of education          | Female  | 11.94 | 0.34 | [11.28, 12.60] |  35.50
      113.33  | low level of education          | Female  | 11.65 | 0.54 | [10.59, 12.71] |  21.54
      168.00  | low level of education          | Female  | 11.36 | 0.84 | [ 9.70, 13.01] |  13.49
      4.00    | intermediate level of education | Female  | 11.34 | 0.24 | [10.87, 11.81] |  47.06
      58.67   | intermediate level of education | Female  | 12.24 | 0.22 | [11.80, 12.67] |  54.82
      113.33  | intermediate level of education | Female  | 13.13 | 0.40 | [12.35, 13.91] |  33.09
      168.00  | intermediate level of education | Female  | 14.03 | 0.62 | [12.82, 15.24] |  22.72
      4.00    | high level of education         | Female  | 12.49 | 0.45 | [11.61, 13.38] |  27.68
      58.67   | high level of education         | Female  | 12.90 | 0.48 | [11.96, 13.84] |  26.99
      113.33  | high level of education         | Female  | 13.31 | 0.97 | [11.40, 15.21] |  13.72
      168.00  | high level of education         | Female  | 13.71 | 1.53 | [10.71, 16.71] |   8.97
      
      Variable predicted: neg_c_7
      Predictors modulated: c12hour, c172code, c161sex
      Predictors averaged: barthtot (65)

---

    Code
      print(estimate_means(fit, c("c12hour", "barthtot = [sd]", "c161sex", "c172code",
        "e16sex"), backend = "marginaleffects", length = 3), table_width = Inf)
    Output
      Estimated Marginal Means
      
      c12hour | barthtot | c161sex | c172code                        | e16sex |  Mean |    SE |           95% CI | t(766)
      -------------------------------------------------------------------------------------------------------------------
      4       |    35.12 | Male    | low level of education          | male   |  9.74 |  4.63 | [  0.66,  18.83] |   2.10
      86      |    35.12 | Male    | low level of education          | male   | 21.80 | 21.63 | [-20.65,  64.25] |   1.01
      168     |    35.12 | Male    | low level of education          | male   | 33.86 | 47.25 | [-58.90, 126.61] |   0.72
      4       |    64.73 | Male    | low level of education          | male   |  9.39 |  1.90 | [  5.67,  13.11] |   4.95
      86      |    64.73 | Male    | low level of education          | male   | 11.99 |  5.40 | [  1.39,  22.59] |   2.22
      168     |    64.73 | Male    | low level of education          | male   | 14.59 | 12.26 | [ -9.47,  38.66] |   1.19
      4       |    94.34 | Male    | low level of education          | male   |  9.03 |  2.38 | [  4.36,  13.71] |   3.79
      86      |    94.34 | Male    | low level of education          | male   |  2.18 | 16.20 | [-29.62,  33.98] |   0.13
      168     |    94.34 | Male    | low level of education          | male   | -4.67 | 34.34 | [-72.09,  62.75] |  -0.14
      4       |    35.12 | Female  | low level of education          | male   | 14.36 |  0.86 | [ 12.67,  16.05] |  16.71
      86      |    35.12 | Female  | low level of education          | male   | 13.13 |  0.54 | [ 12.07,  14.18] |  24.50
      168     |    35.12 | Female  | low level of education          | male   | 11.89 |  1.03 | [  9.87,  13.91] |  11.58
      4       |    64.73 | Female  | low level of education          | male   | 11.84 |  0.62 | [ 10.62,  13.07] |  18.99
      86      |    64.73 | Female  | low level of education          | male   | 11.56 |  0.55 | [ 10.47,  12.64] |  20.92
      168     |    64.73 | Female  | low level of education          | male   | 11.27 |  1.06 | [  9.19,  13.35] |  10.62
      4       |    94.34 | Female  | low level of education          | male   |  9.32 |  0.93 | [  7.50,  11.15] |  10.05
      86      |    94.34 | Female  | low level of education          | male   |  9.99 |  0.87 | [  8.29,  11.69] |  11.52
      168     |    94.34 | Female  | low level of education          | male   | 10.65 |  1.62 | [  7.47,  13.82] |   6.58
      4       |    35.12 | Male    | intermediate level of education | male   | 22.45 |  3.74 | [ 15.10,  29.80] |   6.00
      86      |    35.12 | Male    | intermediate level of education | male   |  7.37 |  6.96 | [ -6.30,  21.03] |   1.06
      168     |    35.12 | Male    | intermediate level of education | male   | -7.72 | 17.20 | [-41.47,  26.04] |  -0.45
      4       |    64.73 | Male    | intermediate level of education | male   | 15.45 |  1.82 | [ 11.87,  19.03] |   8.47
      86      |    64.73 | Male    | intermediate level of education | male   | 14.82 |  5.47 | [  4.08,  25.56] |   2.71
      168     |    64.73 | Male    | intermediate level of education | male   | 14.19 | 12.22 | [ -9.81,  38.18] |   1.16
      4       |    94.34 | Male    | intermediate level of education | male   |  8.45 |  1.17 | [  6.16,  10.74] |   7.23
      86      |    94.34 | Male    | intermediate level of education | male   | 22.27 |  7.76 | [  7.05,  37.49] |   2.87
      168     |    94.34 | Male    | intermediate level of education | male   | 36.09 | 16.29 | [  4.11,  68.07] |   2.22
      4       |    35.12 | Female  | intermediate level of education | male   | 12.96 |  0.79 | [ 11.40,  14.52] |  16.34
      86      |    35.12 | Female  | intermediate level of education | male   | 14.46 |  0.47 | [ 13.55,  15.38] |  31.06
      168     |    35.12 | Female  | intermediate level of education | male   | 15.97 |  0.67 | [ 14.66,  17.27] |  24.00
      4       |    64.73 | Female  | intermediate level of education | male   | 11.48 |  0.45 | [ 10.59,  12.38] |  25.25
      86      |    64.73 | Female  | intermediate level of education | male   | 13.22 |  0.44 | [ 12.37,  14.08] |  30.36
      168     |    64.73 | Female  | intermediate level of education | male   | 14.96 |  0.85 | [ 13.28,  16.64] |  17.53
      4       |    94.34 | Female  | intermediate level of education | male   | 10.01 |  0.54 | [  8.95,  11.07] |  18.59
      86      |    94.34 | Female  | intermediate level of education | male   | 11.98 |  0.66 | [ 10.69,  13.27] |  18.22
      168     |    94.34 | Female  | intermediate level of education | male   | 13.95 |  1.32 | [ 11.37,  16.54] |  10.60
      4       |    35.12 | Male    | high level of education         | male   | 11.88 |  2.02 | [  7.91,  15.85] |   5.88
      86      |    35.12 | Male    | high level of education         | male   | 13.93 |  1.48 | [ 11.03,  16.83] |   9.44
      168     |    35.12 | Male    | high level of education         | male   | 15.97 |  2.48 | [ 11.10,  20.84] |   6.44
      4       |    64.73 | Male    | high level of education         | male   | 11.45 |  1.19 | [  9.11,  13.79] |   9.62
      86      |    64.73 | Male    | high level of education         | male   | 12.48 |  2.02 | [  8.53,  16.44] |   6.19
      168     |    64.73 | Male    | high level of education         | male   | 13.52 |  4.09 | [  5.49,  21.55] |   3.30
      4       |    94.34 | Male    | high level of education         | male   | 11.02 |  1.56 | [  7.95,  14.09] |   7.05
      86      |    94.34 | Male    | high level of education         | male   | 11.04 |  3.23 | [  4.70,  17.38] |   3.42
      168     |    94.34 | Male    | high level of education         | male   | 11.06 |  6.59 | [ -1.88,  24.00] |   1.68
      4       |    35.12 | Female  | high level of education         | male   | 15.92 |  1.51 | [ 12.96,  18.88] |  10.55
      86      |    35.12 | Female  | high level of education         | male   | 14.36 |  1.08 | [ 12.23,  16.49] |  13.26
      168     |    35.12 | Female  | high level of education         | male   | 12.80 |  2.20 | [  8.48,  17.12] |   5.82
      4       |    64.73 | Female  | high level of education         | male   | 13.02 |  0.88 | [ 11.29,  14.75] |  14.78
      86      |    64.73 | Female  | high level of education         | male   | 12.35 |  1.20 | [  9.98,  14.71] |  10.25
      168     |    64.73 | Female  | high level of education         | male   | 11.67 |  2.63 | [  6.50,  16.84] |   4.43
      4       |    94.34 | Female  | high level of education         | male   | 10.13 |  1.09 | [  7.99,  12.27] |   9.29
      86      |    94.34 | Female  | high level of education         | male   | 10.33 |  1.88 | [  6.65,  14.01] |   5.51
      168     |    94.34 | Female  | high level of education         | male   | 10.53 |  3.97 | [  2.73,  18.33] |   2.65
      4       |    35.12 | Male    | low level of education          | female | 16.94 |  2.25 | [ 12.52,  21.35] |   7.53
      86      |    35.12 | Male    | low level of education          | female | 13.60 |  1.19 | [ 11.26,  15.95] |  11.39
      168     |    35.12 | Male    | low level of education          | female | 10.27 |  2.16 | [  6.03,  14.50] |   4.76
      4       |    64.73 | Male    | low level of education          | female | 13.01 |  1.16 | [ 10.73,  15.29] |  11.20
      86      |    64.73 | Male    | low level of education          | female | 10.57 |  1.77 | [  7.10,  14.04] |   5.98
      168     |    64.73 | Male    | low level of education          | female |  8.13 |  4.09 | [  0.10,  16.17] |   1.99
      4       |    94.34 | Male    | low level of education          | female |  9.08 |  1.81 | [  5.52,  12.65] |   5.01
      86      |    94.34 | Male    | low level of education          | female |  7.54 |  2.93 | [  1.80,  13.29] |   2.58
      168     |    94.34 | Male    | low level of education          | female |  6.00 |  6.44 | [ -6.64,  18.64] |   0.93
      4       |    35.12 | Female  | low level of education          | female | 15.68 |  0.97 | [ 13.78,  17.58] |  16.19
      86      |    35.12 | Female  | low level of education          | female | 14.22 |  0.59 | [ 13.06,  15.39] |  23.91
      168     |    35.12 | Female  | low level of education          | female | 12.77 |  1.09 | [ 10.63,  14.90] |  11.74
      4       |    64.73 | Female  | low level of education          | female | 12.73 |  0.58 | [ 11.60,  13.87] |  21.99
      86      |    64.73 | Female  | low level of education          | female | 11.97 |  0.66 | [ 10.67,  13.26] |  18.09
      168     |    64.73 | Female  | low level of education          | female | 11.20 |  1.39 | [  8.48,  13.92] |   8.07
      4       |    94.34 | Female  | low level of education          | female |  9.79 |  0.70 | [  8.40,  11.17] |  13.90
      86      |    94.34 | Female  | low level of education          | female |  9.71 |  1.07 | [  7.60,  11.81] |   9.05
      168     |    94.34 | Female  | low level of education          | female |  9.63 |  2.25 | [  5.21,  14.05] |   4.28
      4       |    35.12 | Male    | intermediate level of education | female | 12.23 |  0.91 | [ 10.45,  14.01] |  13.48
      86      |    35.12 | Male    | intermediate level of education | female | 12.65 |  0.66 | [ 11.35,  13.95] |  19.13
      168     |    35.12 | Male    | intermediate level of education | female | 13.08 |  1.23 | [ 10.67,  15.49] |  10.66
      4       |    64.73 | Male    | intermediate level of education | female | 10.72 |  0.55 | [  9.64,  11.79] |  19.58
      86      |    64.73 | Male    | intermediate level of education | female | 12.65 |  1.08 | [ 10.53,  14.77] |  11.70
      168     |    64.73 | Male    | intermediate level of education | female | 14.59 |  2.32 | [ 10.03,  19.15] |   6.28
      4       |    94.34 | Male    | intermediate level of education | female |  9.21 |  0.62 | [  7.99,  10.42] |  14.89
      86      |    94.34 | Male    | intermediate level of education | female | 12.65 |  1.77 | [  9.18,  16.12] |   7.16
      168     |    94.34 | Male    | intermediate level of education | female | 16.10 |  3.71 | [  8.81,  23.39] |   4.34
      4       |    35.12 | Female  | intermediate level of education | female | 12.91 |  0.47 | [ 11.99,  13.84] |  27.33
      86      |    35.12 | Female  | intermediate level of education | female | 13.14 |  0.33 | [ 12.49,  13.79] |  39.53
      168     |    35.12 | Female  | intermediate level of education | female | 13.37 |  0.64 | [ 12.12,  14.62] |  20.93
      4       |    64.73 | Female  | intermediate level of education | female | 11.38 |  0.29 | [ 10.83,  11.94] |  39.94
      86      |    64.73 | Female  | intermediate level of education | female | 12.28 |  0.42 | [ 11.45,  13.10] |  29.25
      168     |    64.73 | Female  | intermediate level of education | female | 13.17 |  0.88 | [ 11.43,  14.91] |  14.88
      4       |    94.34 | Female  | intermediate level of education | female |  9.86 |  0.34 | [  9.18,  10.53] |  28.57
      86      |    94.34 | Female  | intermediate level of education | female | 11.41 |  0.67 | [ 10.10,  12.72] |  17.11
      168     |    94.34 | Female  | intermediate level of education | female | 12.97 |  1.40 | [ 10.22,  15.72] |   9.26
      4       |    35.12 | Male    | high level of education         | female | 10.93 |  1.13 | [  8.72,  13.14] |   9.71
      86      |    35.12 | Male    | high level of education         | female | 12.15 |  0.88 | [ 10.42,  13.87] |  13.84
      168     |    35.12 | Male    | high level of education         | female | 13.36 |  1.55 | [ 10.32,  16.41] |   8.62
      4       |    64.73 | Male    | high level of education         | female | 10.86 |  0.75 | [  9.40,  12.33] |  14.57
      86      |    64.73 | Male    | high level of education         | female | 12.26 |  1.24 | [  9.82,  14.70] |   9.88
      168     |    64.73 | Male    | high level of education         | female | 13.66 |  2.57 | [  8.62,  18.70] |   5.32
      4       |    94.34 | Male    | high level of education         | female | 10.79 |  1.01 | [  8.80,  12.78] |  10.65
      86      |    94.34 | Male    | high level of education         | female | 12.38 |  2.09 | [  8.28,  16.47] |   5.93
      168     |    94.34 | Male    | high level of education         | female | 13.96 |  4.30 | [  5.51,  22.41] |   3.24
      4       |    35.12 | Female  | high level of education         | female | 13.86 |  0.91 | [ 12.06,  15.65] |  15.18
      86      |    35.12 | Female  | high level of education         | female | 15.00 |  0.75 | [ 13.52,  16.48] |  19.90
      168     |    35.12 | Female  | high level of education         | female | 16.15 |  1.55 | [ 13.11,  19.19] |  10.43
      4       |    64.73 | Female  | high level of education         | female | 12.31 |  0.52 | [ 11.29,  13.34] |  23.59
      86      |    64.73 | Female  | high level of education         | female | 13.54 |  0.86 | [ 11.84,  15.24] |  15.67
      168     |    64.73 | Female  | high level of education         | female | 14.76 |  1.86 | [ 11.10,  18.42] |   7.92
      4       |    94.34 | Female  | high level of education         | female | 10.77 |  0.73 | [  9.34,  12.20] |  14.77
      86      |    94.34 | Female  | high level of education         | female | 12.07 |  1.35 | [  9.42,  14.73] |   8.92
      168     |    94.34 | Female  | high level of education         | female | 13.38 |  2.80 | [  7.87,  18.88] |   4.77
      
      Variable predicted: neg_c_7
      Predictors modulated: c12hour, barthtot = [sd], c161sex, c172code, e16sex

# estimate_means - full labels

    Code
      print(estimate_means(fit, c("c161sex", "c172code", "e16sex", "nur_pst"),
      backend = "marginaleffects"), table_width = Inf)
    Output
      Estimated Marginal Means
      
      c161sex | c172code                        | e16sex | nur_pst         |   Mean |   SE |          95% CI | t(402)
      ---------------------------------------------------------------------------------------------------------------
      Male    | low level of education          | male   | Care Level 1    |  10.00 | 2.59 | [  4.90, 15.10] |   3.85
      Female  | low level of education          | male   | Care Level 1    |  13.75 | 1.06 | [ 11.67, 15.83] |  12.98
      Male    | intermediate level of education | male   | Care Level 1    |  12.00 | 2.12 | [  7.84, 16.16] |   5.66
      Female  | intermediate level of education | male   | Care Level 1    |  10.55 | 0.78 | [  9.01, 12.08] |  13.48
      Male    | high level of education         | male   | Care Level 1    |  14.25 | 1.83 | [ 10.64, 17.86] |   7.77
      Female  | high level of education         | male   | Care Level 1    |  10.00 | 3.67 | [  2.79, 17.21] |   2.73
      Male    | low level of education          | female | Care Level 1    |  15.50 | 1.83 | [ 11.89, 19.11] |   8.45
      Female  | low level of education          | female | Care Level 1    |  11.56 | 0.92 | [  9.76, 13.37] |  12.60
      Male    | intermediate level of education | female | Care Level 1    |  10.16 | 0.84 | [  8.50, 11.81] |  12.07
      Female  | intermediate level of education | female | Care Level 1    |  11.22 | 0.50 | [ 10.24, 12.20] |  22.47
      Male    | high level of education         | female | Care Level 1    |  11.67 | 1.50 | [  8.72, 14.61] |   7.79
      Female  | high level of education         | female | Care Level 1    |  12.44 | 0.86 | [ 10.74, 14.14] |  14.39
      Male    | low level of education          | male   | Care Level 2    |  11.00 | 2.59 | [  5.90, 16.10] |   4.24
      Female  | low level of education          | male   | Care Level 2    |  12.23 | 1.02 | [ 10.23, 14.23] |  12.02
      Male    | intermediate level of education | male   | Care Level 2    |  13.25 | 1.30 | [ 10.70, 15.80] |  10.21
      Female  | intermediate level of education | male   | Care Level 2    |  13.00 | 0.68 | [ 11.66, 14.34] |  19.08
      Male    | high level of education         | male   | Care Level 2    |  10.00 | 3.67 | [  2.79, 17.21] |   2.73
      Female  | high level of education         | male   | Care Level 2    |  12.78 | 1.22 | [ 10.37, 15.18] |  10.45
      Male    | low level of education          | female | Care Level 2    |  13.75 | 1.83 | [ 10.14, 17.36] |   7.49
      Female  | low level of education          | female | Care Level 2    |  11.17 | 1.06 | [  9.08, 13.25] |  10.54
      Male    | intermediate level of education | female | Care Level 2    |  11.17 | 0.86 | [  9.47, 12.87] |  12.91
      Female  | intermediate level of education | female | Care Level 2    |  11.60 | 0.50 | [ 10.61, 12.59] |  23.02
      Male    | high level of education         | female | Care Level 2    |  11.20 | 1.16 | [  8.92, 13.48] |   9.65
      Female  | high level of education         | female | Care Level 2    |  14.56 | 0.92 | [ 12.76, 16.37] |  15.87
      Male    | low level of education          | male   | Care Level 3/3+ | -13.53 | 7.92 | [-29.10,  2.04] |  -1.71
      Female  | low level of education          | male   | Care Level 3/3+ |  12.09 | 1.11 | [  9.92, 14.27] |  10.93
      Male    | intermediate level of education | male   | Care Level 3/3+ |  13.57 | 1.39 | [ 10.85, 16.30] |   9.79
      Female  | intermediate level of education | male   | Care Level 3/3+ |  16.50 | 0.78 | [ 14.96, 18.04] |  21.09
      Male    | high level of education         | male   | Care Level 3/3+ |   8.00 | 3.67 | [  0.79, 15.21] |   2.18
      Female  | high level of education         | male   | Care Level 3/3+ |  16.33 | 2.12 | [ 12.17, 20.50] |   7.71
      Male    | low level of education          | female | Care Level 3/3+ |  12.00 | 2.12 | [  7.84, 16.16] |   5.66
      Female  | low level of education          | female | Care Level 3/3+ |  15.00 | 1.39 | [ 12.27, 17.73] |  10.82
      Male    | intermediate level of education | female | Care Level 3/3+ |  11.54 | 1.02 | [  9.54, 13.54] |  11.34
      Female  | intermediate level of education | female | Care Level 3/3+ |  13.35 | 0.77 | [ 11.84, 14.85] |  17.45
      Male    | high level of education         | female | Care Level 3/3+ |  13.00 | 1.83 | [  9.39, 16.61] |   7.09
      Female  | high level of education         | female | Care Level 3/3+ |  11.43 | 1.39 | [  8.70, 14.15] |   8.24
      
      Variable predicted: neg_c_7
      Predictors modulated: c161sex, c172code, e16sex, nur_pst

---

    Code
      print(estimate_means(fit, c("c161sex", "c172code", "e16sex", "nur_pst"),
      backend = "marginaleffects"), full_labels = FALSE, table_width = Inf)
    Output
      Estimated Marginal Means
      
      c161sex | c172code                        | e16sex | nur_pst         |   Mean |   SE |          95% CI | t(402)
      ---------------------------------------------------------------------------------------------------------------
      Male    | low level of education          | male   | Care Level 1    |  10.00 | 2.59 | [  4.90, 15.10] |   3.85
      Female  |                                 |        |                 |  13.75 | 1.06 | [ 11.67, 15.83] |  12.98
      Male    | intermediate level of education |        |                 |  12.00 | 2.12 | [  7.84, 16.16] |   5.66
      Female  |                                 |        |                 |  10.55 | 0.78 | [  9.01, 12.08] |  13.48
      Male    | high level of education         |        |                 |  14.25 | 1.83 | [ 10.64, 17.86] |   7.77
      Female  |                                 |        |                 |  10.00 | 3.67 | [  2.79, 17.21] |   2.73
      Male    | low level of education          | female |                 |  15.50 | 1.83 | [ 11.89, 19.11] |   8.45
      Female  |                                 |        |                 |  11.56 | 0.92 | [  9.76, 13.37] |  12.60
      Male    | intermediate level of education |        |                 |  10.16 | 0.84 | [  8.50, 11.81] |  12.07
      Female  |                                 |        |                 |  11.22 | 0.50 | [ 10.24, 12.20] |  22.47
      Male    | high level of education         |        |                 |  11.67 | 1.50 | [  8.72, 14.61] |   7.79
      Female  |                                 |        |                 |  12.44 | 0.86 | [ 10.74, 14.14] |  14.39
      Male    | low level of education          | male   | Care Level 2    |  11.00 | 2.59 | [  5.90, 16.10] |   4.24
      Female  |                                 |        |                 |  12.23 | 1.02 | [ 10.23, 14.23] |  12.02
      Male    | intermediate level of education |        |                 |  13.25 | 1.30 | [ 10.70, 15.80] |  10.21
      Female  |                                 |        |                 |  13.00 | 0.68 | [ 11.66, 14.34] |  19.08
      Male    | high level of education         |        |                 |  10.00 | 3.67 | [  2.79, 17.21] |   2.73
      Female  |                                 |        |                 |  12.78 | 1.22 | [ 10.37, 15.18] |  10.45
      Male    | low level of education          | female |                 |  13.75 | 1.83 | [ 10.14, 17.36] |   7.49
      Female  |                                 |        |                 |  11.17 | 1.06 | [  9.08, 13.25] |  10.54
      Male    | intermediate level of education |        |                 |  11.17 | 0.86 | [  9.47, 12.87] |  12.91
      Female  |                                 |        |                 |  11.60 | 0.50 | [ 10.61, 12.59] |  23.02
      Male    | high level of education         |        |                 |  11.20 | 1.16 | [  8.92, 13.48] |   9.65
      Female  |                                 |        |                 |  14.56 | 0.92 | [ 12.76, 16.37] |  15.87
      Male    | low level of education          | male   | Care Level 3/3+ | -13.53 | 7.92 | [-29.10,  2.04] |  -1.71
      Female  |                                 |        |                 |  12.09 | 1.11 | [  9.92, 14.27] |  10.93
      Male    | intermediate level of education |        |                 |  13.57 | 1.39 | [ 10.85, 16.30] |   9.79
      Female  |                                 |        |                 |  16.50 | 0.78 | [ 14.96, 18.04] |  21.09
      Male    | high level of education         |        |                 |   8.00 | 3.67 | [  0.79, 15.21] |   2.18
      Female  |                                 |        |                 |  16.33 | 2.12 | [ 12.17, 20.50] |   7.71
      Male    | low level of education          | female |                 |  12.00 | 2.12 | [  7.84, 16.16] |   5.66
      Female  |                                 |        |                 |  15.00 | 1.39 | [ 12.27, 17.73] |  10.82
      Male    | intermediate level of education |        |                 |  11.54 | 1.02 | [  9.54, 13.54] |  11.34
      Female  |                                 |        |                 |  13.35 | 0.77 | [ 11.84, 14.85] |  17.45
      Male    | high level of education         |        |                 |  13.00 | 1.83 | [  9.39, 16.61] |   7.09
      Female  |                                 |        |                 |  11.43 | 1.39 | [  8.70, 14.15] |   8.24
      
      Variable predicted: neg_c_7
      Predictors modulated: c161sex, c172code, e16sex, nur_pst

---

    Code
      print(estimate_means(fit, c("c161sex", "c172code", "e16sex", "nur_pst",
        "negc7d"), backend = "marginaleffects"), table_width = Inf)
    Output
      Estimated Marginal Means
      
      c161sex | c172code                        | e16sex | nur_pst         | negc7d |  Mean |   SE |          95% CI | t(370)
      -----------------------------------------------------------------------------------------------------------------------
      Male    | low level of education          | male   | Care Level 1    | 0      |  8.00 | 2.28 | [  3.51, 12.49] |   3.51
      Female  | low level of education          | male   | Care Level 1    | 0      | 10.00 | 1.02 | [  7.99, 12.01] |   9.80
      Male    | intermediate level of education | male   | Care Level 1    | 0      |  8.00 | 2.28 | [  3.51, 12.49] |   3.51
      Female  | intermediate level of education | male   | Care Level 1    | 0      |  9.13 | 0.59 | [  7.97, 10.29] |  15.50
      Male    | high level of education         | male   | Care Level 1    | 0      |  9.00 | 2.28 | [  4.51, 13.49] |   3.94
      Female  | high level of education         | male   | Care Level 1    | 0      | 10.00 | 2.28 | [  5.51, 14.49] |   4.38
      Male    | low level of education          | female | Care Level 1    | 0      | 11.00 | 2.28 | [  6.51, 15.49] |   4.82
      Female  | low level of education          | female | Care Level 1    | 0      |  8.89 | 0.76 | [  7.39, 10.38] |  11.69
      Male    | intermediate level of education | female | Care Level 1    | 0      |  9.21 | 0.61 | [  8.02, 10.41] |  15.11
      Female  | intermediate level of education | female | Care Level 1    | 0      |  9.47 | 0.40 | [  8.68, 10.26] |  23.48
      Male    | high level of education         | female | Care Level 1    | 0      |  9.00 | 1.32 | [  6.41, 11.59] |   6.83
      Female  | high level of education         | female | Care Level 1    | 0      |  8.71 | 0.86 | [  7.02, 10.41] |  10.11
      Male    | low level of education          | male   | Care Level 2    | 0      |  9.00 | 2.28 | [  4.51, 13.49] |   3.94
      Female  | low level of education          | male   | Care Level 2    | 0      |  9.50 | 0.93 | [  7.67, 11.33] |  10.20
      Male    | intermediate level of education | male   | Care Level 2    | 0      |  8.75 | 1.14 | [  6.51, 10.99] |   7.67
      Female  | intermediate level of education | male   | Care Level 2    | 0      |  9.30 | 0.72 | [  7.88, 10.72] |  12.89
      Male    | high level of education         | male   | Care Level 2    | 0      | 10.00 | 2.28 | [  5.51, 14.49] |   4.38
      Female  | high level of education         | male   | Care Level 2    | 0      |  9.00 | 1.14 | [  6.76, 11.24] |   7.89
      Male    | low level of education          | female | Care Level 2    | 0      | 10.50 | 1.61 | [  7.33, 13.67] |   6.51
      Female  | low level of education          | female | Care Level 2    | 0      |  9.17 | 0.93 | [  7.34, 11.00] |   9.84
      Male    | intermediate level of education | female | Care Level 2    | 0      |  8.50 | 0.66 | [  7.20,  9.80] |  12.91
      Female  | intermediate level of education | female | Care Level 2    | 0      |  9.30 | 0.42 | [  8.48, 10.12] |  22.33
      Male    | high level of education         | female | Care Level 2    | 0      |  9.43 | 0.86 | [  7.73, 11.12] |  10.93
      Female  | high level of education         | female | Care Level 2    | 0      |  8.00 | 1.61 | [  4.83, 11.17] |   4.96
      Male    | low level of education          | male   | Care Level 3/3+ | 0      |  3.67 | 6.95 | [-10.00, 17.35] |   0.53
      Female  | low level of education          | male   | Care Level 3/3+ | 0      |  9.67 | 1.32 | [  7.08, 12.26] |   7.34
      Male    | intermediate level of education | male   | Care Level 3/3+ | 0      |  9.75 | 1.14 | [  7.51, 11.99] |   8.55
      Female  | intermediate level of education | male   | Care Level 3/3+ | 0      |  9.50 | 1.14 | [  7.26, 11.74] |   8.33
      Male    | high level of education         | male   | Care Level 3/3+ | 0      |  8.00 | 2.28 | [  3.51, 12.49] |   3.51
      Female  | high level of education         | male   | Care Level 3/3+ | 0      | 11.00 | 2.28 | [  6.51, 15.49] |   4.82
      Male    | low level of education          | female | Care Level 3/3+ | 0      |  8.50 | 1.61 | [  5.33, 11.67] |   5.27
      Female  | low level of education          | female | Care Level 3/3+ | 0      |  8.67 | 1.32 | [  6.08, 11.26] |   6.58
      Male    | intermediate level of education | female | Care Level 3/3+ | 0      |  9.13 | 0.81 | [  7.54, 10.71] |  11.31
      Female  | intermediate level of education | female | Care Level 3/3+ | 0      |  9.78 | 0.76 | [  8.28, 11.27] |  12.86
      Male    | high level of education         | female | Care Level 3/3+ | 0      |  9.00 | 2.28 | [  4.51, 13.49] |   3.94
      Female  | high level of education         | female | Care Level 3/3+ | 0      |  9.00 | 1.32 | [  6.41, 11.59] |   6.83
      Male    | low level of education          | male   | Care Level 1    | 1      | 12.00 | 2.28 | [  7.51, 16.49] |   5.26
      Female  | low level of education          | male   | Care Level 1    | 1      | 16.43 | 0.86 | [ 14.73, 18.12] |  19.05
      Male    | intermediate level of education | male   | Care Level 1    | 1      | 14.00 | 1.61 | [ 10.83, 17.17] |   8.68
      Female  | intermediate level of education | male   | Care Level 1    | 1      | 13.57 | 0.86 | [ 11.88, 15.27] |  15.74
      Male    | high level of education         | male   | Care Level 1    | 1      | 16.00 | 1.32 | [ 13.41, 18.59] |  12.15
      Female  | high level of education         | male   | Care Level 1    | 1      | 27.92 | 5.08 | [ 17.93, 37.91] |   5.50
      Male    | low level of education          | female | Care Level 1    | 1      | 17.00 | 1.32 | [ 14.41, 19.59] |  12.91
      Female  | low level of education          | female | Care Level 1    | 1      | 15.00 | 0.86 | [ 13.30, 16.70] |  17.40
      Male    | intermediate level of education | female | Care Level 1    | 1      | 12.80 | 1.02 | [ 10.79, 14.81] |  12.55
      Female  | intermediate level of education | female | Care Level 1    | 1      | 13.77 | 0.49 | [ 12.82, 14.73] |  28.32
      Male    | high level of education         | female | Care Level 1    | 1      | 14.33 | 1.32 | [ 11.74, 16.92] |  10.88
      Female  | high level of education         | female | Care Level 1    | 1      | 14.82 | 0.69 | [ 13.47, 16.17] |  21.54
      Male    | low level of education          | male   | Care Level 2    | 1      | 13.00 | 2.28 | [  8.51, 17.49] |   5.70
      Female  | low level of education          | male   | Care Level 2    | 1      | 14.57 | 0.86 | [ 12.88, 16.27] |  16.90
      Male    | intermediate level of education | male   | Care Level 2    | 1      | 17.75 | 1.14 | [ 15.51, 19.99] |  15.56
      Female  | intermediate level of education | male   | Care Level 2    | 1      | 14.95 | 0.52 | [ 13.92, 15.98] |  28.56
      Male    | high level of education         | male   | Care Level 2    | 1      |  3.80 | 8.72 | [-13.34, 20.94] |   0.44
      Female  | high level of education         | male   | Care Level 2    | 1      | 15.80 | 1.02 | [ 13.79, 17.81] |  15.49
      Male    | low level of education          | female | Care Level 2    | 1      | 17.00 | 1.61 | [ 13.83, 20.17] |  10.54
      Female  | low level of education          | female | Care Level 2    | 1      | 13.17 | 0.93 | [ 11.34, 15.00] |  14.14
      Male    | intermediate level of education | female | Care Level 2    | 1      | 16.50 | 0.93 | [ 14.67, 18.33] |  17.72
      Female  | intermediate level of education | female | Care Level 2    | 1      | 14.61 | 0.48 | [ 13.67, 15.54] |  30.71
      Male    | high level of education         | female | Care Level 2    | 1      | 15.33 | 1.32 | [ 12.74, 17.92] |  11.64
      Female  | high level of education         | female | Care Level 2    | 1      | 15.50 | 0.61 | [ 14.30, 16.70] |  25.42
      Male    | low level of education          | male   | Care Level 3/3+ | 1      |  1.78 | 7.99 | [-13.93, 17.48] |   0.22
      Female  | low level of education          | male   | Care Level 3/3+ | 1      | 13.00 | 0.81 | [ 11.41, 14.59] |  16.12
      Male    | intermediate level of education | male   | Care Level 3/3+ | 1      | 18.67 | 1.32 | [ 16.08, 21.26] |  14.17
      Female  | intermediate level of education | male   | Care Level 3/3+ | 1      | 18.06 | 0.54 | [ 17.00, 19.11] |  33.58
      Male    | high level of education         | male   | Care Level 3/3+ | 1      |  4.60 | 7.00 | [ -9.17, 18.38] |   0.66
      Female  | high level of education         | male   | Care Level 3/3+ | 1      | 19.00 | 1.61 | [ 15.83, 22.17] |  11.78
      Male    | low level of education          | female | Care Level 3/3+ | 1      | 19.00 | 2.28 | [ 14.51, 23.49] |   8.33
      Female  | low level of education          | female | Care Level 3/3+ | 1      | 19.75 | 1.14 | [ 17.51, 21.99] |  17.31
      Male    | intermediate level of education | female | Care Level 3/3+ | 1      | 15.40 | 1.02 | [ 13.39, 17.41] |  15.09
      Female  | intermediate level of education | female | Care Level 3/3+ | 1      | 15.64 | 0.61 | [ 14.44, 16.84] |  25.65
      Male    | high level of education         | female | Care Level 3/3+ | 1      | 14.33 | 1.32 | [ 11.74, 16.92] |  10.88
      Female  | high level of education         | female | Care Level 3/3+ | 1      | 13.25 | 1.14 | [ 11.01, 15.49] |  11.62
      
      Variable predicted: neg_c_7
      Predictors modulated: c161sex, c172code, e16sex, nur_pst, negc7d

---

    Code
      print(estimate_means(fit, c("c161sex", "c172code", "e16sex", "nur_pst",
        "negc7d"), backend = "marginaleffects"), full_labels = FALSE, table_width = Inf)
    Output
      Estimated Marginal Means
      
      c161sex | c172code                        | e16sex | nur_pst         | negc7d |  Mean |   SE |          95% CI | t(370)
      -----------------------------------------------------------------------------------------------------------------------
      Male    | low level of education          | male   | Care Level 1    | 0      |  8.00 | 2.28 | [  3.51, 12.49] |   3.51
      Female  |                                 |        |                 |        | 10.00 | 1.02 | [  7.99, 12.01] |   9.80
      Male    | intermediate level of education |        |                 |        |  8.00 | 2.28 | [  3.51, 12.49] |   3.51
      Female  |                                 |        |                 |        |  9.13 | 0.59 | [  7.97, 10.29] |  15.50
      Male    | high level of education         |        |                 |        |  9.00 | 2.28 | [  4.51, 13.49] |   3.94
      Female  |                                 |        |                 |        | 10.00 | 2.28 | [  5.51, 14.49] |   4.38
      Male    | low level of education          | female |                 |        | 11.00 | 2.28 | [  6.51, 15.49] |   4.82
      Female  |                                 |        |                 |        |  8.89 | 0.76 | [  7.39, 10.38] |  11.69
      Male    | intermediate level of education |        |                 |        |  9.21 | 0.61 | [  8.02, 10.41] |  15.11
      Female  |                                 |        |                 |        |  9.47 | 0.40 | [  8.68, 10.26] |  23.48
      Male    | high level of education         |        |                 |        |  9.00 | 1.32 | [  6.41, 11.59] |   6.83
      Female  |                                 |        |                 |        |  8.71 | 0.86 | [  7.02, 10.41] |  10.11
      Male    | low level of education          | male   | Care Level 2    |        |  9.00 | 2.28 | [  4.51, 13.49] |   3.94
      Female  |                                 |        |                 |        |  9.50 | 0.93 | [  7.67, 11.33] |  10.20
      Male    | intermediate level of education |        |                 |        |  8.75 | 1.14 | [  6.51, 10.99] |   7.67
      Female  |                                 |        |                 |        |  9.30 | 0.72 | [  7.88, 10.72] |  12.89
      Male    | high level of education         |        |                 |        | 10.00 | 2.28 | [  5.51, 14.49] |   4.38
      Female  |                                 |        |                 |        |  9.00 | 1.14 | [  6.76, 11.24] |   7.89
      Male    | low level of education          | female |                 |        | 10.50 | 1.61 | [  7.33, 13.67] |   6.51
      Female  |                                 |        |                 |        |  9.17 | 0.93 | [  7.34, 11.00] |   9.84
      Male    | intermediate level of education |        |                 |        |  8.50 | 0.66 | [  7.20,  9.80] |  12.91
      Female  |                                 |        |                 |        |  9.30 | 0.42 | [  8.48, 10.12] |  22.33
      Male    | high level of education         |        |                 |        |  9.43 | 0.86 | [  7.73, 11.12] |  10.93
      Female  |                                 |        |                 |        |  8.00 | 1.61 | [  4.83, 11.17] |   4.96
      Male    | low level of education          | male   | Care Level 3/3+ |        |  3.67 | 6.95 | [-10.00, 17.35] |   0.53
      Female  |                                 |        |                 |        |  9.67 | 1.32 | [  7.08, 12.26] |   7.34
      Male    | intermediate level of education |        |                 |        |  9.75 | 1.14 | [  7.51, 11.99] |   8.55
      Female  |                                 |        |                 |        |  9.50 | 1.14 | [  7.26, 11.74] |   8.33
      Male    | high level of education         |        |                 |        |  8.00 | 2.28 | [  3.51, 12.49] |   3.51
      Female  |                                 |        |                 |        | 11.00 | 2.28 | [  6.51, 15.49] |   4.82
      Male    | low level of education          | female |                 |        |  8.50 | 1.61 | [  5.33, 11.67] |   5.27
      Female  |                                 |        |                 |        |  8.67 | 1.32 | [  6.08, 11.26] |   6.58
      Male    | intermediate level of education |        |                 |        |  9.13 | 0.81 | [  7.54, 10.71] |  11.31
      Female  |                                 |        |                 |        |  9.78 | 0.76 | [  8.28, 11.27] |  12.86
      Male    | high level of education         |        |                 |        |  9.00 | 2.28 | [  4.51, 13.49] |   3.94
      Female  |                                 |        |                 |        |  9.00 | 1.32 | [  6.41, 11.59] |   6.83
      Male    | low level of education          | male   | Care Level 1    | 1      | 12.00 | 2.28 | [  7.51, 16.49] |   5.26
      Female  |                                 |        |                 |        | 16.43 | 0.86 | [ 14.73, 18.12] |  19.05
      Male    | intermediate level of education |        |                 |        | 14.00 | 1.61 | [ 10.83, 17.17] |   8.68
      Female  |                                 |        |                 |        | 13.57 | 0.86 | [ 11.88, 15.27] |  15.74
      Male    | high level of education         |        |                 |        | 16.00 | 1.32 | [ 13.41, 18.59] |  12.15
      Female  |                                 |        |                 |        | 27.92 | 5.08 | [ 17.93, 37.91] |   5.50
      Male    | low level of education          | female |                 |        | 17.00 | 1.32 | [ 14.41, 19.59] |  12.91
      Female  |                                 |        |                 |        | 15.00 | 0.86 | [ 13.30, 16.70] |  17.40
      Male    | intermediate level of education |        |                 |        | 12.80 | 1.02 | [ 10.79, 14.81] |  12.55
      Female  |                                 |        |                 |        | 13.77 | 0.49 | [ 12.82, 14.73] |  28.32
      Male    | high level of education         |        |                 |        | 14.33 | 1.32 | [ 11.74, 16.92] |  10.88
      Female  |                                 |        |                 |        | 14.82 | 0.69 | [ 13.47, 16.17] |  21.54
      Male    | low level of education          | male   | Care Level 2    |        | 13.00 | 2.28 | [  8.51, 17.49] |   5.70
      Female  |                                 |        |                 |        | 14.57 | 0.86 | [ 12.88, 16.27] |  16.90
      Male    | intermediate level of education |        |                 |        | 17.75 | 1.14 | [ 15.51, 19.99] |  15.56
      Female  |                                 |        |                 |        | 14.95 | 0.52 | [ 13.92, 15.98] |  28.56
      Male    | high level of education         |        |                 |        |  3.80 | 8.72 | [-13.34, 20.94] |   0.44
      Female  |                                 |        |                 |        | 15.80 | 1.02 | [ 13.79, 17.81] |  15.49
      Male    | low level of education          | female |                 |        | 17.00 | 1.61 | [ 13.83, 20.17] |  10.54
      Female  |                                 |        |                 |        | 13.17 | 0.93 | [ 11.34, 15.00] |  14.14
      Male    | intermediate level of education |        |                 |        | 16.50 | 0.93 | [ 14.67, 18.33] |  17.72
      Female  |                                 |        |                 |        | 14.61 | 0.48 | [ 13.67, 15.54] |  30.71
      Male    | high level of education         |        |                 |        | 15.33 | 1.32 | [ 12.74, 17.92] |  11.64
      Female  |                                 |        |                 |        | 15.50 | 0.61 | [ 14.30, 16.70] |  25.42
      Male    | low level of education          | male   | Care Level 3/3+ |        |  1.78 | 7.99 | [-13.93, 17.48] |   0.22
      Female  |                                 |        |                 |        | 13.00 | 0.81 | [ 11.41, 14.59] |  16.12
      Male    | intermediate level of education |        |                 |        | 18.67 | 1.32 | [ 16.08, 21.26] |  14.17
      Female  |                                 |        |                 |        | 18.06 | 0.54 | [ 17.00, 19.11] |  33.58
      Male    | high level of education         |        |                 |        |  4.60 | 7.00 | [ -9.17, 18.38] |   0.66
      Female  |                                 |        |                 |        | 19.00 | 1.61 | [ 15.83, 22.17] |  11.78
      Male    | low level of education          | female |                 |        | 19.00 | 2.28 | [ 14.51, 23.49] |   8.33
      Female  |                                 |        |                 |        | 19.75 | 1.14 | [ 17.51, 21.99] |  17.31
      Male    | intermediate level of education |        |                 |        | 15.40 | 1.02 | [ 13.39, 17.41] |  15.09
      Female  |                                 |        |                 |        | 15.64 | 0.61 | [ 14.44, 16.84] |  25.65
      Male    | high level of education         |        |                 |        | 14.33 | 1.32 | [ 11.74, 16.92] |  10.88
      Female  |                                 |        |                 |        | 13.25 | 1.14 | [ 11.01, 15.49] |  11.62
      
      Variable predicted: neg_c_7
      Predictors modulated: c161sex, c172code, e16sex, nur_pst, negc7d

# estimate_means - protect integers

    Code
      estimate_expectation(model, by = c("c160age=[fivenum]", "c161sex"))
    Output
      Model-based Predictions
      
      c160age | c161sex | Predicted |   SE |         95% CI
      -----------------------------------------------------
      44      | Male    |     11.80 | 0.33 | [11.15, 12.45]
      54      | Male    |     11.65 | 0.31 | [11.04, 12.26]
      63      | Male    |     11.51 | 0.36 | [10.81, 12.21]
      89      | Male    |     11.11 | 0.66 | [ 9.81, 12.41]
      18      | Female  |     11.42 | 0.50 | [10.44, 12.40]
      44      | Female  |     11.92 | 0.26 | [11.41, 12.44]
      54      | Female  |     12.12 | 0.22 | [11.69, 12.55]
      63      | Female  |     12.29 | 0.23 | [11.84, 12.74]
      
      Variable predicted: neg_c_7
      Predictors modulated: c160age=[fivenum], c161sex
      Predictors controlled: barthtot (65), e16sex (male)

---

    Code
      estimate_means(model, by = c("c160age=[fivenum]", "c161sex"), backend = "marginaleffects")
    Output
      Estimated Marginal Means
      
      c160age | c161sex |  Mean |   SE |         95% CI | t(865)
      ----------------------------------------------------------
      18      | Male    | 12.08 | 0.60 | [10.91, 13.26] |  20.22
      44      | Male    | 11.68 | 0.29 | [11.12, 12.24] |  40.98
      54      | Male    | 11.53 | 0.25 | [11.03, 12.02] |  45.75
      63      | Male    | 11.39 | 0.30 | [10.80, 11.97] |  38.07
      89      | Male    | 10.99 | 0.62 | [ 9.76, 12.21] |  17.64
      18      | Female  | 11.30 | 0.44 | [10.44, 12.17] |  25.63
      44      | Female  | 11.80 | 0.18 | [11.44, 12.17] |  63.99
      54      | Female  | 12.00 | 0.14 | [11.72, 12.28] |  84.30
      63      | Female  | 12.17 | 0.18 | [11.83, 12.52] |  69.56
      89      | Female  | 12.68 | 0.43 | [11.84, 13.51] |  29.74
      
      Variable predicted: neg_c_7
      Predictors modulated: c160age=[fivenum], c161sex
      Predictors averaged: barthtot (65), e16sex

