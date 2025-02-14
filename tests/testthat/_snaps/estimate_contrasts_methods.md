# estimate_contrasts - Random Effects Levels, pairwise

    Code
      print(estimate_contrasts(estim, contrast = c("gender", "employed", "age")),
      zap_small = TRUE, table_width = Inf)
    Output
      Model-based Contrasts Analysis
      
      Level1             | Level2             | Difference |   SE |         95% CI | Statistic |      p
      -------------------------------------------------------------------------------------------------
      Male, no, -40      | Female, no, -40    |       0.07 | 1.11 | [-2.11,  2.25] |      0.06 |  0.951
      Male, no, -40      | Male, yes, -40     |      -0.88 | 1.18 | [-3.18,  1.43] |     -0.75 |  0.455
      Female, no, -40    | Male, yes, -40     |      -0.95 | 1.04 | [-2.99,  1.10] |     -0.91 |  0.364
      Male, no, -40      | Female, yes, -40   |      -0.79 | 1.07 | [-2.89,  1.30] |     -0.74 |  0.457
      Female, no, -40    | Female, yes, -40   |      -0.86 | 0.92 | [-2.67,  0.94] |     -0.94 |  0.348
      Male, yes, -40     | Female, yes, -40   |       0.08 | 0.99 | [-1.86,  2.03] |      0.08 |  0.932
      Male, no, -40      | Male, no, 41-64    |       0.30 | 1.10 | [-1.84,  2.45] |      0.28 |  0.781
      Female, no, -40    | Male, no, 41-64    |       0.24 | 0.95 | [-1.63,  2.10] |      0.25 |  0.805
      Male, yes, -40     | Male, no, 41-64    |       1.18 | 1.02 | [-0.83,  3.19] |      1.15 |  0.248
      Female, yes, -40   | Male, no, 41-64    |       1.10 | 0.90 | [-0.66,  2.86] |      1.22 |  0.221
      Male, no, -40      | Female, no, 41-64  |       1.49 | 0.94 | [-0.35,  3.32] |      1.59 |  0.112
      Female, no, -40    | Female, no, 41-64  |       1.42 | 0.76 | [-0.08,  2.91] |      1.86 |  0.063
      Male, yes, -40     | Female, no, 41-64  |       2.37 | 0.85 | [ 0.70,  4.04] |      2.78 |  0.005
      Female, yes, -40   | Female, no, 41-64  |       2.28 | 0.69 | [ 0.92,  3.64] |      3.29 |  0.001
      Male, no, 41-64    | Female, no, 41-64  |       1.18 | 0.74 | [-0.26,  2.63] |      1.60 |  0.109
      Male, no, -40      | Male, yes, 41-64   |      -0.23 | 1.04 | [-2.26,  1.81] |     -0.22 |  0.827
      Female, no, -40    | Male, yes, 41-64   |      -0.30 | 0.88 | [-2.03,  1.44] |     -0.33 |  0.738
      Male, yes, -40     | Male, yes, 41-64   |       0.65 | 0.96 | [-1.23,  2.54] |      0.68 |  0.498
      Female, yes, -40   | Male, yes, 41-64   |       0.57 | 0.83 | [-1.05,  2.19] |      0.69 |  0.492
      Male, no, 41-64    | Male, yes, 41-64   |      -0.53 | 0.86 | [-2.22,  1.16] |     -0.62 |  0.538
      Female, no, 41-64  | Male, yes, 41-64   |      -1.71 | 0.65 | [-2.98, -0.44] |     -2.65 |  0.008
      Male, no, -40      | Female, yes, 41-64 |       1.15 | 0.94 | [-0.70,  2.99] |      1.22 |  0.224
      Female, no, -40    | Female, yes, 41-64 |       1.08 | 0.77 | [-0.43,  2.59] |      1.40 |  0.162
      Male, yes, -40     | Female, yes, 41-64 |       2.02 | 0.86 | [ 0.34,  3.71] |      2.36 |  0.018
      Female, yes, -40   | Female, yes, 41-64 |       1.94 | 0.70 | [ 0.56,  3.32] |      2.76 |  0.006
      Male, no, 41-64    | Female, yes, 41-64 |       0.84 | 0.74 | [-0.62,  2.30] |      1.13 |  0.258
      Female, no, 41-64  | Female, yes, 41-64 |      -0.34 | 0.48 | [-1.28,  0.60] |     -0.71 |  0.476
      Male, yes, 41-64   | Female, yes, 41-64 |       1.37 | 0.66 | [ 0.09,  2.66] |      2.09 |  0.036
      Male, no, -40      | Male, no, 65++     |       0.83 | 1.08 | [-1.29,  2.94] |      0.77 |  0.443
      Female, no, -40    | Male, no, 65++     |       0.76 | 0.93 | [-1.07,  2.58] |      0.82 |  0.415
      Male, yes, -40     | Male, no, 65++     |       1.71 | 1.01 | [-0.26,  3.68] |      1.70 |  0.090
      Female, yes, -40   | Male, no, 65++     |       1.62 | 0.88 | [-0.10,  3.34] |      1.85 |  0.064
      Male, no, 41-64    | Male, no, 65++     |       0.52 | 0.91 | [-1.26,  2.31] |      0.57 |  0.565
      Female, no, 41-64  | Male, no, 65++     |      -0.66 | 0.71 | [-2.05,  0.73] |     -0.93 |  0.354
      Male, yes, 41-64   | Male, no, 65++     |       1.06 | 0.84 | [-0.59,  2.70] |      1.26 |  0.209
      Female, yes, 41-64 | Male, no, 65++     |      -0.32 | 0.72 | [-1.72,  1.09] |     -0.44 |  0.658
      Male, no, -40      | Female, no, 65++   |       1.71 | 0.98 | [-0.21,  3.63] |      1.75 |  0.081
      Female, no, -40    | Female, no, 65++   |       1.64 | 0.81 | [ 0.04,  3.23] |      2.01 |  0.044
      Male, yes, -40     | Female, no, 65++   |       2.59 | 0.90 | [ 0.83,  4.35] |      2.88 |  0.004
      Female, yes, -40   | Female, no, 65++   |       2.50 | 0.75 | [ 1.03,  3.97] |      3.34 | < .001
      Male, no, 41-64    | Female, no, 65++   |       1.40 | 0.79 | [-0.14,  2.95] |      1.78 |  0.076
      Female, no, 41-64  | Female, no, 65++   |       0.22 | 0.55 | [-0.85,  1.29] |      0.40 |  0.686
      Male, yes, 41-64   | Female, no, 65++   |       1.94 | 0.71 | [ 0.55,  3.32] |      2.74 |  0.006
      Female, yes, 41-64 | Female, no, 65++   |       0.56 | 0.56 | [-0.53,  1.65] |      1.01 |  0.312
      Male, no, 65++     | Female, no, 65++   |       0.88 | 0.77 | [-0.62,  2.38] |      1.15 |  0.250
      Male, no, -40      | Male, yes, 65++    |      -0.10 | 1.39 | [-2.83,  2.63] |     -0.07 |  0.942
      Female, no, -40    | Male, yes, 65++    |      -0.17 | 1.28 | [-2.69,  2.35] |     -0.13 |  0.895
      Male, yes, -40     | Male, yes, 65++    |       0.78 | 1.34 | [-1.85,  3.40] |      0.58 |  0.561
      Female, yes, -40   | Male, yes, 65++    |       0.69 | 1.25 | [-1.75,  3.13] |      0.56 |  0.577
      Male, no, 41-64    | Male, yes, 65++    |      -0.41 | 1.27 | [-2.89,  2.08] |     -0.32 |  0.750
      Female, no, 41-64  | Male, yes, 65++    |      -1.59 | 1.13 | [-3.81,  0.64] |     -1.40 |  0.162
      Male, yes, 41-64   | Male, yes, 65++    |       0.13 | 1.22 | [-2.26,  2.52] |      0.10 |  0.917
      Female, yes, 41-64 | Male, yes, 65++    |      -1.25 | 1.14 | [-3.48,  0.99] |     -1.09 |  0.274
      Male, no, 65++     | Male, yes, 65++    |      -0.93 | 1.25 | [-3.39,  1.53] |     -0.74 |  0.459
      Female, no, 65++   | Male, yes, 65++    |      -1.81 | 1.17 | [-4.10,  0.48] |     -1.55 |  0.122
      Male, no, -40      | Female, yes, 65++  |       0.39 | 1.36 | [-2.27,  3.05] |      0.29 |  0.775
      Female, no, -40    | Female, yes, 65++  |       0.32 | 1.25 | [-2.12,  2.76] |      0.26 |  0.798
      Male, yes, -40     | Female, yes, 65++  |       1.27 | 1.30 | [-1.28,  3.82] |      0.97 |  0.330
      Female, yes, -40   | Female, yes, 65++  |       1.18 | 1.20 | [-1.18,  3.54] |      0.98 |  0.326
      Male, no, 41-64    | Female, yes, 65++  |       0.08 | 1.23 | [-2.33,  2.49] |      0.07 |  0.946
      Female, no, 41-64  | Female, yes, 65++  |      -1.10 | 1.09 | [-3.23,  1.04] |     -1.01 |  0.313
      Male, yes, 41-64   | Female, yes, 65++  |       0.62 | 1.18 | [-1.69,  2.92] |      0.52 |  0.601
      Female, yes, 41-64 | Female, yes, 65++  |      -0.76 | 1.09 | [-2.90,  1.39] |     -0.69 |  0.489
      Male, no, 65++     | Female, yes, 65++  |      -0.44 | 1.21 | [-2.82,  1.94] |     -0.36 |  0.717
      Female, no, 65++   | Female, yes, 65++  |      -1.32 | 1.13 | [-3.53,  0.89] |     -1.17 |  0.241
      Male, yes, 65++    | Female, yes, 65++  |       0.49 | 1.50 | [-2.45,  3.43] |      0.33 |  0.745
      
      Variable predicted: qol
      Predictors contrasted: gender, employed, age

---

    Code
      print(estimate_contrasts(estim, contrast = c("gender", "employed"), by = "age"),
      zap_small = TRUE, table_width = Inf)
    Output
      Model-based Contrasts Analysis
      
      age   | Parameter             | Difference |   SE |         95% CI | Statistic |     p
      --------------------------------------------------------------------------------------
      -40   | Male no -Female no    |       0.07 | 1.11 | [-2.11,  2.25] |      0.06 | 0.951
      -40   | Male no -Male yes     |      -0.88 | 1.18 | [-3.18,  1.43] |     -0.75 | 0.455
      -40   | Female no -Male yes   |      -0.95 | 1.04 | [-2.99,  1.10] |     -0.91 | 0.364
      -40   | Male no -Female yes   |      -0.79 | 1.07 | [-2.89,  1.30] |     -0.74 | 0.457
      -40   | Female no -Female yes |      -0.86 | 0.92 | [-2.67,  0.94] |     -0.94 | 0.348
      -40   | Male yes -Female yes  |       0.08 | 0.99 | [-1.86,  2.03] |      0.08 | 0.932
      41-64 | Male no -Female no    |       1.18 | 0.74 | [-0.26,  2.63] |      1.60 | 0.109
      41-64 | Male no -Male yes     |      -0.53 | 0.86 | [-2.22,  1.16] |     -0.62 | 0.538
      41-64 | Female no -Male yes   |      -1.71 | 0.65 | [-2.98, -0.44] |     -2.65 | 0.008
      41-64 | Male no -Female yes   |       0.84 | 0.74 | [-0.62,  2.30] |      1.13 | 0.258
      41-64 | Female no -Female yes |      -0.34 | 0.48 | [-1.28,  0.60] |     -0.71 | 0.476
      41-64 | Male yes -Female yes  |       1.37 | 0.66 | [ 0.09,  2.66] |      2.09 | 0.036
      65+   | Male no -Female no    |       0.88 | 0.77 | [-0.62,  2.38] |      1.15 | 0.250
      65+   | Male no -Male yes     |      -0.93 | 1.25 | [-3.39,  1.53] |     -0.74 | 0.459
      65+   | Female no -Male yes   |      -1.81 | 1.17 | [-4.10,  0.48] |     -1.55 | 0.122
      65+   | Male no -Female yes   |      -0.44 | 1.21 | [-2.82,  1.94] |     -0.36 | 0.717
      65+   | Female no -Female yes |      -1.32 | 1.13 | [-3.53,  0.89] |     -1.17 | 0.241
      65+   | Male yes -Female yes  |       0.49 | 1.50 | [-2.45,  3.43] |      0.33 | 0.745
      
      Variable predicted: qol
      Predictors contrasted: gender, employed

---

    Code
      print(estimate_contrasts(estim, contrast = "employed", by = c("age", "gender")),
      zap_small = TRUE, table_width = Inf)
    Output
      Model-based Contrasts Analysis
      
      Level1 | Level2 | age   | gender | Difference |   SE |        95% CI | Statistic |     p
      ----------------------------------------------------------------------------------------
      no     | yes    | -40   | Female |      -0.86 | 0.92 | [-2.67, 0.94] |     -0.94 | 0.348
      no     | yes    | -40   | Male   |      -0.88 | 1.18 | [-3.18, 1.43] |     -0.75 | 0.455
      no     | yes    | 41-64 | Female |      -0.34 | 0.48 | [-1.28, 0.60] |     -0.71 | 0.476
      no     | yes    | 41-64 | Male   |      -0.53 | 0.86 | [-2.22, 1.16] |     -0.62 | 0.538
      no     | yes    | 65+   | Female |      -1.32 | 1.13 | [-3.53, 0.89] |     -1.17 | 0.241
      no     | yes    | 65+   | Male   |      -0.93 | 1.25 | [-3.39, 1.53] |     -0.74 | 0.459
      
      Variable predicted: qol
      Predictors contrasted: employed

# estimate_contrasts - Random Effects Levels, interaction

    Code
      print(estimate_contrasts(estim, contrast = c("age", "employed"), comparison = "interaction"),
      zap_small = TRUE, table_width = Inf)
    Output
      Model-based Contrasts Analysis
      
      age       | employed   | Difference |   SE |        95% CI | Statistic |      p
      -------------------------------------------------------------------------------
      -40-41-64 | no and yes |       0.00 | 2.21 | [-4.34, 4.34] |      0.00 | > .999
      -40-65+   | no and yes |       0.00 | 2.21 | [-4.34, 4.34] |      0.00 | > .999
      41-64-65+ | no and yes |       0.00 | 2.21 | [-4.34, 4.34] |      0.00 | > .999
      
      Variable predicted: qol
      Predictors contrasted: age, employed

