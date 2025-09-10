# estimate_contrast, counterfactual, snapshots, Level-columns

    Code
      print(out, table_width = Inf)
    Output
      Counterfactual Contrasts Analysis (G-computation)
      
      Level1 | Level2  | Difference |   SE |       95% CI |    z |     p
      ------------------------------------------------------------------
      tx     | control |       5.65 | 1.95 | [1.84, 9.47] | 2.91 | 0.004
      
      Variable predicted: QoL
      Predictors contrasted: treatment
      Predictors averaged: time, education, hospital (0.95), age (0.22), phq4 (-0.076), ID
      p-values are uncorrected.

---

    Code
      print(out, table_width = Inf)
    Output
      Counterfactual Contrasts Analysis (G-computation)
      
      Level1 | Level2  | time | Difference |   SE |         95% CI |     z |      p
      -----------------------------------------------------------------------------
      tx     | control | pre  |      -1.40 | 2.26 | [-5.82,  3.03] | -0.62 |  0.535
      tx     | control | post |       3.32 | 2.26 | [-1.10,  7.75] |  1.47 |  0.141
      tx     | control | end  |      15.00 | 2.26 | [10.57, 19.42] |  6.64 | < .001
      
      Variable predicted: QoL
      Predictors contrasted: treatment
      Predictors averaged: education, hospital (0.95), age (0.22), phq4 (-0.076), ID
      p-values are uncorrected.

---

    Code
      print(out, table_width = Inf)
    Output
      Counterfactual Contrasts Analysis (G-computation)
      
      Level1 | Level2 | treatment | Difference |   SE |         95% CI |     z |      p
      ---------------------------------------------------------------------------------
      end    | pre    | control   |       0.86 | 1.21 | [-1.51,  3.24] |  0.71 |  0.477
      post   | pre    | control   |       1.85 | 1.22 | [-0.53,  4.23] |  1.52 |  0.128
      end    | pre    | tx        |      17.26 | 1.57 | [14.18, 20.34] | 10.99 | < .001
      post   | pre    | tx        |       6.57 | 1.57 | [ 3.50,  9.65] |  4.19 | < .001
      
      Variable predicted: QoL
      Predictors contrasted: time
      Predictors averaged: education, hospital (0.95), age (0.22), phq4 (-0.076), ID
      p-values are uncorrected.

