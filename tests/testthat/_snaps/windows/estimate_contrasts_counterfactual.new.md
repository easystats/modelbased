# estimate_contrast, counterfactual, snapshots, Level-columns

    Code
      print(out, table_width = Inf)
    Output
      Counterfactual Contrasts Analysis (G-computation)
      
      Level1 | Level2 | treatment | Difference |   SE |         95% CI |     z |      p
      ---------------------------------------------------------------------------------
      post   | pre    | control   |       1.85 | 1.22 | [-0.53,  4.23] |  1.52 |  0.128
      end    | pre    | control   |       0.86 | 1.21 | [-1.51,  3.24] |  0.71 |  0.477
      end    | post   | control   |      -0.99 | 1.22 | [-3.37,  1.40] | -0.81 |  0.417
      post   | pre    | tx        |       6.57 | 1.57 | [ 3.50,  9.65] |  4.19 | < .001
      end    | pre    | tx        |      17.26 | 1.57 | [14.18, 20.34] | 10.99 | < .001
      end    | post   | tx        |      10.69 | 1.57 | [ 7.62, 13.76] |  6.82 | < .001
      
      Variable predicted: QoL
      Predictors contrasted: time
      Predictors averaged: education, hospital (0.95), age (0.22), phq4 (-0.076), ID
      p-values are uncorrected.

