# estimate_means for betareg

    Code
      print(out, zap_small = TRUE)
    Output
      Estimated Marginal Means
      
      batch | Proportion |   SE |       95% CI |     z
      ------------------------------------------------
      1     |       0.31 | 0.01 | [0.29, 0.34] | 26.00
      2     |       0.23 | 0.01 | [0.21, 0.26] | 16.76
      3     |       0.28 | 0.01 | [0.25, 0.31] | 18.78
      4     |       0.19 | 0.01 | [0.17, 0.21] | 19.96
      5     |       0.20 | 0.01 | [0.18, 0.22] | 19.16
      6     |       0.19 | 0.01 | [0.17, 0.21] | 17.83
      7     |       0.12 | 0.01 | [0.11, 0.14] | 15.57
      8     |       0.12 | 0.01 | [0.10, 0.13] | 14.56
      9     |       0.11 | 0.01 | [0.09, 0.12] | 12.25
      10    |       0.07 | 0.01 | [0.06, 0.09] | 12.78
      
      Variable predicted: yield
      Predictors modulated: batch
      Predictors averaged: temp (3.3e+02)
      Predictions are on the response-scale.

# estimate_relation for betareg

    Code
      print(out, zap_small = TRUE)
    Output
      Model-based Predictions
      
      batch | Predicted |       95% CI
      --------------------------------
      1     |      0.31 | [0.28, 0.35]
      2     |      0.23 | [0.21, 0.25]
      3     |      0.28 | [0.25, 0.31]
      4     |      0.19 | [0.17, 0.21]
      5     |      0.20 | [0.18, 0.22]
      6     |      0.19 | [0.17, 0.21]
      7     |      0.12 | [0.11, 0.14]
      8     |      0.12 | [0.10, 0.13]
      9     |      0.11 | [0.09, 0.12]
      10    |      0.07 | [0.06, 0.09]
      
      Variable predicted: yield
      Predictors modulated: batch
      Predictors controlled: temp (3.3e+02)
      Predictions are on the response-scale.

