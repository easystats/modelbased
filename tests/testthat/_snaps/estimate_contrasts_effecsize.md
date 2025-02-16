# estimate_contrasts - marginaleffects backend

    Code
      estimate_contrasts(model, backend = "marginaleffects")
    Message
      We selected `contrast=c("Species")`.
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |   SE |         95% CI | t(147) |      p
      ------------------------------------------------------------------------------
      versicolor | setosa     |      -0.66 | 0.07 | [-0.79, -0.52] |  -9.69 | < .001
      virginica  | setosa     |      -0.45 | 0.07 | [-0.59, -0.32] |  -6.68 | < .001
      virginica  | versicolor |       0.20 | 0.07 | [ 0.07,  0.34] |   3.00 |  0.003
      
      Variable predicted: Sepal.Width
      Predictors contrasted: Species
      p-values are uncorrected.

---

    Code
      estimate_contrasts(model, effectsize = "none", backend = "marginaleffects")
    Message
      We selected `contrast=c("Species")`.
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |   SE |         95% CI | t(147) |      p
      ------------------------------------------------------------------------------
      versicolor | setosa     |      -0.66 | 0.07 | [-0.79, -0.52] |  -9.69 | < .001
      virginica  | setosa     |      -0.45 | 0.07 | [-0.59, -0.32] |  -6.68 | < .001
      virginica  | versicolor |       0.20 | 0.07 | [ 0.07,  0.34] |   3.00 |  0.003
      
      Variable predicted: Sepal.Width
      Predictors contrasted: Species
      p-values are uncorrected.

---

    Code
      estimate_contrasts(model, effectsize = "marginal", backend = "marginaleffects")
    Message
      We selected `contrast=c("Species")`.
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |   SE |         95% CI | t(147) |      p | marginal_d
      -------------------------------------------------------------------------------------------
      versicolor | setosa     |      -0.66 | 0.07 | [-0.79, -0.52] |  -9.69 | < .001 |      -1.16
      virginica  | setosa     |      -0.45 | 0.07 | [-0.59, -0.32] |  -6.68 | < .001 |      -0.80
      virginica  | versicolor |       0.20 | 0.07 | [ 0.07,  0.34] |   3.00 |  0.003 |       0.36
      
      Variable predicted: Sepal.Width
      Predictors contrasted: Species
      p-values are uncorrected.

