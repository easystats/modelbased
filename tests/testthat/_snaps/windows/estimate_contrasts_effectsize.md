# estimate_contrasts - emmeans backend

    Code
      estimate_contrasts(model, backend = "emmeans")
    Message
      No variable was specified for contrast estimation. Selecting `contrast =
        "Species"`.
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |         95% CI |   SE | t(147) |      p
      ------------------------------------------------------------------------------
      setosa     | versicolor |       0.66 | [ 0.52,  0.79] | 0.07 |   9.69 | < .001
      setosa     | virginica  |       0.45 | [ 0.32,  0.59] | 0.07 |   6.68 | < .001
      versicolor | virginica  |      -0.20 | [-0.34, -0.07] | 0.07 |  -3.00 |  0.003
      
      Variable predicted: Sepal.Width
      Predictors contrasted: Species
      p-values are uncorrected.

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

