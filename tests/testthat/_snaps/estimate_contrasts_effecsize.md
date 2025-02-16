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

---

    Code
      estimate_contrasts(model, effectsize = "none", backend = "emmeans")
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

---

    Code
      estimate_contrasts(model, effectsize = "emmeans", backend = "emmeans")
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
      
      Level1     | partial_d |      es 95% CI
      ---------------------------------------
      setosa     |      1.94 | [ 1.48,  2.39]
      setosa     |      1.34 | [ 0.91,  1.76]
      versicolor |     -0.60 | [-1.00, -0.20]
      
      Variable predicted: Sepal.Width
      Predictors contrasted: Species
      p-values are uncorrected.

---

    Code
      estimate_contrasts(model, effectsize = "marginal", backend = "emmeans")
    Message
      No variable was specified for contrast estimation. Selecting `contrast =
        "Species"`.
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |         95% CI |   SE | t(147) |      p | marginal_d
      -------------------------------------------------------------------------------------------
      setosa     | versicolor |       0.66 | [ 0.52,  0.79] | 0.07 |   9.69 | < .001 |       1.16
      setosa     | virginica  |       0.45 | [ 0.32,  0.59] | 0.07 |   6.68 | < .001 |       0.80
      versicolor | virginica  |      -0.20 | [-0.34, -0.07] | 0.07 |  -3.00 |  0.003 |      -0.36
      
      Variable predicted: Sepal.Width
      Predictors contrasted: Species
      p-values are uncorrected.

---

    Code
      estimate_contrasts(model, effectsize = "bootES", backend = "emmeans")
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
      
      Level1     | cohens.d | cohens.d 95% CI
      ---------------------------------------
      setosa     |     1.94 |  [ 1.52,  2.39]
      setosa     |     1.34 |  [ 0.82,  1.72]
      versicolor |    -0.60 |  [-0.92, -0.22]
      
      Variable predicted: Sepal.Width
      Predictors contrasted: Species
      p-values are uncorrected.

---

    Code
      estimate_contrasts(model, effectsize = "bootES", bootES_type = "akp.robust.d",
        backend = "emmeans")
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
      
      Level1     | akp.robust.d | akp.robust.d 95% CI
      -----------------------------------------------
      setosa     |         1.88 |      [ 1.42,  2.49]
      setosa     |         1.37 |      [ 0.92,  1.78]
      versicolor |        -0.51 |      [-0.90, -0.17]
      
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

