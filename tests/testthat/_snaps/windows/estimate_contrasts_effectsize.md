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
      versicolor | setosa     |      -0.66 | [-0.79, -0.52] | 0.07 |  -9.69 | < .001
      virginica  | setosa     |      -0.45 | [-0.59, -0.32] | 0.07 |  -6.68 | < .001
      virginica  | versicolor |       0.20 | [ 0.07,  0.34] | 0.07 |   3.00 |  0.003
      
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
      versicolor | setosa     |      -0.66 | [-0.79, -0.52] | 0.07 |  -9.69 | < .001
      virginica  | setosa     |      -0.45 | [-0.59, -0.32] | 0.07 |  -6.68 | < .001
      virginica  | versicolor |       0.20 | [ 0.07,  0.34] | 0.07 |   3.00 |  0.003
      
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
      versicolor | setosa     |      -0.66 | [-0.79, -0.52] | 0.07 |  -9.69 | < .001
      virginica  | setosa     |      -0.45 | [-0.59, -0.32] | 0.07 |  -6.68 | < .001
      virginica  | versicolor |       0.20 | [ 0.07,  0.34] | 0.07 |   3.00 |  0.003
      
      Level1     | partial_d |      es 95% CI
      ---------------------------------------
      versicolor |     -1.94 | [-2.39, -1.48]
      virginica  |     -1.34 | [-1.76, -0.91]
      virginica  |      0.60 | [ 0.20,  1.00]
      
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
      versicolor | setosa     |      -0.66 | [-0.79, -0.52] | 0.07 |  -9.69 | < .001 |      -1.16
      virginica  | setosa     |      -0.45 | [-0.59, -0.32] | 0.07 |  -6.68 | < .001 |      -0.80
      virginica  | versicolor |       0.20 | [ 0.07,  0.34] | 0.07 |   3.00 |  0.003 |       0.36
      
      Variable predicted: Sepal.Width
      Predictors contrasted: Species
      p-values are uncorrected.

---

    Code
      estimate_contrasts(model, effectsize = "boot", backend = "emmeans")
    Message
      No variable was specified for contrast estimation. Selecting `contrast =
        "Species"`.
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |         95% CI |   SE | t(147) |      p
      ------------------------------------------------------------------------------
      versicolor | setosa     |      -0.66 | [-0.79, -0.52] | 0.07 |  -9.69 | < .001
      virginica  | setosa     |      -0.45 | [-0.59, -0.32] | 0.07 |  -6.68 | < .001
      virginica  | versicolor |       0.20 | [ 0.07,  0.34] | 0.07 |   3.00 |  0.003
      
      Level1     | Cohen's d | Cohen's d 95% CI
      -----------------------------------------
      versicolor |     -1.94 |   [-2.39, -1.52]
      virginica  |     -1.34 |   [-1.72, -0.82]
      virginica  |      0.60 |   [ 0.22,  0.92]
      
      Variable predicted: Sepal.Width
      Predictors contrasted: Species
      p-values are uncorrected.

---

    Code
      estimate_contrasts(model, effectsize = "boot", es_type = "akp.robust.d",
        backend = "emmeans")
    Message
      No variable was specified for contrast estimation. Selecting `contrast =
        "Species"`.
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |         95% CI |   SE | t(147) |      p
      ------------------------------------------------------------------------------
      versicolor | setosa     |      -0.66 | [-0.79, -0.52] | 0.07 |  -9.69 | < .001
      virginica  | setosa     |      -0.45 | [-0.59, -0.32] | 0.07 |  -6.68 | < .001
      virginica  | versicolor |       0.20 | [ 0.07,  0.34] | 0.07 |   3.00 |  0.003
      
      Level1     | Robust Cohen's d | Robust Cohen's d 95% CI
      -------------------------------------------------------
      versicolor |            -1.88 |          [-2.49, -1.42]
      virginica  |            -1.37 |          [-1.78, -0.92]
      virginica  |             0.51 |          [ 0.17,  0.90]
      
      Variable predicted: Sepal.Width
      Predictors contrasted: Species
      p-values are uncorrected.

---

    Code
      estimate_contrasts(model, effectsize = "boot", es_type = "hedges.g", backend = "emmeans")
    Message
      No variable was specified for contrast estimation. Selecting `contrast =
        "Species"`.
    Output
      Marginal Contrasts Analysis
      
      Level1     | Level2     | Difference |         95% CI |   SE | t(147) |      p
      ------------------------------------------------------------------------------
      versicolor | setosa     |      -0.66 | [-0.79, -0.52] | 0.07 |  -9.69 | < .001
      virginica  | setosa     |      -0.45 | [-0.59, -0.32] | 0.07 |  -6.68 | < .001
      virginica  | versicolor |       0.20 | [ 0.07,  0.34] | 0.07 |   3.00 |  0.003
      
      Level1     | Hedges' g | Hedges' g 95% CI
      -----------------------------------------
      versicolor |     -1.93 |   [-2.38, -1.51]
      virginica  |     -1.33 |   [-1.71, -0.82]
      virginica  |      0.60 |   [ 0.22,  0.91]
      
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

