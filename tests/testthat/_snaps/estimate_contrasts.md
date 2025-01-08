# estimate_contrasts - marginaleffects

    Code
      print(out, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      Parameter                              | Difference |          95% CI |      p
      ------------------------------------------------------------------------------
      morning, coffee - morning, control     |       5.78 | [  1.83,  9.73] | 0.004 
      morning, coffee - noon, coffee         |       1.93 | [ -2.02,  5.88] | 0.336 
      morning, coffee - noon, control        |       0.00 | [ -3.95,  3.95] | > .999
      morning, coffee - afternoon, coffee    |      -1.93 | [ -5.88,  2.02] | 0.336 
      morning, coffee - afternoon, control   |       0.00 | [ -3.95,  3.95] | > .999
      morning, control - noon, coffee        |      -3.86 | [ -7.81,  0.09] | 0.056 
      morning, control - noon, control       |      -5.78 | [ -9.73, -1.83] | 0.004 
      morning, control - afternoon, coffee   |      -7.71 | [-11.66, -3.76] | < .001
      morning, control - afternoon, control  |      -5.78 | [ -9.73, -1.83] | 0.004 
      noon, coffee - noon, control           |      -1.93 | [ -5.88,  2.02] | 0.336 
      noon, coffee - afternoon, coffee       |      -3.86 | [ -7.81,  0.09] | 0.056 
      noon, coffee - afternoon, control      |      -1.93 | [ -5.88,  2.02] | 0.336 
      noon, control - afternoon, coffee      |      -1.93 | [ -5.88,  2.02] | 0.336 
      noon, control - afternoon, control     |       0.00 | [ -3.95,  3.95] | > .999
      afternoon, coffee - afternoon, control |       1.93 | [ -2.02,  5.88] | 0.336 
      
      Marginal contrasts estimated at time, coffee
      p-values are uncorrected.

---

    Code
      print(out, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      coffee  |              hypothesis | Difference |       95% CI |      p
      ----------------------------------------------------------------------
      coffee  |      (noon) / (morning) |       0.89 | [0.67, 1.11] | < .001
      coffee  | (afternoon) / (morning) |       1.11 | [0.87, 1.36] | < .001
      control |      (noon) / (morning) |       1.51 | [1.06, 1.95] | < .001
      control | (afternoon) / (morning) |       1.51 | [1.06, 1.95] | < .001
      
      Marginal contrasts estimated at time, coffee
      p-values are uncorrected.

---

    Code
      print(out, zap_small = TRUE)
    Output
      Marginal Contrasts Analysis
      
      Parameter       | Difference |          95% CI |     p
      ------------------------------------------------------
      (b2-b1)=(b4-b3) |      -7.71 | [-13.30, -2.12] | 0.007
      
      Marginal contrasts estimated at time, coffee
      p-values are uncorrected.

---

    Code
      estimate_contrasts(model)
    Message
      No variable was specified for contrast estimation. Selecting `contrast =
        "Species"`.
    Output
      Marginal Contrasts Analysis
      
      Level1     |     Level2 | Difference |         95% CI |   SE |  df |     z |      p
      -----------------------------------------------------------------------------------
      setosa     | versicolor |      -0.68 | [-0.86, -0.50] | 0.07 | Inf | -9.27 | < .001
      setosa     |  virginica |      -0.50 | [-0.70, -0.30] | 0.08 | Inf | -5.90 | < .001
      versicolor |  virginica |       0.18 | [-0.02,  0.38] | 0.08 | Inf |  2.12 | 0.034 
      
      Marginal contrasts estimated at Species
      p-value adjustment method: Holm (1979)
      Contrasts are on the response-scale.

---

    Code
      estimate_contrasts(model, backend = "marginaleffects")
    Output
      Marginal Contrasts Analysis
      
      Parameter              | Difference |         95% CI |      p
      -------------------------------------------------------------
      setosa - versicolor    |      -0.68 | [-0.82, -0.54] | < .001
      setosa - virginica     |      -0.50 | [-0.67, -0.33] | < .001
      versicolor - virginica |       0.18 | [ 0.01,  0.35] | 0.035 
      
      Marginal contrasts estimated at 
      p-value adjustment method: Holm (1979)

