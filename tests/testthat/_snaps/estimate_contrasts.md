# estimate_contrasts - marginaleffects

    Code
      print(out, zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      time                  |            coffee | Difference |   SE |          95% CI | t(113) |      p
      -------------------------------------------------------------------------------------------------
      morning - morning     |  coffee - control |       5.78 | 1.99 | [  1.83,  9.73] |   2.90 | 0.004 
      morning - noon        |   coffee - coffee |       1.93 | 1.99 | [ -2.02,  5.88] |   0.97 | 0.336 
      morning - noon        |  coffee - control |       0.00 | 1.99 | [ -3.95,  3.95] |   0.00 | > .999
      morning - afternoon   |   coffee - coffee |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 | 0.336 
      morning - afternoon   |  coffee - control |       0.00 | 1.99 | [ -3.95,  3.95] |   0.00 | > .999
      morning - noon        |  control - coffee |      -3.86 | 1.99 | [ -7.81,  0.09] |  -1.93 | 0.056 
      morning - noon        | control - control |      -5.78 | 1.99 | [ -9.73, -1.83] |  -2.90 | 0.004 
      morning - afternoon   |  control - coffee |      -7.71 | 1.99 | [-11.66, -3.76] |  -3.87 | < .001
      morning - afternoon   | control - control |      -5.78 | 1.99 | [ -9.73, -1.83] |  -2.90 | 0.004 
      noon - noon           |  coffee - control |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 | 0.336 
      noon - afternoon      |   coffee - coffee |      -3.86 | 1.99 | [ -7.81,  0.09] |  -1.93 | 0.056 
      noon - afternoon      |  coffee - control |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 | 0.336 
      noon - afternoon      |  control - coffee |      -1.93 | 1.99 | [ -5.88,  2.02] |  -0.97 | 0.336 
      noon - afternoon      | control - control |       0.00 | 1.99 | [ -3.95,  3.95] |   0.00 | > .999
      afternoon - afternoon |  coffee - control |       1.93 | 1.99 | [ -2.02,  5.88] |   0.97 | 0.336 
      
      Marginal contrasts estimated at time, coffee
      p-values are uncorrected.

---

    Code
      print(out, zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      coffee  |              hypothesis | Difference |   SE |       95% CI | t(113) |      p
      --------------------------------------------------------------------------------------
      coffee  |      (noon) / (morning) |       0.89 | 0.11 | [0.67, 1.11] |   8.08 | < .001
      coffee  | (afternoon) / (morning) |       1.11 | 0.12 | [0.87, 1.36] |   9.05 | < .001
      control |      (noon) / (morning) |       1.51 | 0.22 | [1.06, 1.95] |   6.73 | < .001
      control | (afternoon) / (morning) |       1.51 | 0.22 | [1.06, 1.95] |   6.73 | < .001
      
      Marginal contrasts estimated at time, coffee
      p-values are uncorrected.

---

    Code
      print(out, zap_small = TRUE, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Parameter       | Difference |   SE |          95% CI | t(113) |     p
      ----------------------------------------------------------------------
      (b2-b1)=(b4-b3) |      -7.71 | 2.82 | [-13.30, -2.12] |  -2.73 | 0.007
      
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
    Message
      We selected `contrast=c("Species")`.
    Output
      Marginal Contrasts Analysis
      
      Species                | Difference |   SE |         95% CI |     z |      p
      ----------------------------------------------------------------------------
      setosa - versicolor    |      -0.68 | 0.07 | [-0.82, -0.54] | -9.27 | < .001
      setosa - virginica     |      -0.50 | 0.08 | [-0.67, -0.33] | -5.90 | < .001
      versicolor - virginica |       0.18 | 0.08 | [ 0.01,  0.35] |  2.12 | 0.034 
      
      Marginal contrasts estimated at Species
      p-value adjustment method: Holm (1979)

# estimate_contrasts - filtering works

    Code
      print(out, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      c172code   | Difference |   SE |        95% CI | t(827) |     p
      ---------------------------------------------------------------
      low - mid  |       0.09 | 0.34 | [-0.58, 0.76] |   0.25 | 0.802
      low - high |      -0.61 | 0.43 | [-1.45, 0.24] |  -1.40 | 0.323
      mid - high |      -0.69 | 0.36 | [-1.40, 0.02] |  -1.92 | 0.166
      
      Marginal contrasts estimated at c172code
      p-value adjustment method: Holm (1979)

---

    Code
      print(out, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      c161sex         |    c172code | Difference |   SE |         95% CI | t(825) |      p
      ------------------------------------------------------------------------------------
      Male - Male     |   low - mid |      -0.29 | 0.71 | [-1.68,  1.10] |  -0.41 | > .999
      Male - Male     |  low - high |      -0.69 | 0.83 | [-2.32,  0.95] |  -0.82 | > .999
      Male - Female   |   low - low |      -1.05 | 0.69 | [-2.40,  0.31] |  -1.51 | > .999
      Male - Female   |   low - mid |      -0.85 | 0.64 | [-2.10,  0.40] |  -1.33 | > .999
      Male - Female   |  low - high |      -1.65 | 0.71 | [-3.05, -0.24] |  -2.30 | 0.301 
      Male - Male     |  mid - high |      -0.40 | 0.68 | [-1.73,  0.94] |  -0.59 | > .999
      Male - Female   |   mid - low |      -0.76 | 0.50 | [-1.73,  0.22] |  -1.52 | > .999
      Male - Female   |   mid - mid |      -0.56 | 0.42 | [-1.38,  0.26] |  -1.35 | > .999
      Male - Female   |  mid - high |      -1.36 | 0.53 | [-2.39, -0.32] |  -2.58 | 0.151 
      Male - Female   |  high - low |      -0.36 | 0.66 | [-1.66,  0.95] |  -0.54 | > .999
      Male - Female   |  high - mid |      -0.16 | 0.61 | [-1.35,  1.03] |  -0.27 | > .999
      Male - Female   | high - high |      -0.96 | 0.69 | [-2.30,  0.39] |  -1.40 | > .999
      Female - Female |   low - mid |       0.20 | 0.39 | [-0.57,  0.96] |   0.51 | > .999
      Female - Female |  low - high |      -0.60 | 0.51 | [-1.59,  0.39] |  -1.18 | > .999
      Female - Female |  mid - high |      -0.80 | 0.43 | [-1.63,  0.04] |  -1.87 | 0.799 
      
      Marginal contrasts estimated at c161sex, c172code
      p-value adjustment method: Holm (1979)

---

    Code
      print(out, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      c161sex       | c172code | Difference |   SE |        95% CI | t(825) |      p
      ------------------------------------------------------------------------------
      Male - Female |      low |      -1.05 | 0.69 | [-2.40, 0.31] |  -1.51 | > .999
      Male - Female |      mid |      -0.56 | 0.42 | [-1.38, 0.26] |  -1.35 | > .999
      Male - Female |     high |      -0.96 | 0.69 | [-2.30, 0.39] |  -1.40 | > .999
      
      Marginal contrasts estimated at c161sex
      p-value adjustment method: Holm (1979)

---

    Code
      print(out, table_width = Inf)
    Output
      Estimated Marginal Effects
      
      Slope |       SE |         95% CI |      t |      p
      ---------------------------------------------------
      -0.05 | 4.20e-03 | [-0.06, -0.05] | -12.77 | < .001
      Marginal effects estimated for barthtot

---

    Code
      print(out, table_width = Inf)
    Output
      Estimated Marginal Effects
      
      c172code | Slope |       SE |         95% CI |     t |      p
      -------------------------------------------------------------
      low      | -0.06 | 9.06e-03 | [-0.08, -0.05] | -7.08 | < .001
      mid      | -0.05 | 5.28e-03 | [-0.06, -0.04] | -9.82 | < .001
      high     | -0.05 |     0.01 | [-0.07, -0.03] | -4.51 | < .001
      Marginal effects estimated for barthtot

---

    Code
      print(out, table_width = Inf)
    Output
      Marginal Contrasts Analysis
      
      Parameter  | Difference |   SE |        95% CI |     t |     p
      --------------------------------------------------------------
      low - mid  |      -0.01 | 0.01 | [-0.03, 0.01] | -1.17 | 0.728
      low - high |      -0.02 | 0.01 | [-0.04, 0.01] | -1.10 | 0.728
      mid - high |  -3.26e-03 | 0.01 | [-0.03, 0.02] | -0.27 | 0.786
      
      Marginal contrasts estimated at barthtot
      p-value adjustment method: Holm (1979)

