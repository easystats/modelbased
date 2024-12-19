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

