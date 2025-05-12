# estimate_contrasts - joint test, works with select printing

    Code
      estimate_contrasts(m, contrast = "time", by = "coffee", comparison = "joint")
    Output
      Marginal Joint Test
      
      Contrast | coffee  | df1 | df2 |    F |     p
      ---------------------------------------------
      time     | coffee  | 2   | 114 | 1.76 | 0.176
      time     | control | 2   | 114 | 5.29 | 0.006
      
      p-values are uncorrected.

