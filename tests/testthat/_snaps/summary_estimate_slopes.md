# summary.estimate_slopes

    Code
      summary(slopes)
    Message
      There might be too few data to accurately determine intervals. Consider
        setting `length = 100` (or larger) in your call to `estimate_slopes()`.
    Output
      Johnson-Neymann Intervals
      
      Group      | Start |  End | Direction | Confidence     
      -------------------------------------------------------
      setosa     |  1.00 | 1.62 | positive  | Not Significant
      versicolor |  3.17 | 5.04 | positive  | Significant    
      virginica  |  4.73 | 5.66 | positive  | Significant    
      virginica  |  5.97 | 6.90 | positive  | Not Significant
      
      Marginal effects estimated for Petal.Length
      Type of slope was dY/dX

