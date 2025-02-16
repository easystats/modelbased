# estimate_means() - mixed models

    Code
      estimate_means(m, c("mined", "spp"), backend = "marginaleffects")
    Output
      Estimated Marginal Means
      
      mined | spp   | Mean |   SE |       95% CI |     z
      --------------------------------------------------
      yes   | GP    | 0.12 | 0.09 | [0.03, 0.55] | -2.71
      no    | GP    | 2.63 | 0.38 | [1.98, 3.48] |  6.75
      yes   | PR    | 0.30 | 0.17 | [0.10, 0.90] | -2.15
      no    | PR    | 0.63 | 0.16 | [0.38, 1.05] | -1.76
      yes   | DM    | 1.21 | 0.44 | [0.59, 2.48] |  0.51
      no    | DM    | 3.16 | 0.44 | [2.40, 4.16] |  8.22
      yes   | EC-A  | 0.23 | 0.14 | [0.07, 0.75] | -2.43
      no    | EC-A  | 1.49 | 0.32 | [0.97, 2.27] |  1.83
      yes   | EC-L  | 0.56 | 0.22 | [0.26, 1.22] | -1.47
      no    | EC-L  | 5.11 | 0.64 | [3.99, 6.53] | 12.96
      yes   | DES-L | 1.26 | 0.43 | [0.64, 2.47] |  0.66
      no    | DES-L | 4.68 | 0.58 | [3.66, 5.97] | 12.34
      yes   | DF    | 1.22 | 0.38 | [0.67, 2.23] |  0.65
      no    | DF    | 2.53 | 0.39 | [1.87, 3.41] |  6.04
      
      Variable predicted: count
      Predictors modulated: mined, spp
      Predictors averaged: site
      Predictions are on the response-scale.

---

    Code
      estimate_means(m, c("mined", "spp"), backend = "marginaleffects")
    Output
      Estimated Marginal Means
      
      mined | spp   | Mean |       95% CI
      -----------------------------------
      yes   | GP    | 0.26 | [0.20, 0.34]
      no    | GP    | 2.01 | [1.66, 2.43]
      yes   | PR    | 0.07 | [0.04, 0.10]
      no    | PR    | 0.50 | [0.34, 0.73]
      yes   | DM    | 0.33 | [0.26, 0.43]
      no    | DM    | 2.53 | [2.14, 3.00]
      yes   | EC-A  | 0.12 | [0.09, 0.17]
      no    | EC-A  | 0.93 | [0.70, 1.23]
      yes   | EC-L  | 0.49 | [0.38, 0.62]
      no    | EC-L  | 3.74 | [3.25, 4.30]
      yes   | DES-L | 0.52 | [0.41, 0.65]
      no    | DES-L | 3.96 | [3.46, 4.54]
      yes   | DF    | 0.28 | [0.22, 0.37]
      no    | DF    | 2.18 | [1.81, 2.61]
      
      Variable predicted: count
      Predictors modulated: mined, spp
      Predictions are on the response-scale.

