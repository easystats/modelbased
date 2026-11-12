# estimate_means - print footer for estimate types and re.form

    Code
      print(estimate_means(model, "sex", estimate = "specific"))
    Output
      Conditional Predictions
      
      sex    |    Mean |    SE |             95% CI | t(328)
      ------------------------------------------------------
      female | 3611.50 | 59.73 | [3494.00, 3729.01] |  60.46
      male   | 4148.88 | 38.84 | [4072.48, 4225.28] | 106.83
      
      Variable predicted: body_mass
      Predictors modulated: sex
      Predictors held at: species (Adelie), bill_len (44)

---

    Code
      print(estimate_means(model, "sex", estimate = "typical"))
    Output
      Conditional Predictions
      
      sex    |    Mean |    SE |             95% CI | t(328)
      ------------------------------------------------------
      female | 3873.98 | 25.06 | [3824.69, 3923.27] | 154.61
      male   | 4411.36 | 32.43 | [4347.56, 4475.15] | 136.03
      
      Variable predicted: body_mass
      Predictors modulated: sex
      Predictors held at: bill_len (44)
      Predictors averaged over (balanced): species

---

    Code
      print(estimate_means(model, "sex", estimate = "average"))
    Output
      Marginal Predictions
      
      sex    |    Mean |    SE |             95% CI | t(328)
      ------------------------------------------------------
      female | 3868.92 | 23.72 | [3822.27, 3915.58] | 163.13
      male   | 4545.49 | 23.74 | [4498.79, 4592.19] | 191.49
      
      Variable predicted: body_mass
      Predictors modulated: sex
      Predictors held at: bill_len (44)
      Predictors averaged over (within groups): species

---

    Code
      print(estimate_means(model, "sex", estimate = "population"))
    Output
      Marginal Predictions (G-computation)
      
      sex    |    Mean |    SE |             95% CI | t(328)
      ------------------------------------------------------
      female | 3939.15 | 27.46 | [3885.12, 3993.17] | 143.44
      male   | 4476.52 | 27.38 | [4422.66, 4530.38] | 163.50
      
      Variable predicted: body_mass
      Predictors modulated: sex
      Predictors held at: bill_len (44)
      Predictors averaged over (full sample): species

---

    Code
      print(estimate_means(m, by = "period", estimate = "typical"))
    Output
      Conditional Predictions
      
      period | Probability |   SE |       95% CI |    z
      -------------------------------------------------
      1      |        0.22 | 0.04 | [0.14, 0.29] | 5.76
      2      |        0.10 | 0.03 | [0.04, 0.15] | 3.66
      3      |        0.08 | 0.02 | [0.04, 0.13] | 3.39
      4      |        0.06 | 0.02 | [0.01, 0.10] | 2.50
      
      Variable predicted: cbind(incidence, size - incidence)
      Predictors modulated: period
      Results refer to a typical or to an average random effects group.
      Predictions are on the response-scale.

---

    Code
      print(estimate_means(m, by = "period", estimate = "typical", re.form = NA))
    Output
      Conditional Predictions
      
      period | Probability |   SE |       95% CI |    z
      -------------------------------------------------
      1      |        0.20 | 0.04 | [0.13, 0.27] | 5.36
      2      |        0.08 | 0.02 | [0.04, 0.13] | 3.52
      3      |        0.07 | 0.02 | [0.03, 0.12] | 3.27
      4      |        0.05 | 0.02 | [0.01, 0.09] | 2.44
      
      Variable predicted: cbind(incidence, size - incidence)
      Predictors modulated: period
      Predictions are on the response-scale.

---

    Code
      print(estimate_means(m, by = "period", estimate = "average"))
    Output
      Marginal Predictions
      
      period | Probability |   SE |       95% CI |    z
      -------------------------------------------------
      1      |        0.22 | 0.04 | [0.14, 0.29] | 5.76
      2      |        0.09 | 0.03 | [0.04, 0.14] | 3.65
      3      |        0.08 | 0.02 | [0.03, 0.13] | 3.38
      4      |        0.06 | 0.02 | [0.01, 0.10] | 2.50
      
      Variable predicted: cbind(incidence, size - incidence)
      Predictors modulated: period
      Results are marginalized over random effects levels of: herd
      Predictions are on the response-scale.

---

    Code
      print(estimate_means(m, by = "period", estimate = "average", re.form = NA))
    Output
      Marginal Predictions
      
      period | Probability |   SE |       95% CI |    z
      -------------------------------------------------
      1      |        0.20 | 0.04 | [0.13, 0.27] | 5.36
      2      |        0.08 | 0.02 | [0.04, 0.13] | 3.52
      3      |        0.07 | 0.02 | [0.03, 0.12] | 3.27
      4      |        0.05 | 0.02 | [0.01, 0.09] | 2.44
      
      Variable predicted: cbind(incidence, size - incidence)
      Predictors modulated: period
      Predictions are on the response-scale.

