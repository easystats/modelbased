# estimate_means - print footer for estimate types and re.form

    Code
      print(estimate_means(model, "sex", estimate = "specific"))
    Output
      Conditional Predictions
      
      sex    |    Mean |    SE |             95% CI | t(328)
      ------------------------------------------------------
      female | 3600.66 | 59.66 | [3483.30, 3718.02] |  60.36
      male   | 4148.03 | 39.02 | [4071.26, 4224.79] | 106.30
      
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
      female | 3866.03 | 25.25 | [3816.35, 3915.70] | 153.10
      male   | 4413.39 | 32.38 | [4349.70, 4477.09] | 136.30
      
      Variable predicted: body_mass
      Predictors modulated: sex
      Predictors held at: bill_len (44)
      Predictors averaged over a balanced grid: species

---

    Code
      print(estimate_means(model, "sex", estimate = "average"))
    Output
      Marginal Predictions
      
      sex    |    Mean |    SE |             95% CI | t(328)
      ------------------------------------------------------
      female | 3862.27 | 23.97 | [3815.12, 3909.43] | 161.12
      male   | 4545.68 | 23.76 | [4498.95, 4592.42] | 191.35
      
      Variable predicted: body_mass
      Predictors modulated: sex
      Predictors averaged within levels of sex: species, bill_len

---

    Code
      print(estimate_means(model, "sex", estimate = "population"))
    Output
      Marginal Predictions
      
      sex    |    Mean |    SE |             95% CI | t(328)
      ------------------------------------------------------
      female | 3930.91 | 27.57 | [3876.68, 3985.14] | 142.60
      male   | 4478.27 | 27.26 | [4424.65, 4531.90] | 164.29
      
      Variable predicted: body_mass
      Predictors modulated: sex
      Predictors averaged over the full sample: species, bill_len

---

    Code
      print(estimate_means(model, c("sex", "bill_dep"), estimate = "specific"))
    Output
      Conditional Predictions
      
      sex    | bill_dep |    Mean |     SE |             95% CI | t(326)
      ------------------------------------------------------------------
      female |    13.10 | 3035.95 | 120.96 | [2797.99, 3273.92] |  25.10
      male   |    13.10 | 3706.65 | 133.64 | [3443.73, 3969.56] |  27.74
      female |    14.03 | 3149.47 | 103.10 | [2946.65, 3352.29] |  30.55
      male   |    14.03 | 3766.00 | 114.83 | [3540.09, 3991.91] |  32.80
      female |    14.97 | 3263.11 |  86.61 | [3092.71, 3433.50] |  37.67
      male   |    14.97 | 3825.42 |  96.42 | [3635.73, 4015.11] |  39.67
      female |    15.90 | 3376.62 |  72.50 | [3233.99, 3519.25] |  46.57
      male   |    15.90 | 3884.78 |  78.75 | [3729.85, 4039.70] |  49.33
      female |    16.83 | 3490.14 |  62.37 | [3367.44, 3612.83] |  55.96
      male   |    16.83 | 3944.13 |  62.40 | [3821.37, 4066.89] |  63.21
      female |    17.77 | 3603.77 |  58.32 | [3489.05, 3718.50] |  61.80
      male   |    17.77 | 4003.55 |  48.72 | [3907.71, 4099.39] |  82.18
      female |    18.70 | 3717.29 |  61.57 | [3596.16, 3838.42] |  60.37
      male   |    18.70 | 4062.91 |  40.54 | [3983.15, 4142.66] | 100.22
      female |    19.63 | 3830.80 |  71.13 | [3690.87, 3970.74] |  53.85
      male   |    19.63 | 4122.26 |  41.25 | [4041.10, 4203.42] |  99.92
      female |    20.57 | 3944.44 |  84.91 | [3777.40, 4111.48] |  46.46
      male   |    20.57 | 4181.68 |  50.50 | [4082.33, 4281.03] |  82.81
      female |    21.50 | 4057.95 | 101.17 | [3858.93, 4256.98] |  40.11
      male   |    21.50 | 4241.03 |  64.70 | [4113.74, 4368.32] |  65.55
      
      Variable predicted: body_mass
      Predictors modulated: sex, bill_dep
      Predictors held at: species (Adelie), bill_len (44)

---

    Code
      print(estimate_means(model, c("sex", "bill_dep"), estimate = "typical"))
    Output
      Conditional Predictions
      
      sex    | bill_dep |    Mean |     SE |             95% CI | t(326)
      ------------------------------------------------------------------
      female |    13.10 | 3449.57 |  80.74 | [3290.73, 3608.42] |  42.72
      male   |    13.10 | 4120.27 | 105.41 | [3912.89, 4327.64] |  39.09
      female |    14.03 | 3563.09 |  61.08 | [3442.93, 3683.25] |  58.33
      male   |    14.03 | 4179.62 |  86.64 | [4009.17, 4350.07] |  48.24
      female |    14.97 | 3676.73 |  42.65 | [3592.83, 3760.63] |  86.21
      male   |    14.97 | 4239.04 |  68.47 | [4104.35, 4373.73] |  61.91
      female |    15.90 | 3790.24 |  28.07 | [3735.03, 3845.45] | 135.05
      male   |    15.90 | 4298.40 |  51.58 | [4196.93, 4399.86] |  83.34
      female |    16.83 | 3903.76 |  25.19 | [3854.19, 3953.32] | 154.95
      male   |    16.83 | 4357.75 |  37.70 | [4283.59, 4431.92] | 115.59
      female |    17.77 | 4017.39 |  36.89 | [3944.81, 4089.97] | 108.89
      male   |    17.77 | 4417.17 |  31.16 | [4355.88, 4478.46] | 141.78
      female |    18.70 | 4130.91 |  54.46 | [4023.78, 4238.04] |  75.86
      male   |    18.70 | 4476.53 |  36.19 | [4405.34, 4547.71] | 123.70
      female |    19.63 | 4244.42 |  73.81 | [4099.22, 4389.63] |  57.50
      male   |    19.63 | 4535.88 |  49.36 | [4438.77, 4632.99] |  91.89
      female |    20.57 | 4358.06 |  93.88 | [4173.38, 4542.74] |  46.42
      male   |    20.57 | 4595.30 |  65.99 | [4465.47, 4725.12] |  69.63
      female |    21.50 | 4471.57 | 114.25 | [4246.82, 4696.33] |  39.14
      male   |    21.50 | 4654.65 |  84.02 | [4489.36, 4819.95] |  55.40
      
      Variable predicted: body_mass
      Predictors modulated: sex, bill_dep
      Predictors held at: bill_len (44)
      Predictors averaged over a balanced grid: species

---

    Code
      print(estimate_means(model, c("sex", "bill_dep"), estimate = "average"))
    Condition
      Warning:
      One or all focal terms are numeric. With `estimate = "average"`, values
        are only based on the data points that actually exist. Consequently, the
        output may not represent a balanced range or grid of values from your
        focal term(s). Use `verbose = FALSE` to silence this warning.
    Output
      Marginal Predictions
      
      sex    | bill_dep |    Mean |    SE |             95% CI | t(326)
      -----------------------------------------------------------------
      female |    13.10 | 4478.19 | 51.66 | [4376.57, 4579.82] |  86.69
      female |    15.90 | 3155.13 | 45.18 | [3066.26, 3244.01] |  69.84
      male   |    15.90 | 5510.90 | 36.32 | [5439.46, 5582.34] | 151.75
      female |    18.70 | 3584.28 | 49.55 | [3486.80, 3681.76] |  72.33
      male   |    18.70 | 3951.07 | 29.34 | [3893.36, 4008.79] | 134.67
      male   |    21.50 | 4291.59 | 69.72 | [4154.43, 4428.76] |  61.55
      
      Variable predicted: body_mass
      Predictors modulated: sex, bill_dep
      Predictors averaged within levels of sex and bill_dep: species, bill_len

---

    Code
      print(estimate_means(model, c("sex", "bill_dep"), estimate = "population"))
    Output
      Marginal Predictions
      
      sex    | bill_dep |    Mean |     SE |             95% CI | t(326)
      ------------------------------------------------------------------
      female |    13.10 | 3514.44 |  81.35 | [3354.40, 3674.48] |  43.20
      male   |    13.10 | 4185.14 | 103.62 | [3981.30, 4388.98] |  40.39
      female |    14.03 | 3627.96 |  61.91 | [3506.16, 3749.76] |  58.60
      male   |    14.03 | 4244.49 |  84.56 | [4078.15, 4410.83] |  50.20
      female |    14.97 | 3741.59 |  43.87 | [3655.28, 3827.91] |  85.28
      male   |    14.97 | 4303.91 |  65.95 | [4174.18, 4433.64] |  65.26
      female |    15.90 | 3855.11 |  29.95 | [3796.18, 3914.04] | 128.70
      male   |    15.90 | 4363.26 |  48.37 | [4268.10, 4458.43] |  90.20
      female |    16.83 | 3968.62 |  27.35 | [3914.83, 4022.42] | 145.13
      male   |    16.83 | 4422.62 |  33.46 | [4356.80, 4488.44] | 132.19
      female |    17.77 | 4082.26 |  38.44 | [4006.64, 4157.89] | 106.19
      male   |    17.77 | 4482.04 |  26.22 | [4430.46, 4533.61] | 170.96
      female |    18.70 | 4195.78 |  55.55 | [4086.50, 4305.05] |  75.53
      male   |    18.70 | 4541.39 |  32.32 | [4477.81, 4604.98] | 140.51
      female |    19.63 | 4309.29 |  74.64 | [4162.45, 4456.14] |  57.73
      male   |    19.63 | 4600.75 |  46.80 | [4508.68, 4692.82] |  98.30
      female |    20.57 | 4422.93 |  94.55 | [4236.92, 4608.94] |  46.78
      male   |    20.57 | 4660.17 |  64.24 | [4533.78, 4786.55] |  72.54
      female |    21.50 | 4536.44 | 114.82 | [4310.56, 4762.32] |  39.51
      male   |    21.50 | 4719.52 |  82.77 | [4556.69, 4882.35] |  57.02
      
      Variable predicted: body_mass
      Predictors modulated: sex, bill_dep
      Predictors averaged over the full sample: species, bill_len

---

    Code
      print(estimate_means(model, "bill_dep", estimate = "specific"))
    Output
      Conditional Predictions
      
      bill_dep |    Mean |     SE |             95% CI | t(327)
      ---------------------------------------------------------
      13.10    | 3163.32 | 116.13 | [2934.86, 3391.79] |  27.24
      14.03    | 3245.36 | 100.24 | [3048.16, 3442.57] |  32.37
      14.97    | 3327.49 |  85.56 | [3159.18, 3495.80] |  38.89
      15.90    | 3409.53 |  72.84 | [3266.23, 3552.83] |  46.81
      16.83    | 3491.57 |  63.28 | [3367.09, 3616.05] |  55.18
      17.77    | 3573.70 |  58.42 | [3458.77, 3688.63] |  61.17
      18.70    | 3655.74 |  59.46 | [3538.78, 3772.71] |  61.49
      19.63    | 3737.79 |  66.10 | [3607.76, 3867.82] |  56.55
      20.57    | 3819.91 |  76.92 | [3668.60, 3971.23] |  49.66
      21.50    | 3901.96 |  90.41 | [3724.09, 4079.82] |  43.16
      
      Variable predicted: body_mass
      Predictors modulated: bill_dep
      Predictors held at: species (Adelie), sex (female), bill_len (44)

---

    Code
      print(estimate_means(model, "bill_dep", estimate = "typical"))
    Output
      Conditional Predictions
      
      bill_dep |    Mean |    SE |             95% CI | t(327)
      --------------------------------------------------------
      13.10    | 3781.32 | 84.54 | [3615.00, 3947.64] |  44.73
      14.03    | 3863.36 | 66.29 | [3732.95, 3993.77] |  58.28
      14.97    | 3945.49 | 48.49 | [3850.10, 4040.88] |  81.37
      15.90    | 4027.53 | 31.97 | [3964.64, 4090.43] | 125.97
      16.83    | 4109.57 | 20.12 | [4069.99, 4149.16] | 204.25
      17.77    | 4191.70 | 22.36 | [4147.72, 4235.68] | 187.50
      18.70    | 4273.75 | 36.15 | [4202.63, 4344.86] | 118.23
      19.63    | 4355.79 | 53.15 | [4251.22, 4460.35] |  81.95
      20.57    | 4437.92 | 71.12 | [4298.00, 4577.83] |  62.40
      21.50    | 4519.96 | 89.45 | [4343.98, 4695.93] |  50.53
      
      Variable predicted: body_mass
      Predictors modulated: bill_dep
      Predictors held at: bill_len (44)
      Predictors averaged over a balanced grid: species, sex

---

    Code
      print(estimate_means(model, "bill_dep", estimate = "average", verbose = FALSE))
    Output
      Marginal Predictions
      
      bill_dep |    Mean |    SE |             95% CI | t(327)
      --------------------------------------------------------
      13.10    | 4577.67 | 42.27 | [4494.52, 4660.83] | 108.30
      15.90    | 4999.08 | 24.99 | [4949.93, 5048.24] | 200.07
      18.70    | 3819.96 | 23.22 | [3774.28, 3865.64] | 164.51
      21.50    | 4392.42 | 63.38 | [4267.73, 4517.11] |  69.30
      
      Variable predicted: body_mass
      Predictors modulated: bill_dep
      Predictors averaged within levels of bill_dep: species, sex, bill_len

---

    Code
      print(estimate_means(model, "bill_dep", estimate = "population"))
    Output
      Marginal Predictions
      
      bill_dep |    Mean |    SE |             95% CI | t(327)
      --------------------------------------------------------
      13.10    | 3849.62 | 83.81 | [3684.74, 4014.51] |  45.93
      14.03    | 3931.66 | 65.42 | [3802.96, 4060.36] |  60.10
      14.97    | 4013.79 | 47.38 | [3920.59, 4107.00] |  84.72
      15.90    | 4095.83 | 30.40 | [4036.03, 4155.63] | 134.74
      16.83    | 4177.88 | 17.75 | [4142.96, 4212.79] | 235.40
      17.77    | 4260.00 | 20.45 | [4219.77, 4300.23] | 208.32
      18.70    | 4342.05 | 35.12 | [4272.96, 4411.14] | 123.64
      19.63    | 4424.09 | 52.54 | [4320.73, 4527.44] |  84.21
      20.57    | 4506.22 | 70.72 | [4367.09, 4645.35] |  63.72
      21.50    | 4588.26 | 89.18 | [4412.82, 4763.70] |  51.45
      
      Variable predicted: body_mass
      Predictors modulated: bill_dep
      Predictors averaged over the full sample: species, sex, bill_len

---

    Code
      print(estimate_means(model, c("sex", "species"), estimate = "specific"))
    Output
      Conditional Predictions
      
      sex    | species   |    Mean |    SE |             95% CI | t(325)
      ------------------------------------------------------------------
      female | Adelie    | 3519.07 | 60.43 | [3400.19, 3637.95] |  58.23
      male   | Adelie    | 3972.37 | 59.59 | [3855.14, 4089.60] |  66.66
      female | Chinstrap | 3415.14 | 53.12 | [3310.63, 3519.64] |  64.29
      male   | Chinstrap | 3546.52 | 76.78 | [3395.48, 3697.56] |  46.19
      female | Gentoo    | 4901.55 | 71.40 | [4761.08, 5042.02] |  68.65
      male   | Gentoo    | 5460.47 | 63.95 | [5334.67, 5586.28] |  85.39
      
      Variable predicted: body_mass
      Predictors modulated: sex, species
      Predictors held at: bill_dep (17), bill_len (44)

---

    Code
      print(estimate_means(model, c("sex", "species"), estimate = "typical"))
    Output
      Conditional Predictions
      
      sex    | species   |    Mean |    SE |             95% CI | t(325)
      ------------------------------------------------------------------
      female | Adelie    | 3519.07 | 60.43 | [3400.19, 3637.95] |  58.23
      male   | Adelie    | 3972.37 | 59.59 | [3855.14, 4089.60] |  66.66
      female | Chinstrap | 3415.14 | 53.12 | [3310.63, 3519.64] |  64.29
      male   | Chinstrap | 3546.52 | 76.78 | [3395.48, 3697.56] |  46.19
      female | Gentoo    | 4901.55 | 71.40 | [4761.08, 5042.02] |  68.65
      male   | Gentoo    | 5460.47 | 63.95 | [5334.67, 5586.28] |  85.39
      
      Variable predicted: body_mass
      Predictors modulated: sex, species
      Predictors held at: bill_dep (17), bill_len (44)

---

    Code
      print(estimate_means(model, c("sex", "species"), estimate = "average"))
    Output
      Marginal Predictions
      
      sex    | species   |    Mean |    SE |             95% CI | t(325)
      ------------------------------------------------------------------
      female | Adelie    | 3368.84 | 33.97 | [3302.00, 3435.67] |  99.17
      male   | Adelie    | 4043.49 | 33.97 | [3976.66, 4110.32] | 119.03
      female | Chinstrap | 3527.21 | 49.78 | [3429.28, 3625.13] |  70.86
      male   | Chinstrap | 3938.97 | 49.78 | [3841.04, 4036.90] |  79.13
      female | Gentoo    | 4679.74 | 38.11 | [4604.77, 4754.72] | 122.79
      male   | Gentoo    | 5484.84 | 37.16 | [5411.73, 5557.95] | 147.59
      
      Variable predicted: body_mass
      Predictors modulated: sex, species
      Predictors averaged within levels of sex and species: bill_dep, bill_len

---

    Code
      print(estimate_means(model, c("sex", "species"), estimate = "population"))
    Output
      Marginal Predictions
      
      sex    | species   |    Mean |    SE |             95% CI | t(325)
      ------------------------------------------------------------------
      female | Adelie    | 3519.07 | 60.43 | [3400.19, 3637.95] |  58.23
      male   | Adelie    | 3972.37 | 59.59 | [3855.14, 4089.60] |  66.66
      female | Chinstrap | 3415.14 | 53.12 | [3310.63, 3519.64] |  64.29
      male   | Chinstrap | 3546.52 | 76.78 | [3395.48, 3697.56] |  46.19
      female | Gentoo    | 4901.55 | 71.40 | [4761.08, 5042.02] |  68.65
      male   | Gentoo    | 5460.47 | 63.95 | [5334.67, 5586.28] |  85.39
      
      Variable predicted: body_mass
      Predictors modulated: sex, species
      Predictors averaged over the full sample: bill_dep, bill_len

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
      Random effects: Averaged over a balanced grid of cluster levels.
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
      Random effects: Conditional on a typical cluster.
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
      Random effects: Marginalized across clusters.
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
      Random effects: Conditional on a typical cluster.
      Predictions are on the response-scale.

---

    Code
      print(estimate_means(m, "sex"))
    Output
      Conditional Predictions
      
      sex    | Probability |       95% CI
      -----------------------------------
      female |        0.23 | [0.12, 0.39]
      male   |        0.66 | [0.42, 0.83]
      
      Variable predicted: bill_dep
      Predictors modulated: sex
      Predictors held at: body_mass (4.2e+03)
      Predictors averaged over a balanced grid: species
      Predictions are on the inverse-link-scale.

---

    Code
      print(estimate_means(m, "sex", estimate = "average"))
    Output
      Marginal Predictions
      
      sex    | Probability |   SE |       95% CI |     z
      --------------------------------------------------
      female |        0.40 | 0.03 | [0.34, 0.46] | 13.90
      male   |        0.63 | 0.01 | [0.61, 0.66] | 49.00
      
      Variable predicted: bill_dep
      Predictors modulated: sex
      Predictors averaged within levels of sex: species, body_mass
      Predictions are on the response-scale.

