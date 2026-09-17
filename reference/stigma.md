# Sample dataset from patients with somatic symptons

An artificial, anonymized data set based on a project related to the
development and persistence of somatic symptoms.

The data consists of nine variables from 1140 observations (380 patients
at three time points):

- `ID`: A unique identifier for each participant

- `disease_group`: Classification of main disease from patient, e.g.
  chronic kidney disease (CKD), Ulcerative colitis, Irritable Bowel
  Syndrome (IBS) and others.

- `phq15`: Total score of the PHQ-15 questionnaire (Patient Health
  Questionnaire-15), measuring the somatic symptoms severity

- `symptoms_unreal`: Question regarding perceived stigma related to
  somatic symptoms, asking "People believe that my symptoms are not a
  real illness". Answer categories ranged from `"strongly disagree"`,
  `"disagree"`, `"neutral"`, `"agree"`, and `"strongly agree"`.
  `"I have no complaints"` was the residual category for patients
  without specific somatic symptoms.

- `time3`: Measurement point (baseline, and two follow-ups)

- `education`: Educational level, measured according to the CASMIN
  classification.

- `sex`: The participant's sex

- `age_z`: Age of patient, standardized
