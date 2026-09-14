# Sample dataset from the SOMACROSS project

A sample data set from the SOMACROSS project (Development and
Persistence of Somatic Symptoms). This is a random, anonymized subset of
the original project data set.

The data consists of nine variables from 1140 observations (380 patients
at three time points):

- `patid`: A unique identifier for each participant

- `disease_group`: Classification of main disease from patient, e.g.
  primary Sclerosing Cholangitis, chronic kidney disease (CKD),
  Ulcerative colitis, Irritable Bowel Syndrome (IBS) and others.

- `phq15`: Total score of the PHQ-15 questionnaire (Patient Health
  Questionnaire-15), measuring the somatic symptoms severity

- `stigma_unreal`: Question regarding perceived stigma related to
  somatic symptoms, asking "Most people believe that my symptoms are not
  a real illness". Answer categories ranged from `"strongly disagree"`,
  `"disagree"`, `"neutral"`, `"agree"`, and `"strongly agree"`.
  `"I have no complaints"` was the residual category for patients
  without specific somatic symptoms.

- `time3`: Measurement point (baseline, and two follow-ups)

- `education_casmin`: Educational level, measured according to the
  CASMIN classification.

- `sex`: The participant's sex

- `migration_history`: History of migration from patients

- `age_z`: Age of patient, standardized
