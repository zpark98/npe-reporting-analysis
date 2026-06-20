# Neuropsychiatric adverse event reporting in ClinicalTrials.gov

This repository contains reproducible R scripts for a dissertation analysis of neuropsychiatric adverse event (NPE) reporting in interventional trials registered in ClinicalTrials.gov / AACT.

The project focuses on registry reporting patterns: which trials report NPE terms, how reporting varies over time, and which trial characteristics are associated with NPE reporting. The analyses do not estimate true adverse event incidence and do not make causal claims about interventions causing neuropsychiatric events.

## Data source

The scripts use AACT, the publicly available relational database for ClinicalTrials.gov. Database credentials are not stored in the code. They should be set locally as environment variables:

```text
AACT_DBNAME
AACT_HOST
AACT_PORT
AACT_USER
AACT_PASSWORD
```

The curated term list expected by the first script is:

```text
data/input/definitive_neuro_terms.csv
```

It must contain a column named:

```text
adverse_event_term
```

## Script order

Run the scripts from the project root in this order:

```text
code/01_clean_npe_terms.R
code/02_build_trial_denominator.R
code/03_create_trial_level_dataset.R
code/04_rq1_rq2_reporting_trends.R
code/05_rq3_multivariable_model.R
code/06_rq4_specific_npe_terms.R
code/99_session_info.R
```

## Privacy

Do not upload `.Rhistory`, `.RData`, `.Renviron`, raw data, processed data, results, drafts, or private files. The `.gitignore` file is set up to exclude these by default.
