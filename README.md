# Neuropsychiatric adverse event reporting in ClinicalTrials.gov

This repository contains R scripts for a dissertation analysis of registry-visible neuropsychiatric adverse event (NPE) reporting in interventional trials registered in ClinicalTrials.gov / AACT.

The project focuses on reporting patterns in submitted registry adverse event results: which eligible trials report NPE terms, how reporting varies over time, which trial characteristics are associated with NPE reporting, and which NPE terms are most frequently reported. The analyses do not estimate true adverse event incidence and do not make causal claims about interventions causing neuropsychiatric events.

## Data source

The scripts use AACT, the publicly available relational database for ClinicalTrials.gov. Database credentials are not stored in the code. They should be set locally as environment variables:

```text
AACT_DBNAME
AACT_HOST
AACT_PORT
AACT_USER
AACT_PASSWORD
```

Raw AACT tables and processed analytical datasets are not included in this repository. Scripts are designed to be run against AACT using local database credentials and local working data directories.

The curated operational NPE term list expected by the first script is:

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

## Analysis outputs

The scripts create local files under `data/processed/` and `results/`. These outputs are intentionally excluded from version control because they may contain derived trial-level or adverse-event-level records from AACT.

Main outputs include:

- eligible trial denominator and trial-level NPE reporting indicator;
- RQ1 overall registry-visible NPE reporting proportion;
- RQ2 temporal logistic regression and exploratory GAM sensitivity check;
- RQ3 multivariable logistic regression using primary completion year, trial phase, log2-transformed trial enrolment, and sponsor type;
- descriptive chi-squared checks for NPE reporting by phase and sponsor type;
- RQ4 most frequently reported NPE terms and reported participant-level proportions.

## Privacy and reproducibility

Do not upload `.Rhistory`, `.RData`, `.Renviron`, raw data, processed data, results, drafts, or private files. The `.gitignore` file is set up to exclude these by default.

The repository is intended to document the reproducible analysis workflow rather than to distribute AACT-derived datasets.