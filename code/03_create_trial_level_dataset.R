# 03_create_trial_level_dataset.R
# Purpose: Create the trial-level analysis dataset
# Input: data/processed/all_eligible_trials_denominator.csv and data/processed/npe_trial_summary.csv
# Output: data/processed/trial_analysis_dataset.csv
# Data source: Processed denominator and NPE trial summary files
# Note: NPE reporting is coded at the trial level.

library(dplyr)
library(readr)
library(tidyr)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("results", recursive = TRUE, showWarnings = FALSE)

all_trials_df <- read_csv(
  "data/processed/all_eligible_trials_denominator.csv",
  show_col_types = FALSE
)

npe_trial_summary <- read_csv(
  "data/processed/npe_trial_summary.csv",
  show_col_types = FALSE
)

npe_flag <- npe_trial_summary %>%
  select(nct_id, npe_any, npe_record_count, npe_term_count)

trial_analysis <- all_trials_df %>%
  distinct(nct_id, .keep_all = TRUE) %>%
  left_join(npe_flag, by = "nct_id") %>%
  mutate(
    npe_any = if_else(is.na(npe_any), 0L, 1L),
    npe_record_count = replace_na(npe_record_count, 0),
    npe_term_count = replace_na(npe_term_count, 0)
  )

unmatched_npe_trials <- anti_join(
  npe_trial_summary,
  all_trials_df,
  by = "nct_id"
)

trial_analysis_check <- trial_analysis %>%
  summarise(
    total_trials = n(),
    unique_trials = n_distinct(nct_id),
    npe_reporting_trials = sum(npe_any == 1),
    non_npe_trials = sum(npe_any == 0),
    reporting_rate = mean(npe_any),
    .groups = "drop"
  )

write_csv(
  trial_analysis,
  "data/processed/trial_analysis_dataset.csv"
)

write_csv(
  unmatched_npe_trials,
  "results/unmatched_npe_trials.csv"
)

write_csv(
  trial_analysis_check,
  "results/trial_analysis_check.csv"
)
