# 06_rq4_specific_npe_terms.R
# Purpose: Summarise specific NPE terms reported in trial registries
# Input: data/processed/clean_npe_event_records.csv
# Output: results/q4_* CSV files
# Data source: Cleaned NPE adverse event records
# Note: Participant proportions are descriptive registry-reported proportions, not incidence estimates.

library(dplyr)
library(readr)

dir.create("results", recursive = TRUE, showWarnings = FALSE)

df_clean2 <- read_csv(
  "data/processed/clean_npe_event_records.csv",
  show_col_types = FALSE
)

required_columns <- c(
  "nct_id",
  "term_lower",
  "subjects_affected",
  "subjects_at_risk"
)

missing_columns <- setdiff(required_columns, names(df_clean2))
if (length(missing_columns) > 0) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}

npe_reporting_trial_denominator <- n_distinct(df_clean2$nct_id)

q4_reported_participant_proportions <- df_clean2 %>%
  group_by(term_lower) %>%
  summarise(
    total_subjects_affected = sum(subjects_affected, na.rm = TRUE),
    total_subjects_at_risk = sum(subjects_at_risk, na.rm = TRUE),
    reported_participant_proportion = total_subjects_affected / total_subjects_at_risk,
    reported_participant_percent = reported_participant_proportion * 100,
    n_cleaned_ae_records = n(),
    n_trials = n_distinct(nct_id),
    .groups = "drop"
  ) %>%
  mutate(
    percent_of_npe_reporting_trials = n_trials / npe_reporting_trial_denominator * 100
  ) %>%
  arrange(desc(n_trials), desc(n_cleaned_ae_records))

q4_top10_reported_participant_proportions <- q4_reported_participant_proportions %>%
  slice_head(n = 10)

write_csv(
  q4_reported_participant_proportions,
  "results/q4_reported_participant_proportions_all_terms.csv"
)

write_csv(
  q4_top10_reported_participant_proportions,
  "results/q4_top10_reported_participant_proportions.csv"
)
