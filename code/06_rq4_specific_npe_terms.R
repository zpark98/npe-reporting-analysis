# 06_rq4_specific_npe_terms.R
# Purpose: Summarise specific NPE terms reported in trial registries
# Input: data/processed/clean_npe_event_records.csv
# Output: results/q4_* CSV files
# Data source: Cleaned NPE adverse event records
# Note: Participant proportions are descriptive registry summaries.

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

q4_participant_level <- df_clean2 %>%
  group_by(term_lower) %>%
  summarise(
    total_subjects_affected = sum(subjects_affected, na.rm = TRUE),
    total_subjects_at_risk = sum(subjects_at_risk, na.rm = TRUE),
    participant_proportion = total_subjects_affected / total_subjects_at_risk,
    participant_percent = participant_proportion * 100,
    n_records = n(),
    n_trials = n_distinct(nct_id),
    .groups = "drop"
  ) %>%
  arrange(desc(n_trials))

q4_top10_participant_level <- q4_participant_level %>%
  slice_head(n = 10)

write_csv(
  q4_participant_level,
  "results/q4_participant_level_incidence_all_terms.csv"
)

write_csv(
  q4_top10_participant_level,
  "results/q4_top10_participant_level_incidence.csv"
)
