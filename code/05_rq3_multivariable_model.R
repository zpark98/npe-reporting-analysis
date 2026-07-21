# 05_rq3_multivariable_model.R
# Purpose: Fit multivariable models for trial characteristics and NPE reporting
# Input: data/processed/trial_analysis_dataset.csv
# Output: results/q3_* CSV files
# Data source: Trial-level analysis dataset
# Note: Odds ratios describe associations with registry-visible reporting.

library(broom)
library(dplyr)
library(readr)

dir.create("results", recursive = TRUE, showWarnings = FALSE)

trial_analysis <- read_csv(
  "data/processed/trial_analysis_dataset.csv",
  show_col_types = FALSE
)

trial_analysis_clean <- trial_analysis %>%
  mutate(
    phase_clean = case_when(
      is.na(phase) ~ "UNKNOWN",
      phase == "NA" ~ "UNKNOWN",
      TRUE ~ phase
    ),
    sponsor_clean = case_when(
      is.na(sponsor_type) ~ "UNKNOWN",
      TRUE ~ sponsor_type
    )
  )

q3_check <- trial_analysis_clean %>%
  summarise(
    n_total = n(),
    missing_phase_original = sum(is.na(phase) | phase == "NA"),
    missing_sponsor_original = sum(is.na(sponsor_type)),
    missing_year = sum(is.na(primary_completion_year)),
    missing_sample_size = sum(is.na(sample_size)),
    non_positive_sample_size = sum(sample_size <= 0, na.rm = TRUE),
    sample_size_min = min(sample_size, na.rm = TRUE),
    sample_size_median = median(sample_size, na.rm = TRUE),
    sample_size_max = max(sample_size, na.rm = TRUE),
    .groups = "drop"
  )

q3_df_ref <- trial_analysis_clean %>%
  filter(
    !is.na(primary_completion_year),
    !is.na(sample_size),
    sample_size > 0
  ) %>%
  mutate(
    year_c = primary_completion_year - median(primary_completion_year, na.rm = TRUE),
    log2_trial_enrolment = log2(sample_size),
    phase_clean = relevel(factor(phase_clean), ref = "PHASE2"),
    sponsor_clean = relevel(factor(sponsor_clean), ref = "INDUSTRY")
  )

model_q3_ref <- glm(
  npe_any ~ year_c + phase_clean + log2_trial_enrolment + sponsor_clean,
  family = binomial,
  data = q3_df_ref
)

q3_result_ref <- broom::tidy(
  model_q3_ref,
  conf.int = TRUE,
  exponentiate = TRUE
)

q3_result_readable <- q3_result_ref %>%
  mutate(
    predictor = case_when(
      term == "year_c" ~ "Primary completion year",
      term == "log2_trial_enrolment" ~ "Trial enrolment",
      grepl("^phase_clean", term) ~ "Phase",
      grepl("^sponsor_clean", term) ~ "Sponsor type",
      TRUE ~ "Intercept"
    ),
    category_contrast = recode(
      term,
      "(Intercept)" = "Intercept",
      "year_c" = "Per year",
      "log2_trial_enrolment" = "Per doubling of trial enrolment",
      "phase_cleanEARLY_PHASE1" = "Early phase 1 vs Phase 2",
      "phase_cleanPHASE1" = "Phase 1 vs Phase 2",
      "phase_cleanPHASE1/PHASE2" = "Phase 1/Phase 2 vs Phase 2",
      "phase_cleanPHASE2/PHASE3" = "Phase 2/Phase 3 vs Phase 2",
      "phase_cleanPHASE3" = "Phase 3 vs Phase 2",
      "phase_cleanPHASE4" = "Phase 4 vs Phase 2",
      "phase_cleanUNKNOWN" = "Unknown vs Phase 2",
      "sponsor_cleanFED" = "Federal vs Industry",
      "sponsor_cleanINDIV" = "Individual vs Industry",
      "sponsor_cleanNETWORK" = "Network vs Industry",
      "sponsor_cleanNIH" = "NIH vs Industry",
      "sponsor_cleanOTHER" = "Other vs Industry",
      "sponsor_cleanOTHER_GOV" = "Other government vs Industry",
      "sponsor_cleanUNKNOWN" = "Unknown vs Industry"
    ),
    adjusted_or_95_ci = paste0(
      sprintf("%.2f", estimate),
      " (",
      sprintf("%.2f", conf.low),
      "-",
      sprintf("%.2f", conf.high),
      ")"
    ),
    p_value_formatted = case_when(
      p.value < 0.001 ~ "<0.001",
      p.value < 0.01 ~ sprintf("%.3f", p.value),
      TRUE ~ sprintf("%.2f", p.value)
    )
  ) %>%
  select(
    predictor,
    category_contrast,
    adjusted_or_95_ci,
    p_value_formatted,
    term,
    estimate,
    conf.low,
    conf.high,
    p.value
  )

phase_reference_row <- tibble::tibble(
  predictor = "Phase",
  category_contrast = "Phase 2 (reference)",
  adjusted_or_95_ci = "1.00",
  p_value_formatted = "Reference",
  term = "phase_cleanPHASE2_reference",
  estimate = NA_real_,
  conf.low = NA_real_,
  conf.high = NA_real_,
  p.value = NA_real_
)

sponsor_reference_row <- tibble::tibble(
  predictor = "Sponsor type",
  category_contrast = "Industry (reference)",
  adjusted_or_95_ci = "1.00",
  p_value_formatted = "Reference",
  term = "sponsor_cleanINDUSTRY_reference",
  estimate = NA_real_,
  conf.low = NA_real_,
  conf.high = NA_real_,
  p.value = NA_real_
)

q3_table4_dissertation <- bind_rows(
  q3_result_readable %>% filter(term == "year_c"),
  q3_result_readable %>% filter(term == "log2_trial_enrolment"),
  phase_reference_row,
  q3_result_readable %>% filter(grepl("^phase_clean", term)),
  sponsor_reference_row,
  q3_result_readable %>% filter(grepl("^sponsor_clean", term))
) %>%
  select(predictor, category_contrast, adjusted_or_95_ci, p_value_formatted)

q3_phase_table <- trial_analysis_clean %>%
  group_by(phase_clean) %>%
  summarise(
    total_trials = n(),
    npe_reporting_trials = sum(npe_any == 1),
    non_npe_trials = sum(npe_any == 0),
    npe_reporting_percent = npe_reporting_trials / total_trials * 100,
    non_npe_percent = non_npe_trials / total_trials * 100,
    .groups = "drop"
  )

q3_sponsor_table <- trial_analysis_clean %>%
  group_by(sponsor_clean) %>%
  summarise(
    total_trials = n(),
    npe_reporting_trials = sum(npe_any == 1),
    non_npe_trials = sum(npe_any == 0),
    npe_reporting_percent = npe_reporting_trials / total_trials * 100,
    non_npe_percent = non_npe_trials / total_trials * 100,
    .groups = "drop"
  )

q3_continuous_table <- trial_analysis_clean %>%
  mutate(
    npe_group = if_else(npe_any == 1, "NPE-reporting trials", "Non-NPE trials")
  ) %>%
  group_by(npe_group) %>%
  summarise(
    n_trials = n(),
    year_median = median(primary_completion_year, na.rm = TRUE),
    year_q1 = as.numeric(quantile(primary_completion_year, 0.25, na.rm = TRUE)),
    year_q3 = as.numeric(quantile(primary_completion_year, 0.75, na.rm = TRUE)),
    year_missing = sum(is.na(primary_completion_year)),
    enrolment_median = median(sample_size, na.rm = TRUE),
    enrolment_q1 = as.numeric(quantile(sample_size, 0.25, na.rm = TRUE)),
    enrolment_q3 = as.numeric(quantile(sample_size, 0.75, na.rm = TRUE)),
    enrolment_missing = sum(is.na(sample_size)),
    .groups = "drop"
  )

q3_overall_continuous <- trial_analysis_clean %>%
  summarise(
    n_trials = n(),
    year_median = median(primary_completion_year, na.rm = TRUE),
    year_q1 = as.numeric(quantile(primary_completion_year, 0.25, na.rm = TRUE)),
    year_q3 = as.numeric(quantile(primary_completion_year, 0.75, na.rm = TRUE)),
    year_missing = sum(is.na(primary_completion_year)),
    enrolment_median = median(sample_size, na.rm = TRUE),
    enrolment_q1 = as.numeric(quantile(sample_size, 0.25, na.rm = TRUE)),
    enrolment_q3 = as.numeric(quantile(sample_size, 0.75, na.rm = TRUE)),
    enrolment_missing = sum(is.na(sample_size)),
    .groups = "drop"
  )

phase_chisq <- suppressWarnings(
  chisq.test(table(trial_analysis_clean$phase_clean, trial_analysis_clean$npe_any))
)

sponsor_chisq <- suppressWarnings(
  chisq.test(table(trial_analysis_clean$sponsor_clean, trial_analysis_clean$npe_any))
)

q3_chisq_tests <- tibble::tibble(
  test = c("Phase by NPE reporting", "Sponsor type by NPE reporting"),
  statistic = c(unname(phase_chisq$statistic), unname(sponsor_chisq$statistic)),
  df = c(unname(phase_chisq$parameter), unname(sponsor_chisq$parameter)),
  p_value = c(phase_chisq$p.value, sponsor_chisq$p.value)
)

q3_model_sample_check <- tibble::tibble(
  eligible_denominator = nrow(trial_analysis_clean),
  model_sample = nrow(q3_df_ref),
  excluded_missing_primary_completion_year = sum(is.na(trial_analysis_clean$primary_completion_year)),
  missing_phase_recoded_unknown = sum(trial_analysis_clean$phase_clean == "UNKNOWN"),
  missing_sponsor_recoded_unknown = sum(trial_analysis_clean$sponsor_clean == "UNKNOWN"),
  non_positive_enrolment = sum(trial_analysis_clean$sample_size <= 0, na.rm = TRUE)
)

write_csv(q3_check, "results/q3_data_check.csv")
write_csv(q3_result_ref, "results/q3_multivariable_model_phase2_industry_ref.csv")
write_csv(q3_result_readable, "results/q3_multivariable_model_readable.csv")
write_csv(q3_table4_dissertation, "results/q3_table4_main_model_for_dissertation.csv")
write_csv(q3_phase_table, "results/q3_phase_by_npe_status_row_percent.csv")
write_csv(q3_sponsor_table, "results/q3_sponsor_by_npe_status_row_percent.csv")
write_csv(q3_continuous_table, "results/q3_year_enrolment_by_npe_status.csv")
write_csv(q3_overall_continuous, "results/q3_overall_year_enrolment_summary.csv")
write_csv(q3_chisq_tests, "results/q3_chisq_tests_phase_sponsor.csv")
write_csv(q3_model_sample_check, "results/q3_model_sample_check.csv")
