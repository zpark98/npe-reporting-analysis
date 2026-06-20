# 05_rq3_multivariable_model.R
# Purpose: Fit multivariable models for trial characteristics and NPE reporting
# Input: data/processed/trial_analysis_dataset.csv and optional AACT intervention data
# Output: results/q3_* CSV files
# Data source: Trial-level analysis dataset
# Note: Odds ratios describe associations with registry reporting.

library(broom)
library(dplyr)
library(readr)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("results", recursive = TRUE, showWarnings = FALSE)

trial_analysis <- read_csv(
  "data/processed/trial_analysis_dataset.csv",
  show_col_types = FALSE
)

q3_check <- trial_analysis %>%
  summarise(
    n_total = n(),
    missing_phase = sum(is.na(phase)),
    missing_year = sum(is.na(primary_completion_year)),
    missing_sample_size = sum(is.na(sample_size)),
    sample_size_min = min(sample_size, na.rm = TRUE),
    sample_size_median = median(sample_size, na.rm = TRUE),
    sample_size_max = max(sample_size, na.rm = TRUE),
    .groups = "drop"
  )

q3_df <- trial_analysis %>%
  filter(
    !is.na(phase),
    !is.na(primary_completion_year),
    !is.na(sample_size),
    sample_size > 0
  ) %>%
  mutate(
    year_c = primary_completion_year - median(primary_completion_year, na.rm = TRUE),
    log_sample_size = log(sample_size),
    phase = factor(phase),
    sponsor_type = factor(sponsor_type)
  )

model_q3 <- glm(
  npe_any ~ year_c + phase + log_sample_size + sponsor_type,
  family = binomial,
  data = q3_df
)

q3_result <- broom::tidy(
  model_q3,
  conf.int = TRUE,
  exponentiate = TRUE
)

q3_df_ref <- trial_analysis %>%
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
  ) %>%
  filter(
    !is.na(primary_completion_year),
    !is.na(sample_size),
    sample_size > 0
  ) %>%
  mutate(
    year_c = primary_completion_year - median(primary_completion_year, na.rm = TRUE),
    log_sample_size = log(sample_size),
    phase_clean = relevel(factor(phase_clean), ref = "PHASE2"),
    sponsor_clean = relevel(factor(sponsor_clean), ref = "INDUSTRY")
  )

model_q3_ref <- glm(
  npe_any ~ year_c + phase_clean + log_sample_size + sponsor_clean,
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
    term_label = recode(
      term,
      "(Intercept)" = "Intercept",
      "year_c" = "Primary completion year, per year",
      "phase_cleanEARLY_PHASE1" = "Early Phase 1 vs Phase 2",
      "phase_cleanPHASE1" = "Phase 1 vs Phase 2",
      "phase_cleanPHASE1/PHASE2" = "Phase 1/Phase 2 vs Phase 2",
      "phase_cleanPHASE2/PHASE3" = "Phase 2/Phase 3 vs Phase 2",
      "phase_cleanPHASE3" = "Phase 3 vs Phase 2",
      "phase_cleanPHASE4" = "Phase 4 vs Phase 2",
      "phase_cleanUNKNOWN" = "Unknown phase vs Phase 2",
      "log_sample_size" = "Log sample size",
      "sponsor_cleanFED" = "FED vs Industry",
      "sponsor_cleanINDIV" = "Individual vs Industry",
      "sponsor_cleanNETWORK" = "Network vs Industry",
      "sponsor_cleanNIH" = "NIH vs Industry",
      "sponsor_cleanOTHER" = "Other vs Industry",
      "sponsor_cleanOTHER_GOV" = "Other government vs Industry",
      "sponsor_cleanUNKNOWN" = "Unknown sponsor vs Industry"
    )
  ) %>%
  select(term_label, estimate, conf.low, conf.high, p.value)

q3_phase_table <- trial_analysis %>%
  mutate(
    npe_group = if_else(npe_any == 1, "NPE-reporting trials", "Non-NPE trials"),
    phase_clean = case_when(
      is.na(phase) ~ "UNKNOWN",
      phase == "NA" ~ "UNKNOWN",
      TRUE ~ phase
    )
  ) %>%
  count(npe_group, phase_clean) %>%
  group_by(npe_group) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

q3_sponsor_table <- trial_analysis %>%
  mutate(
    npe_group = if_else(npe_any == 1, "NPE-reporting trials", "Non-NPE trials"),
    sponsor_clean = case_when(
      is.na(sponsor_type) ~ "UNKNOWN",
      TRUE ~ sponsor_type
    )
  ) %>%
  count(npe_group, sponsor_clean) %>%
  group_by(npe_group) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

q3_continuous_table <- trial_analysis %>%
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
    sample_size_median = median(sample_size, na.rm = TRUE),
    sample_size_q1 = as.numeric(quantile(sample_size, 0.25, na.rm = TRUE)),
    sample_size_q3 = as.numeric(quantile(sample_size, 0.75, na.rm = TRUE)),
    sample_size_missing = sum(is.na(sample_size)),
    .groups = "drop"
  )

q3_table1_overview <- trial_analysis %>%
  mutate(
    npe_group = if_else(npe_any == 1, "NPE-reporting trials", "Non-NPE trials")
  ) %>%
  group_by(npe_group) %>%
  summarise(
    n_trials = n(),
    percent_of_total = n() / nrow(trial_analysis) * 100,
    median_year = median(primary_completion_year, na.rm = TRUE),
    year_iqr = paste0(
      as.numeric(quantile(primary_completion_year, 0.25, na.rm = TRUE)),
      " to ",
      as.numeric(quantile(primary_completion_year, 0.75, na.rm = TRUE))
    ),
    median_sample_size = median(sample_size, na.rm = TRUE),
    sample_size_iqr = paste0(
      as.numeric(quantile(sample_size, 0.25, na.rm = TRUE)),
      " to ",
      as.numeric(quantile(sample_size, 0.75, na.rm = TRUE))
    ),
    .groups = "drop"
  )

write_csv(q3_check, "results/q3_data_check.csv")
write_csv(q3_result, "results/q3_multivariable_model.csv")
write_csv(q3_result_ref, "results/q3_multivariable_model_phase2_industry_ref.csv")
write_csv(q3_result_readable, "results/q3_multivariable_model_readable.csv")
write_csv(q3_phase_table, "results/q3_table1_phase_by_npe_status.csv")
write_csv(q3_sponsor_table, "results/q3_table1_sponsor_by_npe_status.csv")
write_csv(q3_continuous_table, "results/q3_table1_year_sample_size_by_npe_status.csv")
write_csv(q3_table1_overview, "results/q3_table1_overview.csv")

# Optional extension from the History: add main intervention type to the model.
# Change this to TRUE only if this analysis is needed and AACT credentials exist.
run_intervention_extension <- FALSE

if (run_intervention_extension) {
  required_env <- c(
    "AACT_DBNAME",
    "AACT_HOST",
    "AACT_PORT",
    "AACT_USER",
    "AACT_PASSWORD"
  )

  missing_env <- required_env[!nzchar(Sys.getenv(required_env))]
  if (length(missing_env) > 0) {
    stop("Missing required environment variables: ", paste(missing_env, collapse = ", "))
  }

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("AACT_DBNAME"),
    host = Sys.getenv("AACT_HOST"),
    port = as.integer(Sys.getenv("AACT_PORT")),
    user = Sys.getenv("AACT_USER"),
    password = Sys.getenv("AACT_PASSWORD")
  )

  denom_ncts <- trial_analysis %>%
    select(nct_id) %>%
    distinct()

  DBI::dbWriteTable(
    con,
    "tmp_denominator_ncts",
    denom_ncts,
    temporary = TRUE,
    overwrite = TRUE
  )

  intervention_type_check <- DBI::dbGetQuery(con, "
  SELECT
    i.intervention_type,
    COUNT(DISTINCT i.nct_id) AS n_trials
  FROM interventions i
  JOIN tmp_denominator_ncts d
    ON i.nct_id = d.nct_id
  GROUP BY i.intervention_type
  ORDER BY n_trials DESC;
  ")

  intervention_df <- DBI::dbGetQuery(con, "
  SELECT
    d.nct_id,
    CASE
      WHEN SUM(CASE WHEN i.intervention_type = 'DRUG' THEN 1 ELSE 0 END) > 0 THEN 'DRUG'
      WHEN SUM(CASE WHEN i.intervention_type = 'BIOLOGICAL' THEN 1 ELSE 0 END) > 0 THEN 'BIOLOGICAL'
      WHEN SUM(CASE WHEN i.intervention_type = 'DEVICE' THEN 1 ELSE 0 END) > 0 THEN 'DEVICE'
      WHEN SUM(CASE WHEN i.intervention_type = 'PROCEDURE' THEN 1 ELSE 0 END) > 0 THEN 'PROCEDURE'
      WHEN SUM(CASE WHEN i.intervention_type = 'BEHAVIORAL' THEN 1 ELSE 0 END) > 0 THEN 'BEHAVIORAL'
      WHEN SUM(CASE WHEN i.intervention_type = 'RADIATION' THEN 1 ELSE 0 END) > 0 THEN 'RADIATION'
      WHEN SUM(CASE WHEN i.intervention_type = 'DIETARY_SUPPLEMENT' THEN 1 ELSE 0 END) > 0 THEN 'DIETARY_SUPPLEMENT'
      WHEN SUM(CASE WHEN i.intervention_type = 'COMBINATION_PRODUCT' THEN 1 ELSE 0 END) > 0 THEN 'COMBINATION_PRODUCT'
      WHEN SUM(CASE WHEN i.intervention_type = 'GENETIC' THEN 1 ELSE 0 END) > 0 THEN 'GENETIC'
      WHEN SUM(CASE WHEN i.intervention_type = 'DIAGNOSTIC_TEST' THEN 1 ELSE 0 END) > 0 THEN 'DIAGNOSTIC_TEST'
      WHEN SUM(CASE WHEN i.intervention_type = 'OTHER' THEN 1 ELSE 0 END) > 0 THEN 'OTHER'
      ELSE 'UNKNOWN'
    END AS main_intervention_type,
    STRING_AGG(DISTINCT i.intervention_type, '; ' ORDER BY i.intervention_type) AS intervention_types,
    COUNT(DISTINCT i.intervention_type) AS n_intervention_types
  FROM tmp_denominator_ncts d
  LEFT JOIN interventions i
    ON d.nct_id = i.nct_id
  GROUP BY d.nct_id;
  ")

  trial_analysis_q3_extra <- trial_analysis %>%
    left_join(intervention_df, by = "nct_id")

  q3_df_with_intervention <- trial_analysis_q3_extra %>%
    mutate(
      phase_clean = case_when(
        is.na(phase) ~ "UNKNOWN",
        phase == "NA" ~ "UNKNOWN",
        TRUE ~ phase
      ),
      sponsor_clean = case_when(
        is.na(sponsor_type) ~ "UNKNOWN",
        TRUE ~ sponsor_type
      ),
      intervention_clean = case_when(
        is.na(main_intervention_type) ~ "UNKNOWN",
        TRUE ~ main_intervention_type
      )
    ) %>%
    filter(
      !is.na(primary_completion_year),
      !is.na(sample_size),
      sample_size > 0
    ) %>%
    mutate(
      year_c = primary_completion_year - median(primary_completion_year, na.rm = TRUE),
      log_sample_size = log(sample_size),
      phase_clean = relevel(factor(phase_clean), ref = "PHASE2"),
      sponsor_clean = relevel(factor(sponsor_clean), ref = "INDUSTRY"),
      intervention_clean = relevel(factor(intervention_clean), ref = "DRUG")
    )

  model_q3_with_intervention <- glm(
    npe_any ~ year_c + phase_clean + log_sample_size + sponsor_clean + intervention_clean,
    family = binomial,
    data = q3_df_with_intervention
  )

  q3_with_intervention_result <- broom::tidy(
    model_q3_with_intervention,
    conf.int = TRUE,
    exponentiate = TRUE
  )

  write_csv(intervention_type_check, "results/q3_intervention_type_check.csv")
  write_csv(
    trial_analysis_q3_extra,
    "data/processed/trial_analysis_dataset_with_intervention_type.csv"
  )
  write_csv(
    q3_with_intervention_result,
    "results/q3_multivariable_model_with_intervention_type.csv"
  )

  try(DBI::dbDisconnect(con), silent = TRUE)
}
