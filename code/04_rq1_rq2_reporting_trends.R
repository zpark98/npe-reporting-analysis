# 04_rq1_rq2_reporting_trends.R
# Purpose: Estimate overall NPE reporting and reporting trends over time
# Input: data/processed/trial_analysis_dataset.csv
# Output: results/q1_* and results/q2_* CSV files
# Data source: Trial-level analysis dataset
# Note: These analyses describe registry reporting, not true incidence.

library(broom)
library(dplyr)
library(readr)

dir.create("results", recursive = TRUE, showWarnings = FALSE)

trial_analysis <- read_csv(
  "data/processed/trial_analysis_dataset.csv",
  show_col_types = FALSE
)

model_q1 <- glm(
  npe_any ~ 1,
  family = binomial,
  data = trial_analysis
)

q1_logit <- broom::tidy(model_q1, conf.int = TRUE)

q1_result <- q1_logit %>%
  mutate(
    reporting_rate = plogis(estimate),
    ci_low = plogis(conf.low),
    ci_high = plogis(conf.high)
  )

q1_summary <- trial_analysis %>%
  summarise(
    total_trials = n(),
    npe_reporting_trials = sum(npe_any == 1),
    non_npe_trials = sum(npe_any == 0),
    reporting_rate = mean(npe_any),
    .groups = "drop"
  )

q1_result_readable <- q1_result %>%
  select(term, reporting_rate, ci_low, ci_high, p.value) %>%
  mutate(
    reporting_rate_percent = reporting_rate * 100,
    ci_low_percent = ci_low * 100,
    ci_high_percent = ci_high * 100
  )

year_check <- trial_analysis %>%
  count(primary_completion_year, sort = FALSE)

q2_df <- trial_analysis %>%
  filter(!is.na(primary_completion_year)) %>%
  mutate(
    year_c = primary_completion_year - median(primary_completion_year, na.rm = TRUE)
  )

model_q2 <- glm(
  npe_any ~ year_c,
  family = binomial,
  data = q2_df
)

q2_result <- broom::tidy(
  model_q2,
  conf.int = TRUE,
  exponentiate = TRUE
)

q2_year_summary <- trial_analysis %>%
  filter(!is.na(primary_completion_year)) %>%
  group_by(primary_completion_year) %>%
  summarise(
    total_trials = n(),
    npe_reporting_trials = sum(npe_any == 1),
    reporting_rate = mean(npe_any),
    .groups = "drop"
  )

q2_df_restricted <- trial_analysis %>%
  filter(
    !is.na(primary_completion_year),
    primary_completion_year >= 2008,
    primary_completion_year <= 2024
  ) %>%
  mutate(
    year_c = primary_completion_year - median(primary_completion_year, na.rm = TRUE)
  )

model_q2_restricted <- glm(
  npe_any ~ year_c,
  family = binomial,
  data = q2_df_restricted
)

q2_restricted_result <- broom::tidy(
  model_q2_restricted,
  conf.int = TRUE,
  exponentiate = TRUE
)

write_csv(q1_summary, "results/q1_overall_reporting_summary.csv")
write_csv(q1_result, "results/q1_intercept_model.csv")
write_csv(q1_result_readable, "results/q1_overall_reporting_rate_readable.csv")
write_csv(year_check, "results/year_check.csv")
write_csv(q2_result, "results/q2_year_trend_model.csv")
write_csv(q2_year_summary, "results/q2_year_reporting_summary.csv")
write_csv(q2_restricted_result, "results/q2_year_trend_model_2008_2024.csv")
