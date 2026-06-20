# 02_build_trial_denominator.R
# Purpose: Build the denominator of eligible interventional trials
# Input: AACT studies, sponsors, and reported_events tables
# Output: data/processed/all_eligible_trials_denominator.csv
# Data source: AACT / ClinicalTrials.gov study and adverse event tables
# Note: Database credentials are read from environment variables.

library(DBI)
library(dplyr)
library(readr)
library(RPostgres)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("results", recursive = TRUE, showWarnings = FALSE)

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

all_trials_df <- DBI::dbGetQuery(con, "
WITH ae_trials AS (
  SELECT DISTINCT nct_id
  FROM reported_events
  WHERE nct_id IS NOT NULL
),
lead_sponsor AS (
  SELECT
    nct_id,
    MAX(CASE
      WHEN LOWER(lead_or_collaborator) = 'lead'
      THEN agency_class
    END) AS sponsor_type
  FROM sponsors
  GROUP BY nct_id
)
SELECT
  s.nct_id,
  s.phase,
  s.study_type,
  s.overall_status,
  COALESCE(ls.sponsor_type, 'UNKNOWN') AS sponsor_type,
  EXTRACT(YEAR FROM s.primary_completion_date)::int AS primary_completion_year,
  s.enrollment AS sample_size
FROM studies s
JOIN ae_trials a
  ON s.nct_id = a.nct_id
LEFT JOIN lead_sponsor ls
  ON s.nct_id = ls.nct_id
WHERE UPPER(s.study_type) = 'INTERVENTIONAL';
")

denominator_check <- all_trials_df %>%
  summarise(
    n_rows = n(),
    n_trials = n_distinct(nct_id),
    missing_phase = sum(is.na(phase)),
    missing_year = sum(is.na(primary_completion_year)),
    missing_sample_size = sum(is.na(sample_size)),
    .groups = "drop"
  )

write_csv(
  all_trials_df,
  "data/processed/all_eligible_trials_denominator.csv"
)

write_csv(
  denominator_check,
  "results/denominator_check.csv"
)

try(DBI::dbDisconnect(con), silent = TRUE)
