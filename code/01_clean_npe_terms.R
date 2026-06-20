# 01_clean_npe_terms.R
# Purpose: Clean and harmonise neuropsychiatric adverse event terms
# Input: data/input/definitive_neuro_terms.csv and AACT database tables
# Output: data/processed/clean_npe_event_records.csv and data/processed/npe_trial_summary.csv
# Data source: AACT / ClinicalTrials.gov adverse event data
# Note: This script identifies registry-reported NPE terms.
# It does not estimate true incidence or causal effects.

library(DBI)
library(dplyr)
library(readr)
library(RPostgres)
library(stringr)

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

npe_terms <- read_csv(
  "data/input/definitive_neuro_terms.csv",
  show_col_types = FALSE
)

if (!"adverse_event_term" %in% names(npe_terms)) {
  stop("data/input/definitive_neuro_terms.csv must contain adverse_event_term")
}

npe_terms_clean <- npe_terms %>%
  transmute(
    term_lower = str_squish(str_to_lower(as.character(adverse_event_term)))
  ) %>%
  filter(!is.na(term_lower), term_lower != "") %>%
  distinct()

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("AACT_DBNAME"),
  host = Sys.getenv("AACT_HOST"),
  port = as.integer(Sys.getenv("AACT_PORT")),
  user = Sys.getenv("AACT_USER"),
  password = Sys.getenv("AACT_PASSWORD")
)

raw_npe_events <- tryCatch(
  {
    DBI::dbWriteTable(
      con,
      "tmp_npe_terms",
      npe_terms_clean,
      temporary = TRUE,
      overwrite = TRUE
    )

    DBI::dbGetQuery(con, "
    WITH lead_sponsor AS (
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
      re.event_type,
      re.adverse_event_term,
      re.subjects_affected,
      re.subjects_at_risk,
      re.vocab,
      re.organ_system
    FROM studies s
    JOIN reported_events re
      ON s.nct_id = re.nct_id
    JOIN tmp_npe_terms t
      ON TRIM(LOWER(re.adverse_event_term)) = t.term_lower
    LEFT JOIN lead_sponsor ls
      ON s.nct_id = ls.nct_id
    WHERE LOWER(s.study_type) = 'interventional';
    ")
  },
  finally = {
    try(DBI::dbDisconnect(con), silent = TRUE)
  }
)

df_clean2 <- raw_npe_events %>%
  mutate(
    nct_id = str_squish(as.character(nct_id)),
    adverse_event_term = str_squish(as.character(adverse_event_term)),
    term_lower = str_squish(str_to_lower(adverse_event_term)),
    organ_system_lower = str_squish(str_to_lower(as.character(organ_system))),
    subjects_affected = suppressWarnings(as.numeric(subjects_affected)),
    subjects_at_risk = suppressWarnings(as.numeric(subjects_at_risk))
  ) %>%
  filter(
    !is.na(nct_id),
    nct_id != "",
    !is.na(term_lower),
    term_lower != ""
  ) %>%
  filter(
    !str_detect(
      term_lower,
      "headache|fatigue|dizziness|nausea|somnolence|pain"
    )
  ) %>%
  filter(
    is.na(organ_system_lower) |
      !str_detect(organ_system_lower, "general disorders")
  ) %>%
  select(-organ_system_lower)

npe_trial_summary <- df_clean2 %>%
  group_by(nct_id) %>%
  summarise(
    npe_any = 1L,
    npe_record_count = n(),
    npe_term_count = n_distinct(term_lower),
    .groups = "drop"
  )

npe_cleaning_summary <- tibble(
  definitive_terms = nrow(npe_terms_clean),
  raw_records = nrow(raw_npe_events),
  cleaned_records = nrow(df_clean2),
  npe_trials = n_distinct(df_clean2$nct_id),
  npe_terms_in_cleaned_data = n_distinct(df_clean2$term_lower)
)

write_csv(npe_terms_clean, "data/processed/npe_terms_clean.csv")
write_csv(raw_npe_events, "data/processed/raw_npe_event_records.csv")
write_csv(df_clean2, "data/processed/clean_npe_event_records.csv")
write_csv(npe_trial_summary, "data/processed/npe_trial_summary.csv")
write_csv(npe_cleaning_summary, "results/npe_cleaning_summary.csv")
