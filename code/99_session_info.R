# 99_session_info.R
# Purpose: Record R and package versions used for the analysis
# Input: Current R session
# Output: results/session_info.txt

dir.create("results", recursive = TRUE, showWarnings = FALSE)

writeLines(
  capture.output(sessionInfo()),
  "results/session_info.txt"
)
