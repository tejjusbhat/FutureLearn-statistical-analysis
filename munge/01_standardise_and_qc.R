############################################################
# 01_standardise_and_qc.R
#
# Purpose:
#   - Standardise enrolment and step-activity data 
#     for runs 6 and 7
#   - Perform basic data quality checks
#   - Create clean tables for later analysis
############################################################

parse_dt <- function(x) {
  x <- dplyr::na_if(x, "")
  x <- dplyr::na_if(x, "NULL")
  x <- dplyr::na_if(x, "NA")

  # Remove trailing " UTC"
  x <- stringr::str_replace(x, "\\s+UTC$", "")
  out <- readr::parse_datetime(x, locale = readr::locale(tz = "UTC"))

  # Fallback for specific format
  needs_fallback <- is.na(out) & !is.na(x)
  if (any(needs_fallback)) {
    out2 <- readr::parse_datetime(
      x[needs_fallback],
      format = "%Y-%m-%d %H:%M:%S",
      locale = readr::locale(tz = "UTC")
    )
    out[needs_fallback] <- out2
  }

  out
}

# Enrolment run 6

enrolments_6_clean <- cyber.security.6_enrolments |>
  janitor::clean_names() |>
  dplyr::mutate(
    run_id = 6L,
    enrolled_at = parse_dt(enrolled_at),
    unenrolled_at = parse_dt(unenrolled_at),
    fully_participated_at = parse_dt(fully_participated_at),
    purchased_statement_at = parse_dt(purchased_statement_at)
  ) |>
  dplyr::filter(role == "learner") |>
  dplyr::mutate(
    # Outcome label for later analysis
    completed = !is.na(fully_participated_at)
  )

# Enrolment run 7

enrolments_7_clean <- cyber.security.7_enrolments |>
  janitor::clean_names() |>
  dplyr::mutate(
    run_id = 7L,
    enrolled_at = parse_dt(enrolled_at),
    unenrolled_at = parse_dt(unenrolled_at),
    fully_participated_at = parse_dt(fully_participated_at),
    purchased_statement_at = parse_dt(purchased_statement_at)
  ) |>
  dplyr::filter(role == "learner") |>
  dplyr::mutate(
    completed = !is.na(fully_participated_at)
  )

# Combine enrolment data

enrolments_clean <- dplyr::bind_rows(
  enrolments_6_clean,
  enrolments_7_clean
)

# Data quality checks for enrolment data

qc_enrolments <- enrolments_clean |>
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_learners = dplyr::n_distinct(learner_id),
    n_runs = dplyr::n_distinct(run_id),
    missing_learner_ids = sum(is.na(learner_id)),
    missing_enrolled_at = sum(is.na(enrolled_at)),
    completion_rate = mean(completed, na.rm = TRUE)
  )

qc_enrolments_duplicates <- enrolments_clean |>
  dplyr::count(learner_id, run_id, name = "n") |>
  dplyr::filter(n > 1) |>
  dplyr::arrange(desc(n))

# Step-activity run 6

step_activity_6_clean <- cyber.security.6_step.activity |>
  janitor::clean_names() |>
  dplyr::mutate(
    run_id = 6L,
    first_visited_at = parse_dt(first_visited_at),
    last_completed_at = parse_dt(last_completed_at),
    visited = !is.na(first_visited_at),
    completed_step = !is.na(last_completed_at)
  )
# Step-activity run 7

step_activity_7_clean <- cyber.security.7_step.activity |>
  janitor::clean_names() |>
  dplyr::mutate(
    run_id = 7L,
    first_visited_at = parse_dt(first_visited_at),
    last_completed_at = parse_dt(last_completed_at),
    visited = !is.na(first_visited_at),
    completed_step = !is.na(last_completed_at)
  )

# Combine step-activity data

step_activity_clean <- dplyr::bind_rows(
  step_activity_6_clean,
  step_activity_7_clean
)

# Data quality checks for step-activity data

qc_step_activity_overview <- step_activity_clean |>
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_learners = dplyr::n_distinct(learner_id),
    missing_learner_id = sum(is.na(learner_id)),
    missing_step = sum(is.na(step)),
    missing_week_number = sum(is.na(week_number)),
    missing_step_number = sum(is.na(step_number)),
    missing_first_visited_at = sum(is.na(first_visited_at)),
    missing_last_completed_at = sum(is.na(last_completed_at)),
    share_steps_completed = mean(completed_step, na.rm = TRUE)
  )

qc_step_activity_unmatched <- step_activity_clean |>
  dplyr::anti_join(
    enrolments_clean |> dplyr::select(learner_id, run_id),
    by = c("learner_id", "run_id")
  )

qc_step_activity_unmatched_overview <- qc_step_activity_unmatched |>
  dplyr::summarise(
    n_unmatched_rows = dplyr::n(),
    n_unmatched_learners = dplyr::n_distinct(learner_id)
  )
  