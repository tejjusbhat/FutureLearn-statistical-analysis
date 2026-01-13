############################################################
# 02_cycle1_features.R
#
# Purpose (Cycle 1 Data Preparation):
#   - Agregate step activity per learner per run for
#     runs 6 and 7.
#   - Get zero activity learners from enrolment data.
#
# Output:
#   - cycle1_engagement_features: per (learner_id, run_id) engagement table
#   - cycle1_analysis_table: enrolments and engagement
#   - qc_cycle1_feature_summary: simple QC to support reporting
#   - qc_cycle1_by_run: breakdown of key metrics by run
############################################################

# Per learner engagement features from step activity

cycle1_engagement_features <- step_activity_clean |>
  dplyr::group_by(learner_id, run_id) |>
  dplyr::summarise(
    # Step-level engagement
    steps_visited = sum(visited, na.rm = TRUE),
    steps_completed = sum(completed_step, na.rm = TRUE),

    # Activity across the weeks
    weeks_active = dplyr::n_distinct(week_number[visited], na.rm = TRUE),
    active_days = dplyr::n_distinct(
      as.Date(first_visited_at[visited]), na.rm = TRUE
    ),

    # Timing metrics
    first_visit_at = suppressWarnings(min(first_visited_at, na.rm = TRUE)),
    last_visit_at  = suppressWarnings(max(first_visited_at, na.rm = TRUE)),
    last_completed_at = suppressWarnings(max(last_completed_at, na.rm = TRUE)),

    .groups = "drop"
  ) |>
  # If a learner has no valid timestamps, min/max return Inf/-Inf;
  dplyr::mutate(
    first_visit_at = dplyr::if_else(
      is.infinite(first_visit_at), as.POSIXct(NA), first_visit_at
    ),
    last_visit_at  = dplyr::if_else(
      is.infinite(last_visit_at),  as.POSIXct(NA), last_visit_at
    ),
    last_completed_at = dplyr::if_else(
      is.infinite(last_completed_at), as.POSIXct(NA), last_completed_at
    ),

    # Completion ratio based on visited steps
    completion_ratio = dplyr::if_else(
      steps_visited > 0,
      steps_completed / steps_visited,
      0
    )
  )

  # Get zero activity learners from enrolment data
  cycle1_analysis_table <- enrolments_clean |>
  dplyr::select(
    learner_id, run_id,
    completed, fully_participated_at,
    enrolled_at, unenrolled_at,
    purchased_statement_at,
    gender, country, detected_country,
    age_range, highest_education_level,
    employment_status, employment_area
  ) |>
  dplyr::left_join(
    cycle1_engagement_features,
    by = c("learner_id", "run_id")
  ) |>
  # Left join to retain zero activity learners
  dplyr::mutate(
    steps_visited   = dplyr::coalesce(steps_visited, 0L),
    steps_completed = dplyr::coalesce(steps_completed, 0L),
    weeks_active    = dplyr::coalesce(weeks_active, 0L),
    active_days     = dplyr::coalesce(active_days, 0L),
    completion_ratio = dplyr::coalesce(completion_ratio, 0),
    any_step_activity = steps_visited > 0
  )

# QC summaries
qc_cycle1_feature_summary <- cycle1_analysis_table |>
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_learners = dplyr::n_distinct(learner_id),
    n_runs = dplyr::n_distinct(run_id),

    completion_rate = mean(completed, na.rm = TRUE),

    share_any_activity = mean(any_step_activity, na.rm = TRUE),
    share_zero_activity = mean(!any_step_activity, na.rm = TRUE),

    median_steps_visited = stats::median(steps_visited, na.rm = TRUE),
    median_steps_completed = stats::median(steps_completed, na.rm = TRUE),
    median_weeks_active = stats::median(weeks_active, na.rm = TRUE),
    median_active_days = stats::median(active_days, na.rm = TRUE)
  )

# Breakdown by run
qc_cycle1_by_run <- cycle1_analysis_table |>
  dplyr::group_by(run_id) |>
  dplyr::summarise(
    n_learners = dplyr::n_distinct(learner_id),
    completion_rate = mean(completed, na.rm = TRUE),
    share_zero_activity = mean(!any_step_activity, na.rm = TRUE),
    median_steps_visited = stats::median(steps_visited, na.rm = TRUE),
    median_steps_completed = stats::median(steps_completed, na.rm = TRUE),
    .groups = "drop"
  )