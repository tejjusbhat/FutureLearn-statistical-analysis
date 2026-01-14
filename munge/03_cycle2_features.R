############################################################
# 03_cycle2_features.R
#
# Purpose:
# - Investigate engagement features using step acivity
# 
# Outputs (cached by ProjectTemplate):
#   - cycle2_early_engagement_features
#   - cycle2_analysis_table
#   - qc_cycle2_feature_summary
############################################################

# Build early engagement features
step_activity_early <- step_activity_clean |>
  dplyr::filter(week_number <= 2)

early_engagement_features <- step_activity_early |>
  dplyr::group_by(learner_id, run_id) |>
  dplyr::summarise(
    steps_visited = sum(visited, na.rm = TRUE),
    steps_completed = sum(completed_step, na.rm = TRUE),
    weeks_active = dplyr::n_distinct(week_number[visited], na.rm = TRUE),
    active_days = dplyr::n_distinct(
      as.Date(first_visited_at[visited]), na.rm = TRUE
    ),
    first_visit_at = suppressWarnings(min(first_visited_at, na.rm = TRUE)),
    last_visit_at  = suppressWarnings(max(first_visited_at, na.rm = TRUE)),

    .groups = "drop"
  ) |>
  dplyr::mutate(
    first_visit_at = dplyr::if_else(
      is.infinite(first_visit_at), as.POSIXct(NA), first_visit_at
),
    last_visit_at  = dplyr::if_else(
      is.infinite(last_visit_at),  as.POSIXct(NA), last_visit_at
    ),

    completion_ratio = dplyr::if_else(
      steps_visited > 0,
      steps_completed / steps_visited,
      0
    )
  )

# Store early engagement features
analysis_table <- enrolments_clean |>
  dplyr::select(
    learner_id, run_id,
    completed, fully_participated_at,
    enrolled_at,
    gender, country, detected_country,
    age_range, highest_education_level,
    employment_status, employment_area
  ) |>
  dplyr::left_join(
    early_engagement_features,
    by = c("learner_id", "run_id")
  ) |>
  dplyr::mutate(
    steps_visited   = dplyr::coalesce(steps_visited, 0L),
    steps_completed = dplyr::coalesce(steps_completed, 0L),
    weeks_active    = dplyr::coalesce(weeks_active, 0L),
    active_days     = dplyr::coalesce(active_days, 0L),
    completion_ratio = dplyr::coalesce(completion_ratio, 0),

    any_early_activity = steps_visited > 0
  )

# Feature summary for quality control
qc_cycle2_feature_summary <- analysis_table |>
  dplyr::summarise(
    n_rows = dplyr::n(),
    completion_rate = mean(completed, na.rm = TRUE),

    share_any_early_activity = mean(any_early_activity, na.rm = TRUE),
    share_zero_early_activity = mean(!any_early_activity, na.rm = TRUE),

    median_steps_visited = stats::median(steps_visited, na.rm = TRUE),
    median_steps_completed = stats::median(steps_completed, na.rm = TRUE),
    median_weeks_active = stats::median(weeks_active, na.rm = TRUE),
    median_active_days = stats::median(active_days, na.rm = TRUE)
  )