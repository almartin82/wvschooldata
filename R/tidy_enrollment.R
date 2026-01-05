# ==============================================================================
# Enrollment Data Tidying Functions
# ==============================================================================
#
# This file contains functions for transforming enrollment data from wide
# format to long (tidy) format and identifying aggregation levels.
#
# Note: WVDE PDF data only provides grade-level enrollment, not demographic
# breakdowns. Demographic data would need to come from ZoomWV or other WVDE sources.
#
# ==============================================================================

#' Tidy enrollment data
#'
#' Transforms wide enrollment data to long format with subgroup column.
#'
#' @param df A wide data.frame of processed enrollment data
#' @return A long data.frame of tidied enrollment data
#' @export
#' @examples
#' \dontrun{
#' wide_data <- fetch_enr(2024, tidy = FALSE)
#' tidy_data <- tidy_enr(wide_data)
#' }
tidy_enr <- function(df) {

  # Invariant columns (identifiers that stay the same)
  invariants <- c(
    "end_year", "type",
    "district_id", "campus_id",
    "district_name", "campus_name",
    "county", "charter_flag"
  )
  invariants <- invariants[invariants %in% names(df)]

  # Grade-level columns
  grade_cols <- grep("^grade_", names(df), value = TRUE)

  # Extract total enrollment as a "subgroup"
  if ("row_total" %in% names(df)) {
    tidy_total <- df |>
      dplyr::select(dplyr::all_of(c(invariants, "row_total"))) |>
      dplyr::mutate(
        n_students = row_total,
        subgroup = "total_enrollment",
        pct = 1.0,
        grade_level = "TOTAL"
      ) |>
      dplyr::select(dplyr::all_of(c(invariants, "grade_level", "subgroup", "n_students", "pct")))
  } else {
    tidy_total <- NULL
  }

  # Transform grade-level enrollment to long format
  if (length(grade_cols) > 0) {
    grade_level_map <- c(
      "grade_pk" = "PK",
      "grade_k" = "K",
      "grade_01" = "01",
      "grade_02" = "02",
      "grade_03" = "03",
      "grade_04" = "04",
      "grade_05" = "05",
      "grade_06" = "06",
      "grade_07" = "07",
      "grade_08" = "08",
      "grade_09" = "09",
      "grade_10" = "10",
      "grade_11" = "11",
      "grade_12" = "12",
      "grade_ug" = "UG"
    )

    tidy_grades <- purrr::map_df(
      grade_cols,
      function(.x) {
        gl <- grade_level_map[.x]
        if (is.na(gl)) gl <- .x

        df |>
          dplyr::rename(n_students = dplyr::all_of(.x)) |>
          dplyr::select(dplyr::all_of(c(invariants, "n_students", "row_total"))) |>
          dplyr::mutate(
            subgroup = "total_enrollment",
            pct = n_students / row_total,
            grade_level = gl
          ) |>
          dplyr::select(dplyr::all_of(c(invariants, "grade_level", "subgroup", "n_students", "pct")))
      }
    )
  } else {
    tidy_grades <- NULL
  }

  # Combine all tidy data
  result <- dplyr::bind_rows(tidy_total, tidy_grades)

  # Handle case where no data was processed
  if (nrow(result) == 0 || !"n_students" %in% names(result)) {
    return(dplyr::tibble(
      end_year = integer(),
      type = character(),
      district_id = character(),
      campus_id = character(),
      district_name = character(),
      campus_name = character(),
      county = character(),
      charter_flag = character(),
      grade_level = character(),
      subgroup = character(),
      n_students = numeric(),
      pct = numeric()
    ))
  }

  result |>
    dplyr::filter(!is.na(n_students)) |>
    dplyr::mutate(
      aggregation_flag = dplyr::case_when(
        !is.na(district_id) & !is.na(campus_id) & district_id != "" & campus_id != "" ~ "campus",
        !is.na(district_id) & district_id != "" ~ "district",
        TRUE ~ "state"
      )
    )
}


#' Identify enrollment aggregation levels
#'
#' Adds boolean flags to identify state, district, and campus level records.
#'
#' @param df Enrollment dataframe, output of tidy_enr
#' @return data.frame with boolean aggregation flags
#' @export
#' @examples
#' \dontrun{
#' tidy_data <- fetch_enr(2024)
#' # Data already has aggregation flags via id_enr_aggs
#' table(tidy_data$is_state, tidy_data$is_district)
#' }
id_enr_aggs <- function(df) {
  df |>
    dplyr::mutate(
      # State level: Type == "State"
      is_state = type == "State",

      # District level: Type == "District"
      is_district = type == "District",

      # Campus level: Type == "Campus" (not available in WVDE PDF data)
      is_campus = type == "Campus",

      # Charter detection - based on charter_flag column
      is_charter = !is.na(charter_flag) & charter_flag == "Y"
    )
}


#' Custom Enrollment Grade Level Aggregates
#'
#' Creates aggregations for common grade groupings: K-8, 9-12 (HS), K-12.
#'
#' @param df A tidy enrollment df
#' @return df of aggregated enrollment data
#' @export
#' @examples
#' \dontrun{
#' tidy_data <- fetch_enr(2024)
#' grade_aggs <- enr_grade_aggs(tidy_data)
#' }
enr_grade_aggs <- function(df) {

  # Group by invariants (everything except grade_level and counts)
  group_vars <- c(
    "end_year", "type",
    "district_id", "campus_id",
    "district_name", "campus_name",
    "county", "charter_flag",
    "subgroup",
    "is_state", "is_district", "is_campus", "is_charter"
  )
  group_vars <- group_vars[group_vars %in% names(df)]

  # K-8 aggregate
  k8_agg <- df |>
    dplyr::filter(
      subgroup == "total_enrollment",
      grade_level %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08")
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarize(
      n_students = sum(n_students, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      grade_level = "K8",
      pct = NA_real_
    )

  # High school (9-12) aggregate
  hs_agg <- df |>
    dplyr::filter(
      subgroup == "total_enrollment",
      grade_level %in% c("09", "10", "11", "12")
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarize(
      n_students = sum(n_students, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      grade_level = "HS",
      pct = NA_real_
    )

  # K-12 aggregate (excludes PK)
  k12_agg <- df |>
    dplyr::filter(
      subgroup == "total_enrollment",
      grade_level %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08",
                         "09", "10", "11", "12")
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarize(
      n_students = sum(n_students, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      grade_level = "K12",
      pct = NA_real_
    )

  dplyr::bind_rows(k8_agg, hs_agg, k12_agg)
}
