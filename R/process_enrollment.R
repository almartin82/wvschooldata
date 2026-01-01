# ==============================================================================
# Enrollment Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw enrollment data from WVDE
# PDFs into a clean, standardized format.
#
# West Virginia data is at the county-district level only (no school-level
# data in the PDFs). The state has 55 county school districts.
#
# ==============================================================================

#' Process raw enrollment data
#'
#' Transforms raw WVDE PDF data into a standardized schema.
#'
#' @param raw_data List containing FTE and headcount data from get_raw_enr
#' @param end_year School year end
#' @return Processed data frame with standardized columns
#' @keywords internal
process_enr <- function(raw_data, end_year) {

  # Process district (county) data
  district_processed <- process_district_enr(raw_data$fte, raw_data$headcount, end_year)

  # Create state aggregate from district data
  state_processed <- create_state_aggregate(district_processed, end_year)

  # Combine all levels
  result <- dplyr::bind_rows(state_processed, district_processed)

  # Ensure consistent column order
  col_order <- c(
    "end_year", "type",
    "district_id", "campus_id",
    "district_name", "campus_name",
    "county", "charter_flag",
    "row_total",
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )

  # Only include columns that exist
  col_order <- col_order[col_order %in% names(result)]
  result <- result[, col_order]

  result
}


#' Process district-level enrollment data
#'
#' Combines FTE grade data with headcount totals.
#'
#' @param fte_data Data frame with FTE enrollment by grade
#' @param headcount_data Data frame with headcount enrollment
#' @param end_year School year end
#' @return Processed district data frame
#' @keywords internal
process_district_enr <- function(fte_data, headcount_data, end_year) {

  # Check if we have FTE data
  if (is.null(fte_data) || nrow(fte_data) == 0) {
    warning("No FTE enrollment data available for year ", end_year)
    return(data.frame())
  }

  # Start with FTE data (has grade breakdown)
  result <- fte_data

  # Merge headcount data if available
  if (!is.null(headcount_data) && nrow(headcount_data) > 0) {
    result <- dplyr::left_join(
      result,
      headcount_data,
      by = "county_name"
    )

    # Use headcount as row_total if it's more reliable (integer vs FTE decimal)
    if ("headcount" %in% names(result)) {
      # Only use headcount if available, otherwise keep FTE-based total
      result$row_total <- dplyr::coalesce(result$headcount, result$row_total)
      result$headcount <- NULL
    }
  }

  # Standardize column names and add required fields
  result <- result |>
    dplyr::mutate(
      end_year = end_year,
      type = "District",
      district_id = sapply(county_name, generate_district_id),
      campus_id = NA_character_,
      district_name = sapply(county_name, format_district_name),
      campus_name = NA_character_,
      county = county_name,
      charter_flag = NA_character_
    )

  # Calculate row_total if not already present
  if (!"row_total" %in% names(result) || all(is.na(result$row_total))) {
    grade_cols <- c("grade_pk", "grade_k", paste0("grade_", sprintf("%02d", 1:12)))
    grade_cols <- grade_cols[grade_cols %in% names(result)]
    result$row_total <- rowSums(result[, grade_cols, drop = FALSE], na.rm = TRUE)
  }

  # Select and order columns
  result <- result |>
    dplyr::select(
      end_year, type,
      district_id, campus_id,
      district_name, campus_name,
      county, charter_flag,
      row_total,
      dplyr::any_of(c(
        "grade_pk", "grade_k",
        "grade_01", "grade_02", "grade_03", "grade_04",
        "grade_05", "grade_06", "grade_07", "grade_08",
        "grade_09", "grade_10", "grade_11", "grade_12"
      ))
    )

  result
}


#' Create state-level aggregate from district data
#'
#' @param district_df Processed district data frame
#' @param end_year School year end
#' @return Single-row data frame with state totals
#' @keywords internal
create_state_aggregate <- function(district_df, end_year) {

  if (nrow(district_df) == 0) {
    return(data.frame())
  }

  # Columns to sum
  sum_cols <- c(
    "row_total",
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )

  # Filter to columns that exist
  sum_cols <- sum_cols[sum_cols %in% names(district_df)]

  # Create state row
  state_row <- data.frame(
    end_year = end_year,
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = NA_character_,
    campus_name = NA_character_,
    county = NA_character_,
    charter_flag = NA_character_,
    stringsAsFactors = FALSE
  )

  # Sum each column
  for (col in sum_cols) {
    state_row[[col]] <- sum(district_df[[col]], na.rm = TRUE)
  }

  state_row
}
